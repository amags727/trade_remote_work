
# import data -------------------------------------------------------------

## base data 
bs_br = c('firmid', 'year', 'dom_turnover', 'empl', 'NACE_BR') %>% 
  import_file('1) data/3_bs_br_data.csv',char_vars =  c('firmid'), col_select = .) %>% 
  distinct(firmid,year, .keep_all = T)

firm_yr_lvl =  c('firmid', 'year', 'total_export_rev_customs','num_export_countries',
  "quartile_comp_data", "quartile_share_comp_data", "empl") %>%
  import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'), col_select = .) %>%
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T) %>% rename_with(~c(gsub('quartile', 'nace_yr_quartile',.)))

indiv_mkt_rev = import_file('1) data/9_customs_cleaned.csv',
    col_select =  c('firmid', 'ctry', 'year', 'exim', 'deflated_value'), char_vars = 'firmid') %>% 
  .[exim == 2 & ctry != 'FR' & year %in% year_range] %>% select(-exim)



### birth data 
overall_birth_data = import_file(file.path(inputs_dir, '16a_firm_lvl_birth_data.parquet'),col_select = c('birth_year', 'firmid'))  
customs_birth_data = c('firmid','ctry', 'country_entrance_year', 'exim') %>% 
  import_file(file.path(inputs_dir, '16b_firm_ctry_lvl_birth_data.parquet'), col_select = .) %>% 
  .[exim == 2 & country_entrance_year %in% year_range & ctry != 'FR'] %>% select(-exim) %>% unique()
customs_first_obs = import_file('1) data/9_customs_cleaned.csv', char_vars =  c('firmid'), col_select = c('year', 'firmid','exim','ctry')) %>% 
  .[exim == 2] %>% select(-exim) %>% unique() %>% .[, .(first_obs_date =  min(year)), by = .(firmid,ctry)]


### linkedin data 
linkedin_ctry_lvl = import_file(linkedin_ctry_lvl_path) 
linkedin_firm_lvl = c('share_comp_data','comp_french', 'comp_total', 'comp_data', 'comp_rnd', 'firmid', 'year') %>% 
  import_file(linkedin_basic_path, col_select = .) %>% 
  .[,first_linkedin_obs_yr := min(year), by = firmid] %>% 
  unbalanced_lag(., id_var = 'firmid', time_var = 'year', 
                 value_vars = con_fil(names(.),'data','rnd'),1)
linkedin_matched_firms = import_file(linkedin_match_path)[['firmid']]


## market lvl data 
base_ctry_lvl =  import_file(file.path(inputs_dir,'16d_firm_ctry_yr_lvl.parquet'))
ctry_lvl_constants =  base_ctry_lvl %>%
  select('ctry',con_fil(con_fil(names(.),"grav", 'variance', 'distance'),'nace', inc = F)) %>%
  unique()
nace_ctry_lvl_constants = base_ctry_lvl %>%
  select('ctry', 'NACE_BR', con_fil(names(.), 'nace', 'variance', or = F)) %>% unique()

ctry_yr_lvl_constants = base_ctry_lvl %>% 
  select('ctry', 'year',con_fil(con_fil(names(.), 'mkt'),'nace','variance', inc = F)) %>%
  unique()

nace_ctry_yr_lvl_constants = base_ctry_lvl %>%
  select( 'ctry','NACE_BR', 'year', con_fil(con_fil(names(base_ctry_lvl), 'nace_mkt'),'variance', inc = F)) %>%
  unique()
# construct data  ---------------------------------------------------------
firms_in_sample = if(dummy_version){
  unique(c(linkedin_matched_firms[1:300],
           firm_yr_lvl[['firmid']][1:300], 
           customs_birth_data[country_entrance_year > 2008][['firmid']][1:300]))
}else{
  intersect(
  linkedin_matched_firms,
  unique(firm_yr_lvl$firmid),
  overall_birth_data[!is.na(birth_year)][['firmid']]
  )} 


expanded_customs = expand(
  firms_in_sample,
  unique(customs_birth_data$ctry),
  year_range,
  names = c('firmid', 'ctry', 'year')) %>%
  
  ## remove all instances where we've observed the firm in that country before 
  merge(customs_first_obs, all.x =T) %>%
  .[year <= first_obs_date | is.na(first_obs_date)] %>% select(-first_obs_date) %>% 
  
  ## repeat (just for safety) with confirmed market entries 
  merge(customs_birth_data, all.x = T, by = c('firmid','ctry')) %>% 
  .[year <= country_entrance_year | is.na(country_entrance_year) ] %>% 
  .[, entered_market := country_entrance_year == year] %>%
  
  ### merge in overall age data and eliminate firms where we don't observe birth / observations before birth 
  merge(overall_birth_data, by = 'firmid') %>% 
  .[year >= birth_year] %>% 
  .[,age := year- birth_year] %>% 
  
  ## merge in the linkedin data 
  merge(linkedin_firm_lvl,by = c('firmid','year'), all.x = T) %>% 
  merge(linkedin_ctry_lvl, by =  c('firmid','ctry','year'), all.x =T) %>% 
  
    ## generate zeros for all countries where firm wasn't active and then generate lagged versions of variables 
    .[year >= first_linkedin_obs_yr,(con_fil(names(linkedin_ctry_lvl),'empl','comp')) := lapply(.SD, function(x) replace_na(x,0)), 
      .SDcols = con_fil(names(linkedin_ctry_lvl),'empl','comp')] %>% 
    unbalanced_lag(., id_var = c('firmid','ctry'), time_var = 'year', value_vars = con_fil(names(linkedin_ctry_lvl),'empl','comp'), 1) %>% 
  
  ## merge in the firm-level data 
  merge(bs_br, all.x = T, by = c('firmid', 'year')) %>% 
  merge(firm_yr_lvl, all.x = T, by = c('firmid', 'year')) %>% 
  merge(indiv_mkt_rev, all.x = T, by= c('firmid', 'ctry', 'year')) %>% 
  
  ## clean up the num_other_markets / other_market_rev 
  .[, deflated_value := replace_na(deflated_value, 0)] %>%
  .[, num_other_export_markets := num_export_countries - entered_market] %>%
  .[, other_export_market_rev :=   total_export_rev_customs - deflated_value]  %>%
  .[, c("total_export_rev_customs", "num_export_countries", "deflated_value") := NULL] %>% 

  ## add in the market level data 
  merge(ctry_lvl_constants, all.x = T, by = c('ctry')) %>%
  merge(ctry_yr_lvl_constants, all.x = T, by = c('ctry', 'year')) %>%
  merge(nace_ctry_lvl_constants, all.x =T, by = c('ctry', 'NACE_BR')) %>%
  merge(nace_ctry_yr_lvl_constants, all.x = T, by = c('ctry', 'NACE_BR', 'year'))

write_parquet(expanded_customs,file.path(inputs_dir, '16e_ctry_entrance.parquet'))

# clear ------------------------------
rm(list= setdiff(ls(), base_env)); gc()


