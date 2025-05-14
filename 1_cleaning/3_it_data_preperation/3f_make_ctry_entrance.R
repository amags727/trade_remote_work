# import data / determine sample -------------------------------------------------------------
## base data 
bs_br = import_file('1) data/3_bs_br_data.csv',char_vars =  c('firmid'), 
        col_select =  c('firmid', 'year', 'dom_turnover', 'empl', 'NACE_BR')) %>% 
        distinct(firmid,year, .keep_all = T)

firm_yr_vars =  c('firmid', 'year', 'total_export_rev_customs','num_export_countries',
                  gpaste('log_', c('empl', 'capital')),
                 gpaste(c('comp_data', 'share_comp_data'),"_nace_", gpaste(c('pct_rank', 'sd_from_mean'), c('', "_age"))))
firm_yr_lvl = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'), col_select = firm_yr_vars) %>%
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T) 

indiv_mkt_rev = import_file('1) data/9_customs_cleaned.csv',
    col_select =  c('firmid', 'ctry', 'year', 'exim', 'deflated_value'), char_vars = 'firmid') %>% 
  .[exim == 2 & ctry != 'FR' & year %in% year_range] %>% select(-exim)

overall_mkt_popularity = indiv_mkt_rev[,.(count =  .N), by = ctry] %>% arrange(-count) %>% 
  .[,mkt_all_time_popularity_rank := 1:nrow(.)] %>% select(-count)

### birth data 
overall_birth_data = import_file(file.path(inputs_dir, '16a_firm_lvl_birth_data.parquet'),col_select = c('first_export_year','birth_year', 'firmid'))
 
customs_birth_data = import_file(file.path(inputs_dir, '16b_firm_ctry_lvl_birth_data.parquet'),
                      col_select = c('firmid','ctry', 'country_entrance_year', 'exim')) %>% 
                      .[exim == 2 & country_entrance_year %in% year_range & ctry != 'FR'] %>% 
                      select(-exim) %>% unique()

customs_first_obs = import_file('1) data/9_customs_cleaned.csv', char_vars =  c('firmid'),
                    col_select = c('year', 'firmid','exim','ctry')) %>% 
                    .[exim == 2] %>% select(-exim) %>% unique() %>%
                    .[, .(first_obs_date =  min(year)), by = .(firmid,ctry)]

### linkedin data 
linkedin_ctry_lvl = import_file(linkedin_ctry_lvl_path) 
linkedin_firm_vars =  c('share_comp_data','comp_french', 'comp_total', 'comp_data', 'comp_rnd', 'firmid', 'year',  'comp_weighted_prestige')
linkedin_firm_lvl = import_file(linkedin_basic_path, col_select = linkedin_firm_vars) %>% 
  .[,first_linkedin_obs_yr := min(year), by = firmid] %>% 
  unbalanced_lag(., id_var = 'firmid', time_var = 'year', value_vars = con_fil(names(.),'data','rnd'),1)

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

## other params 
vars_to_log = c('num_other_export_markets','distance_to_france', 'dom_turnover', 'other_export_market_rev',
                gpaste(c('comp'), '_', c('ever', 'l5', 'now','data', 'rnd')),
                gpaste(c('mkt_', 'nace_mkt_'), c('num_exporters','size_rev')))


### DETERMINE SAMPLE 
linkedin_matched_firms = import_file(linkedin_match_path)[['firmid']]
firm_yr_lvl_firms = firm_yr_lvl %>% distinct(firmid) %>% pull(firmid)
firms_in_sample =  intersect(linkedin_matched_firms, firm_yr_lvl_firms) %>% intersect(.,unique(customs_birth_data$firmid))
countries_in_sample = unique(customs_birth_data$ctry)


# construct piecewise output---------------------------------------------------------
dir.create('1) data/temp_data')
if (dummy_version){num_breaks = 2}else{num_breaks = 1000}
chunk_list <- split(firms_in_sample, cut(seq_along(firms_in_sample), breaks = num_breaks, labels = FALSE))
lapply(1:length(chunk_list), function(chunk_num){
  if (!file.exists(paste0('1) data/temp_data/data',chunk_num,'.csv'))){
  firm_chunk = chunk_list[[chunk_num]]
  expanded_customs = expand(firm_chunk, countries_in_sample, year_range,names = c('firmid', 'ctry', 'year')) %>%
  
    ## remove all instances where we've observed the firm in that country before 
    merge(customs_first_obs[firmid %in% firm_chunk], all.x =T) %>%
    .[year <= first_obs_date | is.na(first_obs_date)] %>% select(-first_obs_date) %>% 
    
    ## repeat (just for safety) with confirmed market entries 
    merge(customs_birth_data[firmid %in% firm_chunk], all.x = T, by = c('firmid','ctry')) %>% 
    .[year <= country_entrance_year | is.na(country_entrance_year) ] %>% 
    .[, entered_market := country_entrance_year == year] %>%
    
    ### merge in overall age data and eliminate firms where we don't observe birth / observations before birth 
    merge(overall_birth_data[firmid %in% firm_chunk], by = 'firmid') %>% 
    .[year >= birth_year] %>% 
    .[,age := year- birth_year] %>% 
    .[,not_yet_exported := year <= first_export_year] %>% 
    .[,young_at_start := NA_min(age)  <= 5, by = firmid] %>% 
    
    ## merge in the linkedin data 
    merge(linkedin_firm_lvl[firmid %in% firm_chunk], by = c('firmid','year'), all.x = T) %>% 
    merge(linkedin_ctry_lvl[firmid %in% firm_chunk], by =  c('firmid','ctry','year'), all.x =T) %>% 
    
      ## generate zeros for all countries where firm wasn't active and then generate lagged versions of variables 
      .[year >= first_linkedin_obs_yr,(con_fil(names(linkedin_ctry_lvl),'empl','comp')) := lapply(.SD, function(x) replace_na(x,0)), 
        .SDcols = con_fil(names(linkedin_ctry_lvl),'empl','comp')] %>% 
      unbalanced_lag(., id_var = c('firmid','ctry'), time_var = 'year', value_vars = con_fil(names(linkedin_ctry_lvl),'empl','comp'), 1) %>% 
    
    ## merge in the firm-level data 
    merge(bs_br[firmid %in% firm_chunk], all.x = T, by = c('firmid', 'year')) %>% 
    merge(firm_yr_lvl[firmid %in% firm_chunk], all.x = T, by = c('firmid', 'year')) %>% 
    merge(indiv_mkt_rev[firmid %in% firm_chunk], all.x = T, by= c('firmid', 'ctry', 'year')) %>% 
    
    ## clean up the num_other_markets / other_market_rev 
    .[, deflated_value := replace_na(deflated_value, 0)] %>%
    .[, num_other_export_markets := num_export_countries - entered_market] %>%
    .[, other_export_market_rev :=   total_export_rev_customs - deflated_value]  %>%
    .[, c("total_export_rev_customs", "num_export_countries", "deflated_value") := NULL] %>% 
  
    ## add in the market level data 
    merge(ctry_lvl_constants, all.x = T, by = c('ctry')) %>%
    merge(ctry_yr_lvl_constants, all.x = T, by = c('ctry', 'year')) %>%
    merge(nace_ctry_lvl_constants, all.x =T, by = c('ctry', 'NACE_BR')) %>%
    merge(nace_ctry_yr_lvl_constants, all.x = T, by = c('ctry', 'NACE_BR', 'year')) %>% 
    
    ## add in log data 
    .[,paste0('log_',vars_to_log) := lapply(.SD,asinh), .SDcols =vars_to_log] %>% 
  
    ## add in market popoularity 
    merge(overall_mkt_popularity, by = 'ctry', all.x = T)
  
  
  
  # output data -------------------------------------------------------------
  fwrite(expanded_customs, paste0('1) data/temp_data/data',chunk_num,'.csv'))
}
})

# clear ------------------------------
rm(list= setdiff(ls(), base_env)); gc()


