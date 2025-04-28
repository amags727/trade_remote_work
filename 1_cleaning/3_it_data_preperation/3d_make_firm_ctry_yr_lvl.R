# import_files  -----------------------------------------------------------
# set parameter values 
linkedin_ctry_vars = gpaste(c('empl', 'comp'),"_", c('now', 'l5', 'ever'))
linkedin_vars = c('firmid', 'year', gpaste( 'comp_', c('data', 'engineer', 'french', 'french_data', 'rnd', 'stem', 'weighted_prestige')))
linkedin_to_log = setdiff(linkedin_vars, c('firmid', 'year', 'weighted_prestige'))
base_data_to_log =  c('num_markets','distance_to_france','age', 'dom_turnover', 'export_rev_customs',
                      gpaste(c('comp'), '_', c('ever', 'l5', 'now')),
                      gpaste(c('mkt_', 'nace_mkt_'), c('num_exporters','size_rev')))


firm_yr_lvl =  c(
  'firmid', 'year', 'dom_turnover', 'NACE_BR',
  "quartile_comp_data", "quartile_share_comp_data", "empl") %>%
  import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'), col_select = .) %>%
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T) %>% rename_with(~c(gsub('quartile', 'nace_yr_quartile',.)))

customs_data = c('firmid', 'ctry','streak_id', 'exim', 'products', 'deflated_value',  'year',
                 gpaste('export_',c('region', 'language', 'border'))) %>%
  import_file('1) data/9_customs_cleaned.csv', col_select = ., char_vars = 'firmid') %>%
  .[year %in% year_range & ctry != 'FR'] %>% distinct(firmid,year,ctry,exim, streak_id, .keep_all = T)


linkedin_ctry_lvl = import_file(linkedin_ctry_lvl_path)
linkedin_firm_lvl = import_file(linkedin_basic_path, col_select = linkedin_vars) %>% 
  .[,paste0('log_',linkedin_to_log) := lapply(.SD,asinh), .SDcols = linkedin_to_log] 

linkedin_matched_firms = import_file(linkedin_match_path)[['firmid']]
customs_birth_data = import_file(file.path(inputs_dir, '16b_firm_ctry_lvl_birth_data.parquet'))


french_distances = fread('1) data/similarity_matrices/outputs/france_distance_data.csv') 
# merge and clean  -----------------------------------------------------------
output = customs_data %>% distinct(firmid,ctry,year,exim, .keep_all = T) %>% #only necessary for the dummy versions (hopefully)
  rename_with(~gsub('export_','grav_',. )) %>% rename('export_rev_customs' = 'deflated_value') %>% 
  remove_if_NA('export_rev_customs') %>% 
  
  ## focus on exports 
  .[exim == 2] %>% .[,exim := NULL] %>% 
  
  # add num markets
  .[, num_markets := .N, by = .(firmid, year)] %>% 
  
  ## merge in age data and generate age variables 
  merge(customs_birth_data[exim == 2][,exim := NULL], by =c('firmid', 'ctry', 'streak_id')) %>%
  .[, `:=`(age = year - birth_year,
           young = year - birth_year <= 5,
           streak_age = year - streak_start,
           first_time_exporting = year == first_export_year,
           first_time_in_ctry = year == country_entrance_year,
           first_year_of_streak = year == streak_start,
           last_year_of_streak = year == streak_end)] %>%

  ## merge in linkedin data 
  merge(linkedin_ctry_lvl, all.x = T, by = c('firmid', 'ctry', 'year')) %>%
  .[firmid %in% linkedin_matched_firms, (linkedin_ctry_vars) := lapply(.SD, function(x) replace_na(x, 0)), .SDcols = linkedin_ctry_vars] %>%
  merge(linkedin_firm_lvl, all.x = T, by = c('firmid', 'year')) %>% 
  
  ## merge in firm-yr lvl 
  merge(firm_yr_lvl, by = c('firmid', 'year'), all.x = T) %>% 
  
  ## add market level variables 
  merge(french_distances, all.x = T, by = 'ctry') %>%
    .[, num_exporters := uniqueN(firmid), by = year] %>% 
    
    # industry 
    .[, nace_num_exporters := uniqueN(firmid), by = .(NACE_BR, year)] %>% .[is.na(NACE_BR), nace_num_exporters := NA] %>% 
  
    # ctry 
    .[, `:=` (mkt_num_exporters = uniqueN(firmid),
              mkt_size_rev = NA_sum(export_rev_customs),
              mkt_failure_rate = NA_sum(year == streak_start & year == streak_end) / NA_sum(!is.na(streak_start + streak_end)))
      , by = .(ctry, year)] %>% 
    variance_metrics(time_id = 'year', group_id = 'ctry', ind_id = 'streak_id',
                   int_id = 'export_rev_customs',  birth_id = 'streak_start', logged_version = T, prefix = 'mkt_',
                   full_dataset = T) %>% 
  
    # NACE / ctry 
    .[, `:=`(nace_mkt_failure_rate = NA_sum(year == streak_start & year == streak_end) / NA_sum(!is.na(streak_start + streak_end)),
             nace_mkt_num_exporters = uniqueN(firmid),
             nace_mkt_size_rev = NA_sum(export_rev_customs),
             nace_mkt = paste0(NACE_BR, ctry)), by = .(NACE_BR, year, ctry)] %>% 
    variance_metrics(time_id = 'year', group_id = 'nace_mkt', ind_id = 'streak_id',
                   int_id = 'export_rev_customs',  birth_id = 'streak_start', logged_version = T, prefix = 'nace_mkt_',
                   full_dataset = T)  %>% select(-nace_mkt) %>% 
    #cleanup 
    .[, `:=`(nace_mkt_share_active_exporters = nace_mkt_num_exporters / nace_num_exporters,
            mkt_share_active_exporters =  mkt_num_exporters/ num_exporters,
            nace_num_exporters = NULL, num_exporters = NULL)] %>% 
  
  # log necessary variables 
  .[,paste0('log_',base_data_to_log) := lapply(.SD,asinh), .SDcols =base_data_to_log]

write_parquet(output,file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet'))

# clear ------------------------------
rm(list= setdiff(ls(), base_env)); gc()


  




  
  


  
  