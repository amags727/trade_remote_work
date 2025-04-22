

# generate firm level version ---------------------------------------------
## identify key variables 
vars_to_mean = c('dom_turnover', 'empl', 'comp_data', 'comp_rnd', 'share_comp_data', "comp_weighted_prestige", 
                 'total_export_rev_customs',  "num_export_countries",  "avg_products_per_ctry", 
                 "export_mkt_avg_comp_now", "export_mkt_avg_comp_l5", "export_mkt_avg_comp_ever")
vars_to_min = c('age', 'year')
vars_to_mode = c('empl_bucket', "quartile_comp_data", "quartile_share_comp_data")
vars_to_any= c('young', 'adolescent')
vars_to_sum = c('years_exporting', 'years_observed')
vars_to_variance = c("log_dom_turnover", "log_total_export_rev_customs")


firm_lvl_collapsed_variance =  import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% 
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T) %>% 
  
  # we want to only look at people who have tried exporting 
  .[year < first_export_year | is.na(first_export_year), total_export_rev_customs := NA] %>% 
  
  # gen years_exporting / observed 
  rename(years_exporting = currently_export) %>% 
  mutate(years_observed = 1) %>% 
  
  # generate log versions of interest vars 
  .[,(vars_to_variance) := lapply(.SD, asinh), .SDcols = gsub('log_', '', vars_to_variance) ] %>% 
  
  .[, c(
  setNames(lapply(.SD[, ..vars_to_min], NA_min), paste0('min_',vars_to_min)),
  setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean),
  setNames(lapply(.SD[, ..vars_to_mode], NA_mode), vars_to_mode),
  setNames(lapply(.SD[, ..vars_to_any], NA_any), vars_to_any),
  setNames(lapply(.SD[, ..vars_to_sum], NA_sum), vars_to_sum),
  setNames(lapply(.SD[, ..vars_to_variance], NA_var), paste0('var_', vars_to_variance)),
  setNames(lapply(.SD[, ..vars_to_variance], function(x)sub_regression(x, year, asr = T)), paste0('detrended_var_', vars_to_variance))
  ), by = firmid]
write_parquet(firm_lvl_collapsed_variance, file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet'))

# make firm ctry lvl version -----------------------------------------------------------------------
firm_ctry_yr_lvl = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')) %>% 
  .[year %in% year_range] %>% distinct(firmid, year, ctry, .keep_all = T) %>% 
  .[,years_observed := .N, by = .(ctry, firmid, streak_id)]


vars_to_first = c(paste0('grav_', c('region', 'language', 'border')), 'streak_start', 
                  paste0('first_time_',c('exporting','in_ctry')),'distance_to_france')
vars_to_min = c('age', 'streak_age', 'year')
vars_to_mean = c('num_markets', 'products',  gpaste(c('empl', 'comp'),"_", c('ever', 'l5', 'now')),
                 'dom_turnover', 'export_rev_customs', con_fil(names(firm_ctry_yr_lvl), 'mkt'),
                 'years_observed')
vars_to_mode = c('NACE_BR','nace_yr_quartile_comp_data', 'nace_yr_quartile_share_comp_data')
vars_to_variance = 'log_export_rev_customs'

firm_ctry_lvl_collapsed_variance = firm_ctry_yr_lvl %>% 
  # generate log versions of interest vars 
  .[,(vars_to_variance) := lapply(.SD, asinh), .SDcols = gsub('log_', '', vars_to_variance) ] %>% 
  .[, c(
    setNames(lapply(.SD[, ..vars_to_min], NA_min), paste0('min_',vars_to_min)),
    setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean),
    setNames(lapply(.SD[, ..vars_to_mode], NA_mode), vars_to_mode),
    setNames(lapply(.SD[, ..vars_to_variance], NA_var), paste0('var_', vars_to_variance)),
    setNames(lapply(.SD[, ..vars_to_variance], function(x) sub_regression(x, year, asr = T)), paste0('detrended_var_', vars_to_variance))
  ),by = .(ctry, firmid, streak_id)]

write_parquet(firm_ctry_lvl_collapsed_variance, file.path(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet'))


rm(list= setdiff(ls(), base_env)); gc()










