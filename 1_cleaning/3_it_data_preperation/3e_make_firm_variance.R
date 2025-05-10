

# generate firm level version ---------------------------------------------
## identify key variables 
relative_vars = gpaste(c('comp_data', 'share_comp_data'),"_", c('nace', 'nace_exporter'), "_", gpaste(c('pct_rank', 'sd_from_mean'), c('', "_age")))
vars_to_mean = c(relative_vars, 'dom_turnover', 'empl', 'comp_data', 'comp_rnd', 'share_comp_data', "comp_weighted_prestige", 
                 'total_export_rev_customs',  "num_export_countries",  "avg_products_per_ctry", 
                 "export_mkt_avg_rev_wgted_comp_now", "export_mkt_avg_rev_wgted_comp_l5", "export_mkt_avg_rev_wgted_comp_ever",
                 "nace_churn_rate", "nace_de_trended_log_variance_ind_lvl", "nace_de_trended_log_variance_group_lvl")

vars_to_min = gpaste(c('first_year', 'age'), '_', c('exports_observed', 'dom_rev_observed'))
vars_to_mode = c('NACE_BR','empl_bucket', "quartile_comp_data", "quartile_share_comp_data")
vars_to_any= c('young', 'adolescent')
vars_to_sum = c('years_exports_observed', 'years_dom_rev_observed')
vars_to_variance = c("log_dom_turnover", "log_total_export_rev_customs")
vars_to_log = con_fil(c(vars_to_mean, 'min_age_exports_observed', 'min_age_dom_rev_observed'), 'comp_weighted_prestige', 'nace', inc = F)


firm_lvl_collapsed_variance =  import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% 
  remove_if_NA(., 'year', 'comp_data') %>% .[!is.na(dom_turnover) | !is.na(total_export_rev_customs)] %>% 
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T) %>% 
  
  # we want to only look at people who have tried exporting 
  .[year < first_export_year | is.na(first_export_year), total_export_rev_customs := NA] %>% 
  
  # add necessary variables 
  .[,`:=`(years_dom_rev_observed = !is.na(dom_turnover),
          years_exports_observed = !is.na(total_export_rev_customs))] %>% 
  
  .[,`:=`(first_year_exports_observed= NA_min(year[!is.na(total_export_rev_customs)]),
          first_year_dom_rev_observed = NA_min(year[!is.na(dom_turnover)]),
          age_exports_observed = NA_min(age[!is.na(total_export_rev_customs)]),
          age_dom_rev_observed = NA_min(age[!is.na(dom_turnover)])), by = firmid] %>% 
  
  
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
  ), by = firmid] %>% 
  
  .[,paste0('log_',vars_to_log) := lapply(.SD,asinh), .SDcols = vars_to_log] %>% 
  .[,`:=`(young_at_start_exports_observed = min_age_exports_observed <= 5,
          young_at_start_dom_rev_observed = min_age_dom_rev_observed <= 5)]

write_parquet(firm_lvl_collapsed_variance, file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet'))
rm(list= setdiff(ls(), base_env)); gc()

# make firm ctry lvl version -----------------------------------------------------------------------

firm_ctry_yr_lvl = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')) %>% 
  remove_if_NA(., 'year', 'export_rev_customs', 'comp_data') %>%
  .[year %in% year_range] %>% distinct(firmid, year, ctry, .keep_all = T) %>% 
  .[,years_observed := 1]


vars_to_first = c('streak_start', paste0('first_time_',c('exporting','in_ctry')),'distance_to_france')
vars_to_min = c('age', 'streak_age', 'year')
vars_to_mean = c('num_markets', 'products',  gpaste("comp_", c('ever', 'l5', 'now', 'data', 'rnd', 'weighted_prestige')),
                 'dom_turnover', 'export_rev_customs', 
                 con_fil(con_fil(names(firm_ctry_yr_lvl), 'mkt'), 'log_nace', 'log_mkt','mkt_log_variance', inc = F))
vars_to_log = c('distance_to_france','min_age', 'min_streak_age',
                con_fil(vars_to_mean, 'weighted_prestige', 'rate', 'variance', 'share_active', inc = F))
vars_to_mode = c('NACE_BR','nace_yr_quartile_comp_data', 'nace_yr_quartile_share_comp_data')
vars_to_variance = 'log_export_rev_customs'
vars_to_sum = c('years_observed')
firm_ctry_lvl_collapsed_variance = firm_ctry_yr_lvl %>% 
  # generate log versions of interest vars 
  .[,(vars_to_variance) := lapply(.SD, asinh), .SDcols = gsub('log_', '', vars_to_variance) ] %>% 
  
  # collapse 
  .[, c(
    setNames(lapply(.SD[, ..vars_to_first], NA_first), vars_to_first),
    setNames(lapply(.SD[, ..vars_to_min], NA_min), paste0('min_',vars_to_min)),
    setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean),
    setNames(lapply(.SD[, ..vars_to_mode], NA_mode), vars_to_mode),
    setNames(lapply(.SD[, ..vars_to_sum], NA_sum), vars_to_sum),
    setNames(lapply(.SD[, ..vars_to_variance], NA_var), paste0('var_', vars_to_variance)),
    setNames(lapply(.SD[, ..vars_to_variance], function(x) sub_regression(x, year, asr = T)), paste0('detrended_var_', vars_to_variance))
  ),by = .(ctry, firmid, streak_id)] %>% 
  
  # clean up 
  .[,young_at_start :=min_age <=5] %>% 
  .[,paste0('log_',vars_to_log) := lapply(.SD,asinh), .SDcols = vars_to_log]



write_parquet(firm_ctry_lvl_collapsed_variance, file.path(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet'))


rm(list= setdiff(ls(), base_env)); gc()










