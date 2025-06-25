# setup -------------------------------------------------------------------
rm(list = ls());gc()

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data",
    
    ## update as makes sense for CASD / your own use 
    T ~ "C:/Users/Public/Documents/Big data Project")
  setwd(root)
}



source('2) code/0_set_parameter_values.R')

# make extended gravity data  --------------------------------------------------------
making_extended_grav = F
if (making_extended_grav){
  firm_yr = import_file(firm_yr_path) %>% within_group_filter(., 'any(currently_export_customs == T)', 'firmid_num')
  
  export_vars = c('firmid_num', 'ctry_num', 'year', 'exim', 'value','products')
  export_data = import_file(raw_customs_firm_lvl_path, col_select = export_vars) %>%
    .[exim == 2 & year %in% year_range] %>% .[,exim := NULL] %>% rename(export_rev_customs= value)
  
  export_ctries = distinct(export_data, ctry_num)
  expansion_base =  merge(firm_yr[,.(firmid_num, year)], firm_yr %>% distinct(firmid_num))
  
  vars_to_cond = c('products', 'export_rev_customs')
  temp = merge(expansion_base[, dummy :=1], export_ctries[, dummy := 1], allow.cartesian = T, by = 'dummy') %>% .[,dummy := NULL] %>%
    merge(export_data, all.x = T, by = c('firmid_num','year', 'ctry_num')) %>% 
    .[, (paste0(vars_to_cond, '_cond')) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols =vars_to_cond] %>%
    .[, (vars_to_cond) := lapply(.SD, function(x) replace_na(x, 0)), .SDcols =vars_to_cond] %>% 
    .[, currently_export_customs := export_rev_customs > 0]
  
  
  
  ## generate extended gravity and gravity 
  french_distances = import_file(similiarity_dir, 'outputs/france_distance_data.csv') 
  overall_distances =import_file(similiarity_dir, '/outputs/overall_distance_data.csv') 
  
  ## generate gravity 
  similiarity_data =import_file(similiarity_dir, 'outputs/similiarity_data.rds')
  grav_cats =  c('region', 'border', 'language')
  for (x in grav_cats) assign(gpaste(x, '_list'), similiarity_data[[paste0('share_',x)]][1][[1]]) # france is index 1 of similiarity data 
  temp[,(paste0('grav_', grav_cats)) := lapply(paste0(grav_cats, "_list"), function(x) ctry_num %in% get(x))]
  temp =  merge(temp, french_distances, all.x = T, by = 'ctry_num')  %>% rename(grav_dist = distance_to_france_km)
  
  ## add extended gravity
  extended_grav_data = rbindlist(lapply(2:nrow(similiarity_data), function(i){
    print(i/nrow(similiarity_data))
    mkt =  similiarity_data$ctry_num[i]
    temp_distances = overall_distances[o_ctry_num == mkt & d_ctry_num != mkt] %>% rename(ctry_num = d_ctry_num, temp_dist = distance_km) %>% select(ctry_num, temp_dist)
    for (x in grav_cats) assign(gpaste(x, '_list'), setdiff(similiarity_data[[paste0('share_',x)]][i][[1]],mkt ))
    sub_temp = temp[, in_mkt := any(ctry_num == mkt & currently_export_customs), by = .(firmid_num,year)][in_mkt == T & currently_export_customs == T] %>% merge(temp_distances, by = 'ctry_num', all.x = T) %>% 
      .[, c(setNames(lapply(paste0(grav_cats, "_list"), function(x) any(ctry_num %in% get(x))), paste0('extended_grav_', grav_cats)),
            setNames(NA_min(temp_dist), 'extended_grav_dist')),by = .(firmid_num,year)] %>%
      .[, ctry_num := mkt]
  }))
  temp = merge(temp, extended_grav_data, all.x = T, by = c('firmid_num', 'ctry_num', 'year')) %>% 
    .[, paste0('extended_grav_', grav_cats) := lapply(grav_cats, function(x) replace_na(get(paste0('extended_grav_', x)),F))] %>% 
    .[, extended_grav_dist := replace_na(extended_grav_dist,Inf)] %>% 
    .[, (paste0('either_grav_', grav_cats)) := lapply(grav_cats, function(x) get(paste0('grav_', x)) | get(paste0('extended_grav_', x)))] %>%
    .[, either_grav_dist := grav_dist] %>% .[extended_grav_dist < grav_dist, either_grav_dist := extended_grav_dist] 
  
  
  write_parquet(temp, '1) data/0_misc_data/0d_all_countries_all_years_all_firms_grav_data.parquet')
  rm(list= setdiff(ls(), base_env)); gc()
}

# import files  --------------------------------------------------------
extended_grav = import_file('1) data/0_misc_data/0d_all_countries_all_years_all_firms_grav_data.parquet') 

customs_age_data = import_file(firm_ctry_lvl_birth_path) %>% .[exim ==2] %>% .[exim := NULL]
vars_to_any = gpaste(c('currently_export', 'nace_share_export', 'is_first_export_year','log_years_since_first_export_year'),'_customs')
firm_yr = import_file(firm_yr_path) %>% rename_with(.cols = vars_to_any, ~paste0(., '_any_ctry')) %>% 
  select(c('firmid_num', 'year', 'NACE_BR', 'log_dom_turnover','avg_prestige_total','share_empl_college', 'use_data','num_mkts', 'last_observed',
           con_fil(con_fil(., 'log', 'nace_comp_data_quartile', 'any_ctry'), 'BS', 'detrended', inc =F))) %>%
  .[,firmid_year_num := as.numeric(as.factor(paste0(firmid_num,"_", year)))] %>% 
  .[, `:=`(num_firms = .N, num_exporters = NA_sum(currently_export_customs_any_ctry)), by = year] %>% 
  .[,  `:=`(nace_num_firms = .N, nace_num_exporters =  NA_sum(currently_export_customs_any_ctry)), by = .(NACE_BR, year)]


# merge clean and output ------------------------------------------------
vars_to_log = c('export_rev_customs', 'num_other_mkts', 'years_since_first_export_year', 'years_since_streak_start')
output = merge(firm_yr, extended_grav, by = c('firmid_num', 'year')) %>% 
  merge(customs_age_data, by = c('firmid_num', 'ctry_num')) %>%
  
  ## generate popularity vars 
  .[, `:=`(ctry_pop = NA_sum(currently_export_customs) / num_firms, 
           ctry_pop_among_exporters = NA_sum(currently_export_customs) / num_exporters), by = .(year, ctry_num)] %>% 
  
  .[, `:=`(nace_ctry_pop = NA_sum(currently_export_customs)/ nace_num_firms,
           nace_ctry_pop_among_exports = NA_sum(currently_export_customs) / nace_num_exporters), by = .(year, NACE_BR, ctry_num)] %>% 
  
  ## add info on time in mkt 
  .[year >= country_entrance_year,  years_since_first_export_year := year - country_entrance_year] %>% 
  .[year >= streak_start, years_since_streak_start := year - streak_start] %>% 
  .[, is_streak_death := year == streak_end] %>% 
  
  ## misc vars 
  .[,num_other_mkts := num_mkts - currently_export_customs] %>% 
  .[, paste0('log_', vars_to_log) := lapply(vars_to_log, function(x) asinh(get(x)))]

  ## add variance information
  command_1 = 'feols(output, log_export_rev_customs~ years_since_first_export_year| firmid_num + year + ctry_num)'
  command_2 = 'feols(output, log_export_rev_customs~ years_since_streak_start| firmid_num + year + ctry_num)'
    models = list(eval(parse(text = command_1)), eval(parse(text = command_2)))
    for (i in 1:2){
      var_name = gpaste('log_export_rev_customs', ifelse(i==1, '', '_cond'), "_detrended_var")
      non_dropped_obs = setdiff(1:nrow(output),-1*models[[i]]$obs_selection$obsRemoved)
      output[non_dropped_obs, (var_name) := models[[i]]$residuals^2]
    }

write_parquet(output, '1) data/12_firm_yr_ctry_lvl_dta.parquet')








