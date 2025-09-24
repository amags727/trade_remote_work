# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))};gc()

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
making_extended_grav = T
if (making_extended_grav){
  firm_yr = import_file(firm_yr_path) %>% within_group_filter(., 'any(currently_export_customs == T)', 'firmid_num')
  
  export_vars = c('firmid_num', 'ctry_num','streak_id', 'year', 'exim', 'value','products')
  export_data = import_file(raw_customs_firm_lvl_path, col_select = export_vars) %>%
    .[exim == 2 & year %in% year_range] %>% .[,exim := NULL] %>% rename(export_rev_customs= value) 
  
  export_ctries = distinct(export_data, ctry_num)
  expansion_base = unique(rbind(export_data[,.(firmid_num, year)], firm_yr[,.(firmid_num, year)]))
  
  vars_to_cond = c('products', 'export_rev_customs')
  temp = expansion_base[, .(ctry_num = export_ctries$ctry_num), by = .(firmid_num, year)] %>%
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
    
    ## generate the country specific lists of distances / gravity compatriots 
    temp_distances = overall_distances[o_ctry_num == mkt & d_ctry_num != mkt] %>% rename(ctry_num = d_ctry_num, temp_dist = distance_km) %>% select(ctry_num, temp_dist)
    for (x in grav_cats) assign(gpaste(x, '_list'), setdiff(similiarity_data[[paste0('share_',x)]][i][[1]],mkt ))
    
    ## assign the extended grav variables based on mkts where the firm operates 
    sub_temp = temp[ctry_num == mkt | currently_export_customs == T] %>% merge(temp_distances, by = 'ctry_num', all.x = T) %>% 
      .[, c(setNames(lapply(paste0(grav_cats, "_list"), function(x) any(ctry_num %in% get(x))), paste0('extended_grav_', grav_cats)),
            setNames(NA_min(temp_dist), 'extended_grav_dist')),by = .(firmid_num,year)] %>%
      .[, ctry_num := mkt]
  }))
  temp = merge(temp, extended_grav_data, all.x = T, by = c('firmid_num', 'ctry_num', 'year')) %>% 
    .[, paste0('extended_grav_', grav_cats) := lapply(grav_cats, function(x) replace_na(get(paste0('extended_grav_', x)),F))] %>% 
    .[, extended_grav_dist := replace_na(extended_grav_dist,Inf)] %>% 
    .[, (paste0('either_grav_', grav_cats)) := lapply(grav_cats, function(x) get(paste0('grav_', x)) | get(paste0('extended_grav_', x)))] %>%
    .[, either_grav_dist := grav_dist] %>% .[extended_grav_dist < grav_dist, either_grav_dist := extended_grav_dist] %>%
    mutate(across(con_fil(.,'grav_dist'), ~asinh(.), .names = 'log_{.col}'))

  write_parquet(temp, extended_grav_path)
  rm(list= setdiff(ls(), base_env)); gc()
}

# import files  --------------------------------------------------------
making_network_closeness = F
if (making_network_closeness){
  
  extended_grav = import_file(extended_grav_path, col_select=c('ctry_num', 'firmid_num')); gc()
  
  n_groups=10
  unique_firmids <- data.table(firmid_num=unique(extended_grav$firmid_num)) %>%
    .[, firm_group := ntile(firmid_num, n_groups)] 
  
  rm(extended_grav); gc()
  network_closeness_path<-"1) data/18_network_closeness/"
  suppressWarnings(dir.create(network_closeness_path))
  
  for(i in 1:n_groups){
    
    print(i)
    temp_firmids <- unique(unique_firmids[firm_group==i]$firmid_num)
    
    network_closeness = import_file(extended_grav_path, col_select=c('ctry_num', 'firmid_num', 'year', 'export_rev_customs')) %>%
      .[firmid_num %in% temp_firmids] %>%
      
      ## add domestic revenue
      rbind(import_file(firm_yr_path, col_select = c('dom_turnover', 'firmid_num', 'year')) %>% .[dom_turnover >0] %>%
              rename(export_rev_customs =dom_turnover) %>% .[,ctry_num := 0]) %>%
      
      ## for each firm-destination-year add all the other destinations the firm has in network
      merge(.[export_rev_customs > 0] %>% rename(d_ctry_num = ctry_num, d_export_rev_customs = export_rev_customs) %>%
              .[, c('firmid_num', 'year', 'd_ctry_num', 'd_export_rev_customs')], by=c('firmid_num', 'year'), allow.cartesian = T) %>%
      rename(o_ctry_num = ctry_num) %>% .[o_ctry_num != d_ctry_num & o_ctry_num != 0] %>%
      
      ## merge in distance data and calc weighted mean
      merge(import_file(similiarity_dir, '/outputs/overall_distance_data.csv'), by = con_fil(., 'ctry')) %>%
      .[, france := d_ctry_num == 0] %>%
      .[, .(network_closeness_inc_france = asinh(weighted.mean(distance_km, d_export_rev_customs)),
            network_closeness_excl_franc = asinh(weighted.mean(distance_km[!france], d_export_rev_customs[!france]))), by = . (o_ctry_num,firmid_num, year)] %>%
      rename(ctry_num = o_ctry_num)
    
    write_parquet(network_closeness, paste0(network_closeness_path, "tmp", i, ".parquet"))
    rm(network_closeness); gc()
    
  }
  
  network_closeness<-data.table()
  for(i in 1:n_groups){
    network_closeness_temp<-import_file(paste0(network_closeness_path, "tmp", i, ".parquet"))
    network_closeness<-rbind(network_closeness, network_closeness_temp, fill=T)
    rm(network_closeness_temp); gc()
  }
  write_parquet(network_closeness, paste0(network_closeness_path, "newtwork_closeness.parquet"))
  
}

network_closeness_path<-"1) data/18_network_closeness/"
network_closeness <- open_dataset(paste0(network_closeness_path, "newtwork_closeness.parquet")) %>% collect()

# extended_grav = import_file(extended_grav_path) THIS COMMAND FAILS BECAUSE OF MEMORY ISSUES
extended_grav <- open_dataset(extended_grav_path)
extended_grav <- extended_grav %>% collect()

vars_to_any = gpaste(c('currently_export', 'nace_share_export', 'is_first_export_year','log_years_since_first_export_year'),'_customs')
firm_yr = import_file(firm_yr_path) %>% rename_with(.cols = vars_to_any, ~paste0(., '_any_ctry')) %>% 
  select(c('firmid_num', 'year', 'NACE_BR', 'log_dom_turnover','avg_prestige_total', 'empl', 'empl_bin', 'young',
           'share_empl_college', 'use_data','use_data_lag1','num_mkts', 'last_observed',"capital_intensity",
           con_fil(con_fil(., 'log', 'nace_comp_data_quartile', 'any_ctry'), 'BS', 'detrended', inc =F))) %>%
  .[,firmid_year_num := as.numeric(as.factor(paste0(firmid_num,"_", year)))] %>% 
  .[, `:=`(num_firms = .N, num_exporters = NA_sum(currently_export_customs_any_ctry)), by = year] %>% 
  .[,  `:=`(nace_num_firms = .N, nace_num_exporters =  NA_sum(currently_export_customs_any_ctry)), by = .(NACE_BR, year)]

age_data = import_file(firm_lvl_birth_path, col_select = c('firmid_num','first_import_year', 'first_export_year_customs', 'birth_year'))
customs_age_data = import_file(firm_ctry_lvl_birth_path, col_select = c('firmid_num', 'ctry_num', 'streak_id', 'streak_start', 'streak_end', 'country_entrance_year')) %>% 
  rename(ctry_entrance_year = country_entrance_year)

# merge clean and output ------------------------------------------------
vars_to_log = c('export_rev_customs', 'num_other_mkts', 'years_since_first_export_year', 'years_since_streak_start')
output = merge(firm_yr, extended_grav, by = c('firmid_num', 'year')) %>% 
  merge(network_closeness, all.x=T, by=c('firmid_num', 'year', 'ctry_num')) %>%
  
  ## add age data 
  merge(age_data, by = c('firmid_num')) %>% 
  merge(customs_age_data, all.x = T, by = c('firmid_num', 'ctry_num', 'streak_id')) %>%
  .[, ctry_entrance_year := NA_min(ctry_entrance_year), by = .(firmid_num, ctry_num)] %>%   
  
  ## generate popularity vars 
  .[, `:=`(ctry_pop = NA_sum(currently_export_customs) / num_firms, 
           ctry_pop_among_exporters = NA_sum(currently_export_customs) / num_exporters), by = .(year, ctry_num)] %>% 
  
  .[, `:=`(nace_ctry_pop = NA_sum(currently_export_customs)/ nace_num_firms,
           nace_ctry_pop_among_exports = NA_sum(currently_export_customs) / nace_num_exporters), by = .(year, NACE_BR, ctry_num)] %>% 
  
  ## add info on time in mkt 
  .[year >= ctry_entrance_year,  years_since_first_export_year := year - ctry_entrance_year] %>% 
  .[year >= streak_start, years_since_streak_start := year - streak_start] %>% 
  .[, is_streak_death := year == streak_end] %>% 
  
  ## misc vars 
  .[,num_other_mkts := num_mkts - currently_export_customs] %>% 
  .[, paste0('log_', vars_to_log) := lapply(vars_to_log, function(x) asinh(get(x)))] 

rm(network_closeness, firm_yr, extended_grav); gc()

# add variance information ------------------------------------------------
# generate measures of de-trended variance of each firm per year 
detrended_vars = gpaste('log_export_rev_customs_cond_detrended_var', c('', '_wo_ctry_fe'))
if (dummy_version){ output[, (detrended_vars) := runif(.N)]}else{
  command_1 = 'feols(output, log_export_rev_customs~ years_since_streak_start| firmid_num + year + ctry_num)'
  command_2 = gsub('\\+ ctry_num', '',command_1)
  models = list(eval(parse(text = command_1)), eval(parse(text = command_2)))
  for (i in 1:2){
    var_name = detrended_vars[i]
    non_dropped_obs = setdiff(1:nrow(output),-1*models[[i]]$obs_selection$obsRemoved)
    output[non_dropped_obs, (var_name) := models[[i]]$residuals^2]
  }}

# variance of market as a whole over time 
ctry_group_var_temp = output[, .(export_rev_customs = NA_sum(export_rev_customs)) , by = .(ctry_num, year)] %>% 
  .[, .(ctry_log_variance_group_lvl = NA_sd(asinh(export_rev_customs))), by = ctry_num]

#avg variance of firms in each market over time 
ctry_ind_var_temp = output[, .(ind_log_var = NA_sd(asinh(export_rev_customs_cond))), by = .(firmid_num,ctry_num, streak_id)] %>% 
  .[, .(ctry_log_variance_ind_lvl = NA_mean(ind_log_var)), by = ctry_num]

# avg detrended-variance of firms in each market 
ctry_detrended_var_temp = output[, .(ctry_detrended_var_yr_to_yr = NA_mean(log_export_rev_customs_cond_detrended_var_wo_ctry_fe)), by = .(ctry_num,year)]  %>% 
  .[, ctry_detrended_var := NA_mean(ctry_detrended_var_yr_to_yr), by = ctry_num]

rm(age_data, customs_age_data, models); gc()

# THIS RUNS INTO MEMORY ISSUES
# output = output %>% merge(ctry_group_var_temp , by ='ctry_num') %>% 
#   merge(ctry_ind_var_temp, by ='ctry_num')  %>% 
#   merge(ctry_detrended_var_temp, by = c('ctry_num', 'year'))

# combine 
output =  merge(output, ctry_group_var_temp , by ='ctry_num')
rm(ctry_group_var_temp); gc()
output =  merge(output, ctry_ind_var_temp , by ='ctry_num')
rm(ctry_ind_var_temp); gc()
output =  merge(output, ctry_detrended_var_temp , by =c('ctry_num', 'year'))
rm(ctry_detrended_var_temp); gc()


  
 # add churn information -----------------------------------------------------------------------
churn_temp = output[year < max(year_range) & year > min(year_range), .(
  ctry_entrance_rate_yr_to_yr = NA_mean(year == streak_start),
  ctry_exit_rate_yr_to_yr = NA_mean(year == streak_end),
  ctry_churn_rate_yr_to_yr = NA_mean(year == streak_end | year == streak_start),
  ctry_immediate_failure_rate_yr_to_yr = NA_mean(year == streak_end & year == streak_start)), by = .(ctry_num, year)] %>% 
  group_by(ctry_num) %>% mutate(across(con_fil(., 'yr_to_yr'), ~ NA_mean(.), .names = "{.col}hh")) %>% 
  rename_with(~gsub('_yr_to_yrhh', '',.))

output = merge(output, churn_temp, by = c('ctry_num', 'year'), all.x = T) 


# export ------------------------------------------------------------------
write_parquet(output, firm_yr_ctry_path) 

