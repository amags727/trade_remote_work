# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))};

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/Big Data Code",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')

# set parameters /import files  -----------------------------------------------------------

# set main parameter values 
  linkedin_ctry_vars = gpaste(c('empl', 'comp'),"_", c('now', 'l5', 'ever'))
  linkedin_vars = c('firmid', 'year', gpaste( 'comp_', c('data', 'engineer', 'french', 'french_data', 'rnd', 'stem', 'weighted_prestige')))
  linkedin_to_log = setdiff(linkedin_vars, c('firmid', 'year', 'weighted_prestige'))
  base_data_to_log =  c('num_markets','distance_to_france','age', 'dom_turnover', 'export_rev_customs','other_market_rev',
                        gpaste(c('comp'), '_', c('ever', 'l5', 'now')),
                        gpaste(c('mkt_', 'nace_mkt_'), c('num_exporters','size_rev')))
  firm_yr_rel_vars =  gpaste(c("comp_data", "share_comp_data"),"_", c('nace', 'nace_exporter'),"_", gpaste(c('pct_rank','sd_from_mean'),c('', "_age")))
  firm_yr_vars = c('firmid', 'year', 'dom_turnover', 'NACE_BR', "empl", 'share_comp_data', firm_yr_rel_vars)

  
# set params for relative data intensity
  d_vars = c("comp_data", "share_comp_data")
  divisions_list = list(list('ctry', c('ctry', 'year')), list('ctry_nace', c('ctry', 'NACE_BR', 'year')))


# import files 
  firm_yr_lvl = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'), col_select = firm_yr_vars) %>%
                .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T)
  
  export_data = c('firmid', 'ctry','streak_id', 'exim', 'products', 'deflated_value',  'year') %>%
    import_file('1) data/9_customs_cleaned.csv', col_select = ., char_vars = 'firmid') %>%
    .[year %in% year_range & ctry != 'FR' & exim == 2] %>% .[, exim := NULL] %>% distinct(firmid,year,ctry, streak_id, .keep_all = T) %>%
    rename('export_rev_customs' = 'deflated_value') %>% remove_if_NA('export_rev_customs')
    
  similiarity_data = import_file ('1) data/similarity_matrices/outputs/similiarity_data.rds')

  linkedin_ctry_lvl = import_file(linkedin_ctry_lvl_path)
  linkedin_firm_lvl = import_file(linkedin_basic_path, col_select = linkedin_vars) %>% 
    .[,paste0('log_',linkedin_to_log) := lapply(.SD,asinh), .SDcols = linkedin_to_log] 
  
  linkedin_matched_firms = import_file(linkedin_match_path)[['firmid']]
  customs_birth_data = import_file('9_age_data/9c_firm_ctry_lvl_birth_data.parquet')
  french_distances = fread('1) data/similarity_matrices/outputs/france_distance_data.csv') 
# merge and clean  -----------------------------------------------------------

## add  gravity 
grav_cats =  c('region', 'border', 'language')
i = which(similiarity_data$ctry == 'FR')
for (x in grav_cats) assign(gpaste(x, '_list'), similiarity_data[[paste0('share_',x)]][i][[1]])
export_data[,(paste0('grav_', grav_cats)) := lapply(paste0(grav_cats, "_list"), function(x) ctry %in% get(x))]

## add extended gravity
extended_grav_data = rbindlist(lapply(1:nrow(similiarity_data), function(i){
  mkt =  similiarity_data$ctry[i]
  for (x in grav_cats) assign(gpaste(x, '_list'), setdiff(similiarity_data[[paste0('share_',x)]][i][[1]],mkt ))
  temp = export_data[, in_mkt := any(ctry == mkt), by = .(firmid,year)][in_mkt == T] %>% 
    .[, setNames(lapply(paste0(grav_cats, "_list"),
                        function(x) any(ctry %in% get(x))), paste0('extended_grav_', grav_cats)),
      by = .(firmid,year)] %>% 
    .[, ctry := mkt]
}))
export_data = merge(export_data, extended_grav_data, all.x = T, by = c('firmid', 'ctry', 'year')) %>% 
  .[, (paste0('either_grav_', grav_cats)) := lapply(grav_cats,
  function(x) get(paste0('grav_', x)) | get(paste0('extended_grav_', x)))] 
rm(extended_grav_data); gc()
  
output = export_data %>% 
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
    calc_churn_rates(., 'ctry', 'streak_start', 'last_year_of_streak', 'year', 'mkt') %>%
    
    # NACE / ctry 
    .[, `:=`(nace_mkt_failure_rate = NA_sum(year == streak_start & year == streak_end) / NA_sum(!is.na(streak_start + streak_end)),
             nace_mkt_num_exporters = uniqueN(firmid),
             nace_mkt_size_rev = NA_sum(export_rev_customs),
             nace_mkt = paste0(NACE_BR, ctry)), by = .(NACE_BR, year, ctry)] %>% 
    variance_metrics(time_id = 'year', group_id = 'nace_mkt', ind_id = 'streak_id',
                   int_id = 'export_rev_customs',  birth_id = 'streak_start', logged_version = T, prefix = 'nace_mkt_',
                   full_dataset = T)  %>% select(-nace_mkt) %>% 
    calc_churn_rates(., c('ctry','NACE_BR'), 'streak_start', 'last_year_of_streak', 'year', 'nace_mkt') %>% 
    
    #cleanup 
    .[, `:=`(nace_mkt_share_active_exporters = nace_mkt_num_exporters / nace_num_exporters,
            mkt_share_active_exporters =  mkt_num_exporters/ num_exporters,
            nace_num_exporters = NULL, num_exporters = NULL)] %>% 
    
    # add variable for revenue in other markets 
    .[, other_market_rev := NA_sum(export_rev_customs) - export_rev_customs, by = .(firmid, year)] %>% 
  
  
  # log necessary variables 
  .[,paste0('log_',base_data_to_log) := lapply(.SD,asinh), .SDcols =base_data_to_log]

  # add final comparison variables 
  for (i in 1:length(divisions_list)){inner = divisions_list[[i]][[1]]; outer = ''; group = divisions_list[[i]][[2]]
  for (j in 1:2){if (j ==2){outer = "_age"; group = c(group,'young')}
    output = output %>% 
      .[, (gpaste(d_vars,'_',inner,'_pct_rank',outer)) := lapply(d_vars, function(x) percent_rank(get(x))), by = group] %>%
      .[, (gpaste(d_vars,'_',inner,'_sd_from_mean',outer)) := lapply(d_vars, function(x)(get(x)- NA_mean(get(x)))/ NA_sd(get(x))), by = group]
  }
  }
  
  # add year variables for event study 
  vars_to_yr = c('comp_data', 'log_comp_data')
  for (yr in year_range) output[, (gpaste(vars_to_yr, "_y", yr)) := lapply(vars_to_yr, function(x) ifelse(year == yr, get(x), 0))]
  

write_parquet(output,file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet'))
# clear ------------------------------
rm(list= setdiff(ls(), base_env)); gc()


  




  
  


  
  
