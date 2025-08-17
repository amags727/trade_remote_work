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
# import component datasets --------------------------------------------------------
age_data = import_file(firm_lvl_birth_path, col_select =  c('birth_year', 'firmid_num', 'last_observed'))
linkedin_firm_yr = import_file(linkedin_firm_yr_path) 

bs_vars = c('firmid_num', 'year','dom_turnover', 'empl', 'capital', 'intangible_fixed_assets', 'labor_cost','turnover')
bs_data = import_file(raw_bs_br_path,col_select = bs_vars) %>% 
  .[year %in% year_range & firmid_num %in% linkedin_firm_yr$firmid_num] %>% 
  .[, capital_intensity:=ifelse(turnover==0, NA, log(capital/(turnover)))]

euro_list = import_file(similiarity_dir, 'outputs/similiarity_data.rds')[['share_region']][[1]]
firm_euro_lvl_streak_info = import_file(
  raw_customs_firm_lvl_path, col_select =  c('firmid_num', 'ctry_num', 'year', 'exim', 'value')) %>%
  .[exim == 2] %>%  
  .[,euro := ctry_num %in% euro_list] %>% 
  .[,.(value = NA_sum(value)), by = .(firmid_num, year, euro)] %>%  
  unbalanced_lag(. , c('firmid_num', 'euro'), 'year', 'value', 1) %>% 
  .[,export_streak_id_customs := cumsum(is.na(value_lag1))] %>% 
  .[, streak_start_year := min(year), by = export_streak_id_customs] %>% 
  .[, first_export_year_customs := min(year), by = .(firmid_num, euro)] %>%
  merge(age_data[,.(firmid_num, birth_year)], by = 'firmid_num') %>% 
  
  # correct first export year (streak and firm level)
  .[streak_start_year == 1994, streak_start_year := ifelse(birth_year == 1994, streak_start_year, NA)] %>% 
  .[first_export_year_customs == 1994, first_export_year_customs := ifelse(birth_year == 1994, first_export_year_customs, NA)] %>% 
  .[,export_streak_age_customs := year - streak_start_year] %>%
  .[,.(firmid_num,year, euro, export_streak_id_customs, export_streak_age_customs)]

export_EU_data = import_file(raw_customs_firm_lvl_path, col_select = c('firmid_num', 'year', 'exim', 'ctry_num', 'value')) %>%
  .[exim == 2 & year %in% year_range] %>% 
  .[,euro := ctry_num %in% euro_list] %>% 
  .[,.(total_export_rev_customs_cond = sum(value, na.rm = T)), by = .(firmid_num, year, euro)]


# prepare output  ---------------------------------------------------------
suffixes = c('customs')
vars_to_cond = 'total_export_rev_customs'
vars_to_log = c('age', 'dom_turnover',  gpaste("total_export_rev_customs", c('', '_cond')), 'export_streak_age_customs')

     
# start output generation by making parallel version based for euro / non-euro 
output = rbind(bs_data[, euro := F], bs_data[, euro := T]) %>% 
  
  # merge linkedin
  merge(linkedin_firm_yr, by = c('firmid_num', 'year')) %>% 
  
  # merge process age data 
  merge(age_data, by = c('firmid_num'), all.x = T) %>% 
  .[, age := year - birth_year] %>% remove_if_NA('age') %>% 
  
  # merge and process customs data 
  merge(export_EU_data, all.x = T,  by = c('firmid_num', 'year', 'euro')) %>% 
  .[, total_export_rev_customs := replace_na(total_export_rev_customs_cond,0)] %>% 
  .[, currently_export_customs := !is.na(total_export_rev_customs_cond)] %>% 
  
  # merge streak info 
  merge(firm_euro_lvl_streak_info, all.x = T, by= c('firmid_num', 'year', 'euro')) %>% 
  
  # make log variables 
  .[,paste0('log_', vars_to_log) := lapply(vars_to_log, function(x) asinh(get(x)))] %>% 
  .[,log_dom_turnover_sq := log_dom_turnover^2] %>% 
    
  # make vars based on leads 
  unbalanced_lag(.,c('firmid_num', 'euro'), 'year',  'currently_export_customs', -1, expand = T, expand_value = F, death_var = 'last_observed') %>% 
  .[currently_export_customs == T, stop_exporting_customs := !currently_export_customs_lead1] %>% 
  
  ## add industry variables 
  .[, nace_share_export_customs := NA_mean(currently_export_customs), by = .(NACE_BR, year, euro)] 

  ## add detrended variance values 
  if(dummy_version) model = feols(output, log_total_export_rev_customs_cond ~ log_export_streak_age_customs )
  if(!dummy_version) model = feols(output, log_total_export_rev_customs_cond ~ log_export_streak_age_customs | firmid_num + year)
  non_dropped_obs = setdiff(1:nrow(output),-1*model$obs_selection$obsRemoved)
  output[non_dropped_obs, log_total_export_rev_customs_cond_detrended_var:= model$residuals^2]

## Output the file 
write_parquet(output,firm_euro_yr_path)
