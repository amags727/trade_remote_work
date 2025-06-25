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
# import component datasets --------------------------------------------------------
age_vars =  c('birth_year', gpaste('first_export_year_', c('customs', 'BS')),'last_observed', 'firmid_num')
age_data = import_file('1) data/9_age_data/9b_firm_lvl_birth_data.parquet', col_select = age_vars)

linkedin_vars = c('firmid_num', 'year', gpaste(c('empl', 'avg_prestige'),'_', c('total', 'data')), 'share_comp_data',
                  gpaste(c('nace_comp_data_quartile', 'log_comp_total', 'log_comp_data', 'use_data'), c("", '_lag1')), 'NACE_BR',
                  'share_data_empl_analyst','share_empl_college') %>% unique() %>% setdiff(., c('avg_prestige_data'))
linkedin_firm_yr = import_file(linkedin_firm_yr_path, col_select = linkedin_vars) 

bs_vars = c('firmid_num', 'year','dom_turnover', 'empl', 'capital', 'intangible_fixed_assets','for_turnover', 'labor_cost')
bs_data = import_file('1) data/3_bs_br_data.parquet',col_select = bs_vars) %>% 
  rename(total_export_rev_BS = for_turnover) %>%
  .[year %in% year_range & firmid_num %in% linkedin_firm_yr$firmid_num]

firm_lvl_streak_info = import_file('1) data/9_age_data/9d_firm_lvl_export_streak_info.parquet')
export_vars = c('firmid_num', 'year', 'exim', 'value','products')
export_data = import_file('1) data/6_customs_firm_level.parquet', col_select = export_vars) %>%
  .[exim == 2 & year %in% year_range] %>% .[,exim := NULL] %>%
  .[,.(num_mkts = .N, total_export_rev_customs = sum(value, na.rm = T), products_per_ctry = NA_mean(products)), by = .(firmid_num, year)]



# prepare output  ---------------------------------------------------------

suffixes = c('customs', 'BS')
vars_to_cond = c('num_mkts', 'products_per_ctry', gpaste('total_export_rev_' ,suffixes))
vars_to_log = c('age', 'dom_turnover',  gpaste("total_export_rev_",suffixes, c('', '_cond')),
                gpaste(c('export_streak_age', 'years_since_first_export_year'), "_",suffixes))


output = merge(bs_data, linkedin_firm_yr, by = c('firmid_num', 'year')) %>% 

  ## merge in process/age data
  merge(age_data, by = 'firmid_num', all.x = T) %>% 
  .[, age := year - birth_year] %>% remove_if_NA('age') %>% 
  .[, (paste0("years_since_first_export_year_", suffixes)) := lapply(suffixes, function(x) ifelse(year <get(paste0('first_export_year_', x)), NA, year -get(paste0('first_export_year_', x))))] %>% 
  .[, (paste0("is_first_export_year_", suffixes)) := lapply(suffixes, function(x) year == get(paste0('first_export_year_', x)))] %>% 
  .[, `:=`(age_bracket = as.factor(case_when(age <= 5 ~ 1, age <= 10 ~2, age <= 20 ~3, T ~4)), young = age <=5)]  %>% 
  
  
  ## merge in process export data 
  merge(export_data, all.x = T,  by = c('firmid_num', 'year')) %>% 
  .[, (paste0(vars_to_cond, '_cond')) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols =vars_to_cond] %>%
  .[, (vars_to_cond) := lapply(.SD, function(x) replace_na(x, 0)), .SDcols =vars_to_cond] %>% 
  .[, (paste0('currently_export_', c('customs', 'BS'))) := lapply(c('customs', 'BS'), function(x) get(paste0('total_export_rev_',x))>0)] %>% 
 
 
  ## merge in info on export streaks 
  merge(firm_lvl_streak_info, all.x = T, by= c('firmid_num', 'year')) %>% 
  
  ## make log variables 
  .[,paste0('log_', vars_to_log) := lapply(vars_to_log, function(x) asinh(get(x)))] %>% 
  .[,log_dom_turnover_sq := log_dom_turnover^2] %>% 

  ## make vars based on leads 
  unbalanced_lag(.,'firmid_num', 'year', c('currently_export_BS', 'currently_export_customs'), -1, expand = T, expand_value = F, death_var = 'last_observed') %>% 
  .[currently_export_BS == T, stop_exporting_BS := !currently_export_BS_lead1] %>% 
  .[currently_export_customs == T, stop_exporting_customs := !currently_export_customs_lead1] %>% 
  
  ## add industry variables 
  .[, gpaste('nace_share_export_', suffixes):= lapply(suffixes, function(x) NA_mean(get(paste0('currently_export_', x)))), by = c('NACE_BR', 'year')]

  ## add detrended variance values 
  for( suffix in suffixes){
    command_1 = gpaste('feols(output, log_total_export_rev_',suffix, '~log_age| firmid_num + year)')
    command_2 = gsub('log_age', gpaste('log_export_streak_age_', suffix), command_1)
    models = list(eval(parse(text = command_1)), eval(parse(text = command_2)))
    for (i in 1:2){
      var_name = gpaste('log_total_export_rev_',suffix, ifelse(i==1, '', '_cond'), "_detrended_var")
      non_dropped_obs = setdiff(1:nrow(output),-1*models[[i]]$obs_selection$obsRemoved)
      output[non_dropped_obs, (var_name) := models[[i]]$residuals^2]
      output[, paste0('nace_avg_', var_name):= NA_mean(get(var_name)), by = .(NACE_BR, year)]
    }
  }

write_parquet(output,firm_yr_path)
rm(list= setdiff(ls(), c(base_env))); gc()
# generate unmatched input for summary stats ------------------------------

#### 
#import data
####
linkedin_firms =import_file(linkedin_firm_yr_path, col_select = 'firmid_num') %>% pull('firmid_num') %>% unique()

bs_vars = c('firmid_num', 'year', 'dom_turnover', 'empl', 'capital', 'intangible_fixed_assets','for_turnover')
bs_data = import_file('1) data/3_bs_br_data.parquet',col_select = bs_vars) %>% 
  rename(total_export_rev_BS = for_turnover) %>%
  .[year %in% year_range &! firmid_num %in% linkedin_firms] 

export_vars = c('firmid_num', 'year', 'exim', 'value','products')
export_data = import_file('1) data/6_customs_firm_level.parquet', col_select = export_vars) %>%
  .[exim == 2 & year %in% year_range &! firmid_num %in% linkedin_firms] %>% .[,exim := NULL] %>%
  .[,.(num_mkts = .N, total_export_rev_customs = sum(value, na.rm = T), products_per_ctry = NA_mean(products)), by = .(firmid_num, year)]

age_data = import_file('1) data/9_age_data/9b_firm_lvl_birth_data.parquet', col_select = c('firmid_num', 'birth_year'))
firm_yr_linkedin = import_file(firm_yr_path) %>% .[,`:=`(comp_data = sinh(log_comp_data), comp_total= sinh(log_comp_total)) ]

#### 
# merge and clean
####
suffixes = c('customs', 'BS')
vars_to_cond = c('num_mkts', 'products_per_ctry', gpaste('total_export_rev_' ,suffixes))

output = bs_data %>% 
  ## add export data / vars 
  merge(export_data, all.x = T, by = c('firmid_num', 'year')) %>% 
  .[, (paste0(vars_to_cond, '_cond')) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols =vars_to_cond] %>%
  .[, (vars_to_cond) := lapply(.SD, function(x) replace_na(x, 0)), .SDcols =vars_to_cond] %>%
  .[, (paste0('currently_export_', c('customs', 'BS'))) := lapply(c('customs', 'BS'), function(x) get(paste0('total_export_rev_',x))>0)] %>% 
  
  ## add in age data / vars 
  merge(age_data, all.x = T, by = 'firmid_num') %>% 
  .[,age := year - birth_year] 

## combine with the linkedin firms 
output = rbindlist(list(output,firm_yr_linkedin), use.names = T, fill = T)[, in_linkedin := !is.na(comp_data)]

## export 
write_parquet(output, '1) data/11_firm_yr_summary_stats_input.parquet')
# cleanup -----------------------------------------------------------------
rm(list= setdiff(ls(), c(base_env))); gc()


