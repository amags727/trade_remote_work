# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}; gc();
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

## import helper functions 
source('2) code/0_set_parameter_values.R')

# h -----------------------------------------------------------------------

## RUN VARIATIONS 
dta = import_file('1) data/11_parameter_calibration/clean/combined_firm_dta.parquet') %>%
  .[fiscal_year %in% year_range & nation == 'UNITED STATES' & empl_ratio < 10] %>% 
  unbalanced_lag(., 'isin', 'fiscal_year', 'abs_pct_FE_tplus1_t', 1:3) %>% 
  .[, abs_pct_fe_mean_prior3 := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0('abs_pct_FE_tplus1_t_lag',1:3)] %>% 
  .[,paste0('abs_pct_FE_tplus1_t_lag',1:3) := NULL] %>% .[,log_age := asinh(age)] 

feols(data = dta, log_abs_FE_tplus1_t~log_comp_data*log_x_bar + log_comp_total + share_empl_college + avg_prestige| isin + fiscal_year,cluster = ~isin)
feols(data = dta, log_abs_FE_tplus1_t~log_comp_data*abs_pct_fe_mean_prior3 + log_comp_total + share_empl_college + avg_prestige| isin + fiscal_year,cluster = ~isin)









base_command = reg_command('dta', 'log_abs_FE_tplus1_t', 'log_comp_data', ' +log_x_bar + log_comp_total + share_empl_college + avg_prestige',
                           fe = '| isin + fiscal_year', cluster = 'isin' )
variations = data.table(command = c(base_command, gsub('_data', '_data*log_x_bar', base_command))) %>% 
  bind_rows(mutate(., command = gsub('isin', 'industry_group', command))) %>% 
  mutate(fixed_effect = ifelse(grepl('industry',command), 'industry', 'firm'))

model_output = evaluate_variations(variations)[['model_output']]

## BUILD TABLE 
coef_names = c('log payroll data', 'log payroll total', 'share empl. college grad', 'empl. prestige',
               '\\hspace{5 pt}x log avg. firm sales', 'log avg. firm sales')
label = '9_forecasting_reduced_form_results'
format_table(
  model_output, label = label,
  coef_names = coef_names,
  coef_order = c(1,5,6, 2:4),
  headers = make_headers(4, c('Log Abs. T+1 Forecast Error')),
  custom_rows = list('Fixed Effects',c("Industry/Firm", rep(c('firm', 'ind.'), each = 2)),
                     c('Time', rep('\\checkmark', 4))),
  custom_row_placement = c(21:23),
  notes = 'Robust Standard Errors clustered at the firm level.',
  note_width = .5,
  output_path =  paste0(de_dummy(finished_output_dir),  label, '.tex'), make_tex = F)













