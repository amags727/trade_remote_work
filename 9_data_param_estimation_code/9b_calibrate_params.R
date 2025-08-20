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

# import and clean dta -----------------------------------------------------------------------
min_streak_length = 8
earliest_streak_start = 2000

combined_dta = import_file('1) data/11_parameter_calibration/clean/combined_firm_dta.parquet') %>% 
  .[fiscal_year >= earliest_streak_start] %>% 
  unbalanced_lag(. , 'isin', 'fiscal_year', 'full_info', c(-1,1)) %>%
  .[,streak_id := cumsum(is.na(full_info_lag1))] %>% 
  .[!full_info == T, streak_id := NA] %>%
  .[,streak_start := min(ifelse(full_info_for_start, fiscal_year, Inf)), by = streak_id] %>% 
  .[, is_streak_start := fiscal_year == streak_start] %>% 
  .[, is_streak_end := is.na(full_info_lead1)] %>%
  .[fiscal_year >= streak_start] %>% 
  .[, streak_length :=.N, by = 'streak_id'] %>% 
  .[streak_length >= min_streak_length] %>% 
  .[, x_bar := NA_mean(sales_actual), by = 'streak_id'] %>% 
  .[, t := fiscal_year] %>% 
  .[, L := comp_data] %>% 
  .[, FE := forecast_error] %>% 
  .[is_streak_start== F, forecast_range := NA] %>%
  .[,firm_num := .GRP, by = isin] %>% 
  .[, .(t, isin,is_streak_start, forecast_range, x_bar, L, FE)]

  


