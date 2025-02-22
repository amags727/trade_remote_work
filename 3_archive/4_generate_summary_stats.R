# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','vtable')
lapply(packages, library, character.only = T)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

raw_dir = 'C:/Users/Public/1. Microprod/0. Raw data processing/Data/'


# generate moments of rev estimation -------------------------------------
  export_regression_inputs = fread('../1) data/8_revenue_prediction_inputs.csv')
  
  ## export the list and types of each variable 
  variable_list = data.frame(vars = names(export_regression_inputs), type = NA)
  for (i in seq_along(variable_list$vars)){
    variable_list[i,2] = typeof(export_regression_inputs[[i]])
  }
  fwrite(variable_list,'../3) output/summary tables/export regression/variable_list.csv')
  
  vars_to_summarize = names(export_regression_inputs)[!names(export_regression_inputs) %in%
                                                        c('ctry', 'hs_class', 'year','firmid','portfolio_key')]
  
  summary_data = sumtable(export_regression_inputs, vars =vars_to_summarize, out = 'return')
  fwrite(summary_data,'../3) output/summary tables/export regression/summary_data.csv')

  

# generate moments of full estimation -------------------------------------
  full_estimation_inputs = as.data.table(read_dta('../1) data/comp_test.dta'))

  variable_list = data.frame(vars = names(full_estimation_inputs), type = NA)
  for (i in seq_along(variable_list$vars)){
    variable_list[i,2] = typeof(full_estimation_inputs[[i]])
  }
  fwrite(variable_list,'../3) output/summary tables/full regression/variable_list.csv')
  
  
  vars_to_summarize = names(full_estimation_inputs)[!names(full_estimation_inputs) %in%
                                                        c('ctry','ctry_code','hs_class', 'year','firmid','portfolio_key')]
  
  summary_data = sumtable(full_estimation_inputs, vars =vars_to_summarize, out = 'return')
  fwrite(summary_data,'../3) output/summary tables/full regression/summary_data.csv')
  
  
  
  
    
  