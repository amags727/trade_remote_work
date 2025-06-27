# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))};

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')


# firm yr  ----------------------------------------------------------------
file_name = file.path(inputs_dir, '16c_firm_yr_lvl.parquet')
base = import_file(file_name) 
vars_to_yr = c('log_comp_data')
for (yr in year_range) base[, (gpaste(vars_to_yr, "_y", yr)) := lapply(vars_to_yr, function(x) ifelse(year == yr, get(x), 0))]
write_parquet(base, file_name)
rm(list= setdiff(ls(), base_env)); gc()

# firm ctry year  ---------------------------------------------------------
file_name = file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')
base = import_file(file_name) 
vars_to_yr = c('log_comp_data')

for (yr in year_range) base[, (gpaste(vars_to_yr, "_y", yr)) := lapply(vars_to_yr, function(x) ifelse(year == yr, get(x), 0))]
write_parquet(base, file_name)
rm(list= setdiff(ls(), base_env)); gc()








