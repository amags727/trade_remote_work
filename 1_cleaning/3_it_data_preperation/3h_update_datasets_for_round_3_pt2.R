# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}
source('2) code/0_set_parameter_values.R')




# update firm_yr_lvl ------------------------------------------------------
import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% 
  .[, `:=`(log_empl = asinh(empl), log_capital = asinh(capital))] %>% 
  write_parquet(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'))

# update firm_yr_ctry_lvl -------------------------------------------------
base = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')) %>% 
  .[, other_market_rev := NA_sum(export_rev_customs) - export_rev_customs, by = .(firmid, year)] %>% 
  .[, log_other_market_rev := asinh(other_market_rev)]
  
write_parquet(base,file.path(inputs_dir,'16d_firm_ctry_yr_lvl.parquet'))
rm(list= setdiff(ls(), base_env)); gc()

# update country entrance  ------------------------------------------------
firm_yr_vars = c('firmid', 'year','log_empl', 'log_capital')
firm_yr = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'), col_select = firm_yr_vars) 

for (file in list.files('1) data/temp_data',recursive = TRUE, full.names = TRUE)){
  import_file(file,char_vars = 'firmid') %>% select(-intersect(names(.), firm_yr_vars[-c(1:2)])) %>% 
    merge(firm_yr, all.x = T,  by = c('firmid','year')) %>%
    .[,log_other_export_market_rev := asinh(other_export_market_rev)] %>% 
    fwrite(.,file)
}
rm(list= setdiff(ls(), base_env)); gc()


