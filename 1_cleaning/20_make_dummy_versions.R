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

## import helper functions 
source('2) code/0_set_parameter_values.R')

# 3_bs_br_data -------------------------------------------------------------------------
base = import_file(raw_bs_br_path) %>% stratified_sample(., c('empl_bucket', 'NACE_BR', 'year'),.01)
base_dummy = gen_dummy_dataset(base, 
                  subset_vars =  c('empl_bucket'),
                  discrete_vars = c('empl', 'NACE_BR','year'),
                  id_vars = 'firmid_num',
                  rounding_vars = NA,
                  dont_repeat_ids_within = 'year')
write_parquet(base_dummy, paste0(dummy_data_dir,'3_bs_br_data.parquet'))


# 4 ofats data -------------------------------------------------------------------------
file_path = raw_ofats_path
base = import_file(raw_ofats_path) %>% .[,name := 'x'] %>% stratified_sample(., c('ctryofats', 'year'),.01)
base_dummy = gen_dummy_dataset(base, 
                               subset_vars = c('ctryofats', 'year'),
                               discrete_vars = 'name',
                               id_vars = 'firmid_num',
                               dont_repeat_ids_within = c('ctryofats', 'year'))

write_parquet(base_dummy, gsub('1) data/', dummy_data_dir, file_path))


# 5 customs data product level -----------------------------------------------------------------------
file_path = raw_customs_product_lvl_path
subset_vars = c('year', 'ctry_num',  'exim')
base = import_file(file_path) %>% stratified_sample(., subset_vars,.001)
base_dummy = gen_dummy_dataset(
  base, 
  subset_vars,
  discrete_vars = "CN8plus",
  id_vars = 'firmid_num',
  dont_repeat_ids_within = c('ctry_num', 'year','CN8plus'))
write_parquet(base_dummy, gsub('1) data/', dummy_data_dir, file_path))

rm(list= setdiff(ls(), c(base_env))); gc()

# 6 customs data firm level -----------------------------------------------------------------------
file_path = raw_customs_firm_lvl_path
subset_vars = c('year', 'ctry_num',  'exim')
base = import_file(file_path) %>% stratified_sample(., subset_vars,.001)
base_dummy = gen_dummy_dataset(
  base, 
  subset_vars,
  discrete_vars = 'streak_id',
  id_vars = 'firmid_num',
  dont_repeat_ids_within = c('ctry_num', 'year'))
write_parquet(base_dummy, gsub('1) data/', dummy_data_dir, file_path))
rm(list= setdiff(ls(), c(base_env))); gc()



# 12 firm ctry year -------------------------------------------------------
file_path = '1) data/12_firm_yr_ctry_lvl_dta.parquet'

base = import_file(file_path)
base = rbind(stratified_sample(base[currently_export_customs ==F], 'use_data', .0000001),
             stratified_sample(base[currently_export_customs == T], 'use_data', .001))
gc()

subset_vars = c('use_data', 'currently_export_customs')
id_vars = con_fil(base, 'id')
discrete_vars = c(names(base)[sapply(base,is.logical)],
                  con_fil(con_fil(base, 'quartile', 'year'),'log', 'id', inc = F),
                  'ctry_num', "NACE_BR", 'num_mkts','last_observed', 'streak_start', 'streak_end')
base_dummy = gen_dummy_dataset(base, subset_vars, discrete_vars, id_vars, dont_repeat_ids_within = c('year','ctry_num'))
write_parquet(base_dummy, gsub('1) data/', dummy_data_dir, file_path))


