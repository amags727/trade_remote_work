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


# update firm levl variance  ----------------------------------------------
vars_to_mean = c('capital', gpaste('nace_', c('entrance', 'exit'), '_rate'))
vars_to_log = c('capital')
new_meaned = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% 
  remove_if_NA(., 'year', 'comp_data') %>% .[!is.na(dom_turnover) | !is.na(total_export_rev_customs)] %>% 
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T) %>% 
  .[,c(setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean)), by = firmid] %>% 
  .[,(paste0('log_',vars_to_log)):= lapply(vars_to_log, function(x) asinh(get(x)))]

import_file(file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet')) %>%
  select(-intersect(names(.), gpaste(c('', "log_"), vars_to_mean))) %>% 
  merge(new_meaned, by = 'firmid')  %>%
  write_parquet(., file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet'))
rm(list= setdiff(ls(), base_env)); gc()


# update firm_ctry_yr variance dataset ------------------------------------
french_grav_list = import_file('1) data/similarity_matrices/outputs/similiarity_data.rds') %>% .[ctry == 'FR']
vars_to_mean = c('other_market_rev')

new_meaned = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')) %>% 
  remove_if_NA(., 'year', 'export_rev_customs', 'comp_data') %>%
  .[year %in% year_range] %>% distinct(firmid, year, ctry, .keep_all = T) %>% 
  .[,c(setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean)), by = .(firmid, ctry, streak_id)] %>% 
  .[,log_other_market_rev := asinh(other_market_rev)]

import_file(file.path(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet')) %>% 
  select(-intersect(names(.),gpaste(c('','log_'), vars_to_mean))) %>% 
  merge(new_meaned, by = c('firmid', 'streak_id', 'ctry')) %>% 
  .[,`:=`(grav_language = ctry %in% french_grav_list$share_language[[1]],
          grav_border = ctry %in% french_grav_list$share_border[[1]],
          grav_region = ctry %in% french_grav_list$share_region[[1]])] %>% 
  write_parquet(., gpaste(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet'))

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




