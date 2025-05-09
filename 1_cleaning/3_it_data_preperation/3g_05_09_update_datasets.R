#basic setup 
rm(list = ls());
setwd('../../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../../..')}
source('2) code/0_set_parameter_values.R')


# update firm_yr_lvl ---------------------------------------------------------------
d_vars = c("comp_data", "share_comp_data")
base = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% select(- con_fil(., 'quartile')) %>% 
  
  # generate comparison vars 
  .[, (gpaste(d_vars, '_nace_quartile')) := lapply(d_vars, function(x) as.factor(ntile(get(x), 4))), by = .(NACE_BR, year)] %>%
  .[, (gpaste(d_vars,'_nace_pct_rank')) := lapply(d_vars, function(x) percent_rank(get(x))), by = .(NACE_BR, year)] %>%
  .[, (gpaste(d_vars,'_nace_sd_from_mean')) := lapply(d_vars, function(x)(get(x)- NA_mean(get(x)))/ NA_sd(get(x))), by = .(NACE_BR, year)] %>%
  
  # generate comparison vars (age)
  .[, (gpaste(d_vars, '_nace_quartile_age')) := lapply(d_vars, function(x) as.factor(ntile(get(x), 4))), by = .(NACE_BR, year,young)] %>%
  .[, (gpaste(d_vars,'_nace_pct_rank_age')) := lapply(d_vars, function(x) percent_rank(get(x))), by = .(NACE_BR, year,young)] %>%
  .[, (gpaste(d_vars,'_nace_sd_from_mean_age')) := lapply(d_vars, function(x)(get(x)- NA_mean(get(x)))/ NA_sd(get(x))), by = .(NACE_BR, year,young)] 

write_parquet(base,file.path(inputs_dir, '16c_firm_yr_lvl.parquet'))


rm(list= setdiff(ls(), base_env)); gc()
 



   

