# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest', 'kableExtra', 'survival')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')}
source("2) code/00_helper_functions.R")
output_dir = '3) output/4) data_summary_stats'
inputs_dir = ('1) data/16_inputs_for_data_summary_stats')
base_env = c(ls(),'base_env')


# h -----------------------------------------------------------------------
full_output = import_file(file.path(output_dir, 'full_output.rds'))
variation_output = full_output$variation_output
model_output = full_output$model_output
failed_output = full_output$failed_output %>% filter(!grepl('de_trended', reason))

# block 1  ---------------------------------------------------------
growth_suffixes = c("_growth_rate") #, "_growth_rate_lead1", "_growth_rate_2yr")

block_1_dep = c('log_turnover', paste0('turnover',growth_suffixes),
                paste0('log_',c('empl','age',paste0('cost_per_worker', c('_bs','_linkedin',"_linkedin_fr")))))

block_1_ind = gpaste(c("quartile", "quartile_share", "log", "share"), "_comp_data")

block_1 = expand(block_1_dep,block_1_ind,c("", " + log_age"), names = c('dep_var', 'ind_var', "controls"),
                 order = c(3,2,1)) %>% filter(!(dep_var == 'log_age' & controls == 'log_age')) 


for(ind_variable in block_1_ind){
  label = paste0('block_1_', ind_variable)
  counters = variation_output[
    block == 1 & dep_var %in% block_1_dep 
    & (dep_var == 'log_age' | controls == ' + log_age')
    & !(dep_var == 'log_age' & controls == ' + log_age')
    &  ind_var == ind_variable] %>%
    pull(counter) %>% unique() 
  
  model_inputs = full_output$model_output[counters]
  col_titles = unique(variation_output[counter %in% counters][['dep_var']]) %>% 
    gsub('_','\\\\_',.)
    
  format_table(model_inputs, label, column_names = col_titles, caption = label %>% gsub('_',' ',.),
               output_path = file.path(output_dir, paste0(label, '.tex')), rescale_factor = 1)
}


# block 2 -----------------------------------------------------------------
block_2_dep =  gpaste(order = c(4,3,2,1),c('','de_trended_'),"variance_",c('',"log_"),
                      c('turnover', 'dom_turnover', 'value_bs_export', 'value_customs_export'))
block_2_ind =  gpaste(c(gpaste(c('max_', 'median_'),c("quartile", "quartile_share"),order = c(2,1)),
                        "mean_share", 'mean_log'), "_comp_data")    
block_2 = expand(block_2_dep, block_2_ind, c("", " + log_min_sample_age + log_sample_years_observed"),
                 names = c('dep_var', 'ind_var', 'controls'), order = c(3,2,1)) %>% 
  mutate(block = 2, dataset = 'firm_lvl', fe = '| NACE_BR', cluster = "NACE_BR", 
         across('controls', ~ifelse(grepl('export',dep_var) &.!="", paste0(., " + log_years_exported"), .)),
         command = feols_command(dataset, dep_var,ind_var, controls, fe, cluster))

for(ind_variable in block_2_ind){
  label = paste0('block_2_', ind_variable)
  counters = variation_output[block == 2 & controls != "" &  ind_var == ind_variable & 
                              grepl('log', dep_var)] %>%
    pull(counter) %>% unique()
  model_inputs = full_output$model_output[counters]
  col_titles = unique(variation_output[counter %in% counters][['dep_var']]) %>% 
    gsub('_','\\\\_',.)
  format_table(model_inputs, label, column_names = col_titles, caption = label %>% gsub('_',' ',.),
               output_path = file.path(output_dir, paste0(label, '.tex')), rescale_factor = 1)
}


# block 3 -----------------------------------------------------------------
c("quartile_comp_data", "quartile_share_comp_data")

ind_variable = "quartile_share_comp_data"
dep_var = c('currently_export', "market_entry_failure_rate_export")

counters = variation_output[block == 1 & dep_var %in% c('intermarket_hhi_export') &
                            ind_var == ind_variable] %>% pull(counter) %>% unique()

model_output[counters]
for(ind_variable in block_2_ind){
  
  
}
# block 8 -----------------------------------------------------------------
block_8 = expand(block_1_ind, c('+log_age', '+log_age + log_dom_turnover') %>% c(., gsub("\\+log", "*log",.)),
                 names = c('ind_var', 'controls')) %>%
  mutate(dep_var = 'is_first_export_year', block = 9, dataset = "firm_yr_lvl[year <= first_export_year] ",
         fe = "| NACE_BR + year", cluster ='firmid',
         command = flogit_command(dataset, dep_var,ind_var, controls, fe, cluster)) 

counters = variation_output[block == 8 ] 
  &  ind_var == ind_variable] %>%
  pull(counter) %>% unique() 









