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


#import data / set parameters  -----------------------------------------------------------------------
firm_yr = import_file(firm_yr_path) %>% mutate(across(con_fil(., 'bracket', 'quartile'), ~as.factor(.)))

base_controls =  paste("", 'log_comp_total', 'log_comp_total_lag1', 'log_dom_turnover', 'log_dom_turnover_sq',
                       'avg_prestige_total', 'share_empl_college', "capital_intensity", 'log_age', sep = " + ")

rev_command = reg_command(dataset = 'firm_yr', dep_var =  'log_total_export_rev_BS', ind_var = 'log_comp_data + log_comp_data_lag1', 
                           controls = base_controls, fe = "| firmid_num + year", cluster = 'firmid_num')

int_rev_command = gsub('log_comp_data \\+ log_comp_data_lag1','log_comp_data*interaction + log_comp_data_lag1*interaction', rev_command)

entry_command = reg_command(dataset = 'firm_yr', dep_var = 'currently_export_BS', 'log_comp_data + log_comp_data_lag1', 
                            controls = base_controls, fe = "| firmid_num + year", cluster = 'firmid_num', family = 'binomial')

int_entry_command = gsub('log_comp_data \\+ log_comp_data_lag1','log_comp_data*interaction + log_comp_data_lag1*interaction', entry_command)

# 2 generate variations ----------------------------------------------------------------------
interactions = c('log_dom_turnover_bar',con_fil(firm_yr, 'prior'))
baseline_regs = data.frame(command = c(rev_command, entry_command))

size_quartile_regs = data.table(
  command = rep(c(rev_command,rev_command, entry_command), each = 4), quartile_num = rep(1:4,3),
  intensive_margin = rep(c(F,T,F), each =4)) %>% rowwise() %>% 
  mutate(command = gsub('firm_yr', paste0('firm_yr[log_dom_turnover_bar_quartile==', quartile_num,']'), command)) %>%
  as.data.table() %>% 
  .[intensive_margin == T, command := gsub('firm_yr\\[', 'firm_yr[currently_export_BS == T &', command)]

interaction_regs = data.table(
  command = rep(c(int_rev_command, int_entry_command), each = length(interactions)),
  interaction_var = rep(interactions,2)) %>% rowwise() %>% 
  mutate(command = gsub('interaction', interaction_var, command))

variations = rbindlist(list(baseline_regs, size_quartile_regs, interaction_regs), fill = T, use.names = T) %>% 
  bind_rows(mutate(., command = gsub('BS', 'customs',command))) %>% mutate(version = if_else(grepl('BS', command), 'BS', 'customs'))

if (!dummy_version){  
  model_output = evaluate_variations(variations)
  if(nrow(model_output$failed_output)!= 0) print('CHECK WHAT WENT WRONG')
  write_rds(model_output, paste0(raw_output_dir, '2c_firm_yr_supp_variations.rds'))
}






