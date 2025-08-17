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
firm_yr = import_file(firm_yr_path)
firm_euro_yr = import_file(firm_euro_yr_path)
base_controls =  paste("", 'log_comp_total', 'log_comp_total_lag1', 'log_dom_turnover', 'log_dom_turnover_sq',
                       'avg_prestige_total', 'share_empl_college', "capital_intensity", 'log_age', sep = " + ")

base_command = reg_command(dataset = 'firm_yr', dep_var =  'log_total_export_rev_BS', ind_var = 'log_comp_data + log_comp_data_lag1', 
                           controls = base_controls, fe = "| firmid_num + year", cluster = 'firmid_num')

int_base_command = gsub('log_comp_data \\+ log_comp_data_lag1',
                        'log_comp_data*nace_share_export_BS + log_comp_data_lag1*nace_share_export_BS',
                        base_command)


# 2a generate variations ----------------------------------------------------------------------
baseline_rev = data.table(dep_var = 'rev', command = c(
  ## add BS versions 
  base_command,
  int_base_command, 
  base_command %>% gsub('log_age', 'log_export_streak_age_BS',.) %>% gsub('firm_yr', 'firm_yr[currently_export_BS == T]', .),
  
  ## add customs versions 
  base_command %>% gsub('BS', 'customs',.), 
  int_base_command %>% gsub('BS', 'customs',.),
  int_base_command %>% 
    gsub('log_age','log_export_streak_age_customs', .) %>% 
    gsub('firm_yr', 'firm_yr[currently_export_customs == T]',.) %>% 
    gsub('BS', 'customs', .)
)) 

baseline_currently_export = baseline_rev[c(1,4)] %>% 
  .[,command := gsub('feols', 'feglm', command)] %>% 
  .[,command := gsub(')', ", family = 'binomial')",command)] %>% 
  .[,command := gsub('log_total_export_rev', 'currently_export', command)] %>%
  .[,dep_var := 'currently_export']

baseline_death = baseline_rev[c(3,6)] %>% 
  .[,command := gsub('feols', 'feglm', command)] %>% 
  .[,command := gsub(')', ", family = 'binomial')",command)] %>% 
  .[,command := gsub('log_total_export_rev', 'stop_exporting', command)] %>%
  .[,command := gsub('\\*nace_share_export_customs', '', command)] %>% 
  .[,dep_var := 'streak_death']
  
baseline_detrended_var = baseline_rev[c(3,6)] %>% 
  .[, command := gsub('\\*nace_share_export_customs', '', command)] %>% 
  .[, command := gsub('~log', '_cond_detrended_var~log', command)] %>% 
  .[, dep_var := 'detrended_var']

## setup baseline 
variations = rbindlist(list(baseline_rev, baseline_currently_export, baseline_death, baseline_detrended_var), use.names = T, fill = T) %>% 
  as.data.table() %>% 

  ## add version with industry fe
  bind_rows(mutate(.,command = gsub("\\| firmid_num", ' | NACE_BR', command))) 
  
  ## add version breaking down euro vs. non-euro 
  variations = rbind(
    variations[, euro := FALSE],
    variations[grepl('customs', command) & !grepl('nace_share', command)] %>%
      .[, command := gsub('firm_yr', 'firm_euro_yr', command)] %>% 
      .[, command := gsub('log_comp_data \\+ log_comp_data_lag1', 'log_comp_data*euro + log_comp_data_lag1*euro', command)] %>% 
      .[, euro := TRUE]) %>% 

   ## add version at extensive margin 
  bind_rows(mutate(., command = gsub('log_comp_data', 'use_data', command))) %>%

  ## categorize
  mutate(version = ifelse(grepl("BS", command), "BS", 'customs'),
         restriction = ifelse(grepl('currently', command), 'currently_export', 'none'),
         fe = ifelse(grepl('\\| firmid_num', command), 'firm', 'industry'),
         share_interaction = grepl('nace_share', command),
         extensive_margin = grepl('use_data', command), 
         idx = 1:nrow(.)) %>% 
    select(idx,dep_var, version, restriction, fe, share_interaction, euro,extensive_margin, command)
  
model_output = evaluate_variations(variations)
if(nrow(model_output$failed_output)!= 0) print('CHECK WHAT WENT WRONG')
write_rds(model_output, paste0(raw_output_dir, '2_variations.rds'))



