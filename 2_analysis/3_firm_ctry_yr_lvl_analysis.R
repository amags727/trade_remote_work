##### DESCRIPTION -------------------------------------------------------------------
# The goal here is to firstfigure out what our baseline specification
# (all obs, only currently epxorting, only currently exporting to market)
# should be and then once we figure that out, use that specification for the rest of the variaition analysis 

# setup -------------------------------------------------------------------
rm(list = ls()); gc();

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
# set parameter values  ---------------------------------------------------
base_controls =  paste("", 'log_comp_total', 'log_comp_total_lag1', 'log_dom_turnover', 'log_dom_turnover_sq',
                       'avg_prestige_total', 'share_empl_college', "capital_intensity", 'ctry_pop_among_exporters', 'log_age', sep = " + ")

base_command = reg_command(dataset = 'firm_yr_ctry', dep_var =  'log_export_rev_customs', ind_var = 'log_comp_data + log_comp_data_lag1', 
                           controls = base_controls, fe = "| firmid_num + year + ctry_num", cluster = 'firmid_num')
restrictions = c('', '[currently_export_customs_any_ctry == T]', '[currently_export_customs== T]')


firm_yr_ctry = import_file(firm_yr_ctry_path)

# 3a test different base specifications --------------------------------------------------------
rev_variations = data.table(restriction = restrictions) %>% rowwise() %>%
  mutate(command = gsub('firm_yr_ctry', paste0('firm_yr_ctry', restriction), base_command)) %>% 
  mutate(dep_var = 'rev')

currently_export_variations = data.table(restriction = restrictions[1:2]) %>% rowwise() %>% 
  mutate(command = gsub('firm_yr_ctry', paste0('firm_yr_ctry', restriction), base_command)) %>% mutate(command = 
   gsub('feols', 'feglm', command) %>% gsub(')', ", family = 'binomial')",.) %>%  # update the regression type 
   gsub('log_export_rev_customs', 'currently_export_customs',.)) %>% # update the dep var 
  mutate(dep_var = 'currently_export')

streak_death_variations = data.table(command = base_command %>% 
   gsub('feols', 'feglm', .) %>% gsub(')', ", family = 'binomial')",.) %>%  # update the regression type 
   gsub('log_export_rev_customs', 'is_streak_death',.) %>% # update the dep var
   gsub('\\| firmid', '+ log_years_since_streak_start | firmid',. )) %>% # update controls 
  mutate(dep_var = 'streak_death')

detrended_var_variations = data.table(command = base_command %>% 
  gsub('log_export_rev_customs', 'log_export_rev_customs_cond_detrended_var',.) %>% # update the dep var                            
  gsub('\\| firmid', '+ log_years_since_streak_start | firmid',. )) %>% # update controls 
  mutate(dep_var = 'detrended_var')

variations = rbindlist(list(rev_variations, currently_export_variations, streak_death_variations, detrended_var_variations), use.names = T, fill = T)  %>% 
  rbind(.,.) %>% mutate(intensive_margin = rep(c(F,T), each = nrow(.)/2)) %>% as.data.table() %>% 
  .[intensive_margin == T, command := gsub('log_comp_data', 'use_data', command)] %>%
  select(intensive_margin, dep_var, restriction, everything())

model_output = evaluate_variations(variations)
if (nrow(model_output$failed_output) >0) print('CHECK WHAT WENT WRONG WITH REGRESSIONS')
write_rds(model_output$model_output,paste0(raw_output_dir, "3a_ctry_lvl_baseline_analysis.RDS"))

### WHEN WE FIGURE OUT OUR PREFERRED SPECIFICATION WE WILL SELECT 
### ROWS FROM VARIATIONS TO FORM OUR BASE FOR REST OF ANALYSIS 
### THIS IS JUST A PLACEHOLDER 
base_variations = variations[c(1,4,6,7) %>% c(., 7+.),] %>% .[,idx := .I]

# 3b variation analysis  --------------------------------------------------
interaction_variations = rbindlist(lapply(1:nrow(interaction_vars_and_terms),function(i){
  t_variations = base_variations
  interaction = interaction_vars_and_terms$var[i]; interaction_name = interaction_vars_and_terms$term[i]
  ind_var_options = gpaste(c('log_comp_data', 'use_data'), c('_lag1', ''))
  for (option in ind_var_options){
    t_variations = t_variations %>% mutate(
      command = gsub(paste0("(?<!_lag1)\\b", option, "\\b"),paste0(option, "*", interaction),command,perl = TRUE))
  }
  t_variations = t_variations %>% mutate(interaction = interaction_name)
}))
interaction_variations = rbindlist(list(base_variations, interaction_variations), use.names = T, fill =T)
model_output = evaluate_variations(interaction_variations)
if (nrow(model_output$failed_output) >0) print('CHECK WHAT WENT WRONG WITH REGRESSIONS')
write_rds(model_output$model_output,paste0(raw_output_dir,"3b_ctry_lvl_interaction_analysis.RDS"))



