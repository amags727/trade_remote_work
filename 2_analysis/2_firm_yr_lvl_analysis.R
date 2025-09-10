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
  
if (!dummy_version){  
  model_output = evaluate_variations(variations)
  if(nrow(model_output$failed_output)!= 0) print('CHECK WHAT WENT WRONG')
  write_rds(model_output, paste0(raw_output_dir, '2_firm_yr_variations.rds'))
}
# Output Revenue Results ---------------------------------------------------
analysis_output = import_file(de_dummy(raw_output_dir), '2_firm_yr_variations.rds')
model_output =  analysis_output$model_output; variation_output = analysis_output$variation_output 

## Generate Revenue Tables   
coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age', "log capital intensity",
               rep(c(gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'share industry exporting'), 'log export\nstreak year'),2))
rev_labels = c('2a.1_rev_firm_FE','2a.2_rev_ind_FE', '2a.3_rev_firm_FE_ext', '2a.4_rev_ind_FE_ext') 
rev_reg_list = list(1:6, 13:18, 33:38, 45:50)
notes_list = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE." %>%
  c(., gsub('firm and', 'industry and', .)) %>% rep(.,2)
final_commands_list = "table = gsub('lx', 'x',table)" %>% c(., paste0(., " %>% gsub('log payroll', 'use', .)")) %>% rep(.,each = 2)
for (i in 1:4){
format_table(
  model_output[rev_reg_list[[i]]] , label = rev_labels[i],
  coef_names = coef_names,
  coef_order = c(1, 12,2,13,11, 3:10,14),
  headers = "&\\multicolumn{3}{c}{Total Export Rev (Balance Sheet)}& &\\multicolumn{3}{c}{Total Export Rev (Customs)}\\\\",
  divisions_before = 4,
  rescale_factor = 1,
  custom_rows = list(""),
  custom_row_placement = 18,
  final_commands =final_commands_list[i],
  notes = notes_list[i],
  note_width = 1,
  output_path =  paste0(de_dummy(finished_output_dir),  rev_labels[i], '.tex'), make_tex = F )
}

### Generate supplementary regression tables 
supp_coef_names = coef_names[-c(11:13, 15:17)]
supp_reg_list = list(
  variations[dep_var != 'rev' & fe == 'firm' & extensive_margin == F & euro == F][['idx']],
  variations[dep_var != 'rev' & fe == 'industry' & extensive_margin == F & euro == F][['idx']],
  variations[dep_var != 'rev' & fe == 'firm' & extensive_margin == T & euro == F][['idx']],
  variations[dep_var != 'rev' & fe == 'industry' & extensive_margin == T & euro == F][['idx']])
supp_labels = gsub('2a', '2b', rev_labels) %>% gsub('rev', 'supp', . )
supp_final_commands_list = rep(c('', "table = gsub('log payroll', 'use', table)"), each = 2)

for (i in 1:4){
format_table(model_output[supp_reg_list[[i]]], label = supp_labels[i],
             coef_names = supp_coef_names,
             headers = make_headers(2,  c('P(currently exporting)', 'P(end export streak)', 'Detrended Export Variance')),
             column_names = rep(c('BS', 'customs'),3),
             divisions_before = c(3,5),
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 12,
             notes = notes_list[i],
             note_width = 1.1,
             final_commands =  supp_final_commands_list[i],
             output_path =  paste0(de_dummy(finished_output_dir), supp_labels[i], '.tex'), make_tex = F )
}





