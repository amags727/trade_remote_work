# setup -------------------------------------------------------------------
rm(list = ls()); gc();

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/Big Data Code",
    
    ## update as makes sense for CASD / your own use 
    T ~ "C:/Users/Public/Documents/Big data Project")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')




# 3a.1 export rev  --------------------------------------------------------
firm_yr_ctry = import_file(firm_yr_ctry_path)

## run regressions 
base_command = reg_command('firm_yr_ctry',
            dep_var = 'log_total_export_rev_customs',
            ind_var = paste('log_comp_data', 'log_comp_data_lag1', 'log_comp_total', 'log_comp_total_lag1',
                        'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college',
                        'log_age', sep = " + "),
            fe = "| firmid_num + year", 
            cluster = 'firmid_num') 
variations = data.frame(command =  c(base_command, 
             gsub("customs~", 'customs~log_comp_data*currently_export_customs +log_comp_data_lag1*currently_export_customs + ',base_command),
             gsub("customs~", 'customs~log_comp_data*currently_export_customs_any_ctry +log_comp_data_lag1*currently_export_customs_any_ctry + ',base_command)))
model_output = evaluate_variations(variations)[['model_output']]

## output results 
coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
               c(gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'currently export in given market')),
               c(gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'currently export in any market')))

label = "3a.1_export_rev"
format_table(model_output, label = label,
             coef_names = coef_names,
             coef_order = c(1,11,14,2,12,15,10,13,3:9),
             headers = "&\\multicolumn{3}{c}{Total Export Rev (Customs)}\\\\",
             divisions_before = 4,
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 18,
             final_commands = "table = gsub('lx', 'x',table)",
             notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
             note_width = 1,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )


# 3a.2 extensive margin-------------------------------------------------------------------------

# NOTE: I'm creating use_data_lag_1, as it did not exist in the dataset
setorder(firm_yr_ctry, firmid_num, ctry_num, year)
firm_yr_ctry[, use_data_lag1:=shift(use_data, n=1L, type="lag"), by=.(firmid_num, ctry_num)]


base_command = reg_command('firm_yr_ctry',
                           dep_var = 'log_total_export_rev_customs',
                           ind_var = paste('use_data', 'use_data_lag1', 'log_comp_total', 'log_comp_total_lag1',
                                           'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college',
                                           'log_age', sep = " + "),
                           fe = "| firmid_num + year", 
                           cluster = 'firmid_num') 
variations = data.frame(command =  c(base_command, 
                                     gsub("customs~", 'customs~use_data*currently_export_customs +use_data_lag1*currently_export_customs + ',base_command),
                                     gsub("customs~", 'customs~use_data*currently_export_customs_any_ctry +use_data_lag1*currently_export_customs_any_ctry + ',base_command)))
model_output = evaluate_variations(variations)[['model_output']]


## output results 
coef_names = c('use data', 'use data lagged', 'log total payroll', 'log total payroll lagged',
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
               c(gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'currently export in given market')),
               c(gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'currently export in any market')))

label = "3a.2_export_rev_extensive_margin"
format_table(model_output, label = label,
             coef_names = coef_names,
             coef_order = c(1,11,14,2,12,15,10,13,3:9),
             headers = "&\\multicolumn{3}{c}{Total Export Rev (Customs)}\\\\",
             divisions_before = 4,
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 18,
             final_commands = "table = gsub('lx', 'x',table)",
             notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
             note_width = 1,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )


# 3b.1 currently exporting - variance ---------------------------------------------------

## RUN REGRESSIONS 
c_exporting_controls_given = c('log_comp_data*currently_export_customs', 'log_comp_data_lag1*currently_export_customs', 'log_comp_total', 'log_comp_total_lag1',
                               'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college', 'log_age')
c_exporting_controls_any = c('log_comp_data*currently_export_customs_any_ctry', 'log_comp_data_lag1*currently_export_customs_any_ctry', 'log_comp_total', 'log_comp_total_lag1',
                             'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college', 'log_age')

# stop_exporting_controls = c(con_fil(c_exporting_controls, "lag",'age', inc = F), 'log_export_streak_age_BS')
# variance_controls = gsub('log_age', 'log_export_streak_age_BS',c_exporting_controls)

model_output = evaluate_variations(data.frame(command = c(
  # reg_command('firm_yr_ctry', dep_var = 'currently_export_customs', ind_var = paste(c_exporting_controls, collapse = "+"),
  #             fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial'), # currently_export_customs is our interaction
  # reg_command('firm_yr_ctry', dep_var = 'stop_exporting', ind_var = paste(stop_exporting_controls, collapse = "+"),
  #             fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial') %>% c(., gsub('BS', 'customs',.)), # stop_exporting not available
  reg_command('firm_yr_ctry', dep_var = 'log_export_rev_customs_cond_detrended_var', ind_var = paste(c_exporting_controls_given, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num'),
  reg_command('firm_yr_ctry', dep_var = 'log_export_rev_customs_cond_detrended_var', ind_var = paste(c_exporting_controls_any, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num')
  
)))[['model_output']]
coef_names = c('log payroll data', 'currently export in given market','lagged log payroll data',
               gpaste(c('', 'lagged '),'log payroll ', c('total'), order = 3:1),
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
               c(gpaste(c('\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'currently export in given market')),
               'currently export in any market',
               c(gpaste(c('\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'currently export in any market')))

## OUTPUT RESULTS 
headers =  "& Detrended Export Variance & Detrended Export Variance "
label = "3b.1_extensive_x_variance"
format_table(model_output, label = label,
             coef_names = coef_names,
             coef_order = c(1,11,14,3,12,15,2,13,4:10),
             # headers =headers,
             # column_names = rep(c('BS', 'customs'),2),
             # divisions_before = c(3,5),
             rescale_factor = 1,
             # custom_rows = list(""),
             custom_row_placement = 18,
             final_commands = "table = gsub('lx', 'x',table)",
             notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
             note_width = 1.1,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )


### NOTE THAT THE UNCONDITIONAL VERSION OF THE DETRENDED EXPORT VARIANCE IS SIGNIFICANT BUT ONLY IF THE FIRM IS CURRENTLY EXPORTING 
### I DIDN'T INCLUDE IT BC ITS HARDER TO EXPLAIN BUT THE CODE IS RIGHT HERE 
## (log_data + log_data*currently_export = -.028444);  (log_data_lag1 + log_data_lag1*currently_export =-0.042057)
# feols(data = firm_yr, log_total_export_rev_BS_detrended_var~log_comp_data*currently_export_BS +log_comp_data_lag1*currently_export_BS+ log_comp_total+log_comp_total_lag1+log_dom_turnover+log_dom_turnover_sq+avg_prestige_total+share_empl_college+log_age| firmid_num + year,cluster = ~firmid_num)


# 3b.2 extensive margin  -----------------------------------------------------------------------

## RUN REGRESSIONS 
c_exporting_controls_given = c('use_data*currently_export_customs', 'use_data_lag1*currently_export_customs', 'log_comp_total', 'log_comp_total_lag1',
                               'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college', 'log_age')
c_exporting_controls_any = c('use_data*currently_export_customs_any_ctry', 'use_data_lag1*currently_export_customs_any_ctry', 'log_comp_total', 'log_comp_total_lag1',
                             'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college', 'log_age')

# stop_exporting_controls = c(con_fil(c_exporting_controls, "lag",'age', inc = F), 'log_export_streak_age_BS')
# variance_controls = gsub('log_age', 'log_export_streak_age_BS',c_exporting_controls)

model_output = evaluate_variations(data.frame(command = c(
  # reg_command('firm_yr_ctry', dep_var = 'currently_export_customs', ind_var = paste(c_exporting_controls, collapse = "+"),
  #             fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial'), # currently_export_customs is our interaction
  # reg_command('firm_yr_ctry', dep_var = 'stop_exporting', ind_var = paste(stop_exporting_controls, collapse = "+"),
  #             fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial') %>% c(., gsub('BS', 'customs',.)), # stop_exporting not available
  reg_command('firm_yr_ctry', dep_var = 'log_export_rev_customs_cond_detrended_var', ind_var = paste(c_exporting_controls_given, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num'),
  reg_command('firm_yr_ctry', dep_var = 'log_export_rev_customs_cond_detrended_var', ind_var = paste(c_exporting_controls_any, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num')
  
)))[['model_output']]
coef_names = c('use data', 'currently export in given market','lagged use data',
               gpaste(c('', 'lagged '),'log payroll ', c('total'), order = 3:1),
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
               c(gpaste(c('\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'currently export in given market')),
               'currently export in any market',
               c(gpaste(c('\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'currently export in any market')))

## OUTPUT RESULTS 
headers =  "& Detrended Export Variance & Detrended Export Variance "
label = "3b.2_extensive_x_variance_extensive_margin"
format_table(model_output, label = label,
             coef_names = coef_names,
             coef_order = c(1,11,14,3,12,15,2,13,4:10),
             # headers =headers,
             # column_names = rep(c('BS', 'customs'),2),
             # divisions_before = c(3,5),
             rescale_factor = 1,
             # custom_rows = list(""),
             custom_row_placement = 18,
             final_commands = "table = gsub('lx', 'x',table)",
             notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
             note_width = 1.1,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )





# 3c  gravity ------------------------------------------


## run regressions 
grav_types<-c("extended_", "", "either_")

for(grav_type in grav_types){
  
  dep_var<-'log_total_export_rev_customs'
  base_command = reg_command('firm_yr_ctry',
                             dep_var = dep_var,
                             ind_var = paste('log_comp_data', 'log_comp_data_lag1', 'log_comp_total', 'log_comp_total_lag1',
                                             'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college',
                                             'log_age', sep = " + "),
                             fe = "| firmid_num + year", 
                             cluster = 'firmid_num') 
  interactions = gpaste(grav_type, 'grav_',c('region', 'language', 'border'))
  variations = data.frame(command =  c(base_command, 
                                       unlist(lapply(interactions, function(inter) gsub("customs~", paste0('customs~log_comp_data*', inter, ' +log_comp_data_lag1*', inter, ' + '),base_command)))))
  
  model_output = evaluate_variations(variations)[['model_output']]
  
  ## output results 
  coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
                 gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
                 c(gpaste(c('', '\\hspace{5 pt}x ',  '\\hspace{5 pt}lx '), gsub("_", "", grav_type), ' gravity ', c("region"))),
                 c(gpaste(c('', '\\hspace{5 pt}x ',  '\\hspace{5 pt}lx '), gsub("_", "", grav_type), ' gravity ', c("language"))),
                 c(gpaste(c('', '\\hspace{5 pt}x ',  '\\hspace{5 pt}lx '), gsub("_", "", grav_type), ' gravity ', c("border"))))
  
  label = paste0("3c_", grav_type, "gravity")
  format_table(model_output, label = label,
               coef_names = coef_names,
               coef_order = c(1,11,14,17,2,12,15,18,10,13,16,3:9),
               headers = "&\\multicolumn{4}{c}{Total Export Rev. (Customs)}\\\\",
               # divisions_before = 4,
               rescale_factor = 0.8,
               custom_rows = list(""),
               custom_row_placement = 18,
               final_commands = "table = gsub('lx', 'x',table)",
               notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
               note_width = 1,
               output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
  
}







