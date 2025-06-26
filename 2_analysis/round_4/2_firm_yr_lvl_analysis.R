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




# 2a export rev  --------------------------------------------------------
firm_yr = import_file(firm_yr_path)

## run regressions 
base_command = reg_command('firm_yr',
            dep_var = 'log_total_export_rev_BS',
            ind_var = paste('log_comp_data', 'log_comp_data_lag1', 'log_comp_total', 'log_comp_total_lag1',
                        'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college',
                        'log_age', sep = " + "),
            fe = "| firmid_num + year", 
            cluster = 'firmid_num') 
variations = data.frame(command =  c(base_command, 
             gsub("BS~", 'BS~log_comp_data*nace_share_export_BS +log_comp_data_lag1*nace_share_export_BS + ',base_command),
             gsub("log_age", "log_age + log_export_streak_age_BS", base_command)) %>% 
               c(., gsub('BS', 'customs', .)))
variations$command[6] = gsub("log_age", "log_age + log_export_streak_age_customs",variations$command[5])
model_output = evaluate_variations(variations)[['model_output']]
write_rds(model_output, paste0(raw_output_dir,'2a.1_export_rev.rds'))

## output results 
{
coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
               rep(c(gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'share industry exporting'), 'log export\nstreak year'),2))

label = "2a.1_export_rev"
format_table(model_output, label = label,
             coef_names = coef_names,
             coef_order = c(1, 11,2,12,10, 3:9,13),
             headers = "&\\multicolumn{3}{c}{Total Export Rev (Balance Sheet)}& &\\multicolumn{3}{c}{Total Export Rev (Customs)}\\\\",
             divisions_before = 4,
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 18,
             final_commands = "table = gsub('lx', 'x',table)",
             notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
             note_width = 1,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
}
rm(list= setdiff(ls(), c(base_env))); gc()


# 2a.2 extensive margin-------------------------------------------------------------------------

firm_yr = import_file(firm_yr_path)
base_command = reg_command('firm_yr',
                           dep_var = 'log_total_export_rev_BS',
                           ind_var = paste('use_data', 'use_data_lag1', 'log_comp_total', 'log_comp_total_lag1',
                                           'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college',
                                           'log_age', sep = " + "),
                           fe = "| firmid_num + year", 
                           cluster = 'firmid_num') 
variations = data.frame(command =  c(base_command, 
                                     gsub("BS~", 'BS~use_data*nace_share_export_BS +use_data_lag1*nace_share_export_BS + ',base_command),
                                     gsub("log_age", "log_age + log_export_streak_age_BS", base_command)) %>% 
                          c(., gsub('BS', 'customs', .)))
variations$command[6] = gsub("log_age", "log_age + log_export_streak_age_customs",variations$command[5])
model_output = evaluate_variations(variations)[['model_output']]
write_rds(model_output, paste0(raw_output_dir,'2a.2_export_rev_extensive_margin.rds'))

## output results 
{
coef_names = c('use data', 'use data lagged', 'log total payroll', 'log total payroll lagged',
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
               rep(c(gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'share industry exporting'), 'log export\nstreak year'),2))

label = "2a.2_export_rev_extensive_margin"
format_table(model_output, label = label,
             coef_names = coef_names,
             coef_order = c(1, 11,2,12,10, 3:9,13),
             headers = "&\\multicolumn{3}{c}{Total Export Rev (Balance Sheet)}& &\\multicolumn{3}{c}{Total Export Rev (Customs)}\\\\",
             divisions_before = 4,
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 18,
             final_commands = "table = gsub('lx', 'x',table)",
             notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
             note_width = 1,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
}

rm(list= setdiff(ls(), c(base_env))); gc()
# 2b currently exporting/about to exit / variance -----------------------------------------------------------------------
firm_yr = import_file(firm_yr_path)

## RUN REGRESSIONS 
c_exporting_controls = c('log_comp_data', 'log_comp_data_lag1', 'log_comp_total', 'log_comp_total_lag1',
                     'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college', 'log_age')
stop_exporting_controls = c(con_fil(c_exporting_controls, "lag",'age', inc = F), 'log_export_streak_age_BS')
variance_controls = gsub('log_age', 'log_export_streak_age_BS',c_exporting_controls)

model_output = evaluate_variations(data.frame(command = c(
  reg_command('firm_yr', dep_var = 'currently_export_BS', ind_var = paste(c_exporting_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial') %>% c(., gsub('BS', 'customs',.)),
  reg_command('firm_yr', dep_var = 'stop_exporting_BS', ind_var = paste(stop_exporting_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial') %>% c(., gsub('BS', 'customs',.)),
  reg_command('firm_yr', dep_var = 'log_total_export_rev_BS_cond_detrended_var', ind_var = paste(variance_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num') %>% c(., gsub('BS', 'customs',.))
)))[['model_output']]
write_rds(model_output, paste0(raw_output_dir,'2b.1_extensive_x_variance.rds'))



## OUTPUT RESULTS 
{
coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
               rep('log export\nstreak year',2))
headers =  paste(c("&",gpaste('\\multicolumn{2}{c}{', c('P(currently exporting)', 'P(end export streak)', 'Detrended Export Variance'),'}', collapse_str = '&&'), '\\\\'), collapse = "")
label = "2b.1_extensive_x_variance"
format_table(model_output, label = label,
             coef_names = coef_names,
             headers =headers,
             column_names = rep(c('BS', 'customs'),3),
             divisions_before = c(3,5),
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 12,
             notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
             note_width = 1.1,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
}

### NOTE THAT THE UNCONDITIONAL VERSION OF THE DETRENDED EXPORT VARIANCE IS SIGNIFICANT BUT ONLY IF THE FIRM IS CURRENTLY EXPORTING 
### I DIDN'T INCLUDE IT BC ITS HARDER TO EXPLAIN BUT THE CODE IS RIGHT HERE 
## (log_data + log_data*currently_export = -.028444);  (log_data_lag1 + log_data_lag1*currently_export =-0.042057)
# feols(data = firm_yr, log_total_export_rev_BS_detrended_var~log_comp_data*currently_export_BS +log_comp_data_lag1*currently_export_BS+ log_comp_total+log_comp_total_lag1+log_dom_turnover+log_dom_turnover_sq+avg_prestige_total+share_empl_college+log_age| firmid_num + year,cluster = ~firmid_num)
rm(list= setdiff(ls(), c(base_env))); gc()






# 2b.2 extensive margin ---------------------------------------------------
firm_yr = import_file(firm_yr_path)

## RUN REGRESSIONS 
c_exporting_controls = c('use_data', 'use_data_lag1', 'log_comp_total', 'log_comp_total_lag1',
                         'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college', 'log_age')
stop_exporting_controls = c(con_fil(c_exporting_controls, "lag",'age', inc = F), 'log_export_streak_age_BS')
variance_controls = gsub('log_age', 'log_export_streak_age_BS',c_exporting_controls)

model_output = evaluate_variations(data.frame(command = c(
  reg_command('firm_yr', dep_var = 'currently_export_BS', ind_var = paste(c_exporting_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial') %>% c(., gsub('BS', 'customs',.)),
  reg_command('firm_yr', dep_var = 'stop_exporting_BS', ind_var = paste(stop_exporting_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial') %>% c(., gsub('BS', 'customs',.)),
  reg_command('firm_yr', dep_var = 'log_total_export_rev_BS_cond_detrended_var', ind_var = paste(variance_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num') %>% c(., gsub('BS', 'customs',.))
)))[['model_output']]
write_rds(model_output, paste0(raw_output_dir,'2b.2_extensive_x_variance_extensive_margin'))



## OUTPUT RESULTS 
{
coef_names = c('use data', 'use data lagged', 'log total payroll', 'log total payroll lagged',
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
               rep('log export\nstreak year',2))
headers =  paste(c("&",gpaste('\\multicolumn{2}{c}{', c('P(currently exporting)', 'P(end export streak)', 'Detrended Export Variance'),'}', collapse_str = '&&'), '\\\\'), collapse = "")
label = "2b.2_extensive_x_variance_extensive_margin"
format_table(model_output, label = label,
             coef_names = coef_names,
             headers =headers,
             column_names = rep(c('BS', 'customs'),3),
             divisions_before = c(3,5),
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 12,
             notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
             note_width = 1.1,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
}

rm(list= setdiff(ls(), c(base_env))); gc()






# 2b.3 redo 2b.1 with interactions in customs  --------------------------------
firm_yr = import_file(firm_yr_path)

## RUN REGRESSIONS 
c_exporting_controls = c('log_comp_data*nace_share_export_BS', 'log_comp_data_lag1*nace_share_export_BS', 'log_comp_total', 'log_comp_total_lag1',
                         'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college', 'log_age')
stop_exporting_controls = c(con_fil(c_exporting_controls, "lag",'age', inc = F), 'log_export_streak_age_BS')
variance_controls = gsub('log_age', 'log_export_streak_age_BS',c_exporting_controls)

model_output = evaluate_variations(data.frame(command = c(
  reg_command('firm_yr', dep_var = 'currently_export_customs', ind_var = paste(c_exporting_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial'),
  reg_command('firm_yr', dep_var = 'stop_exporting_customs', ind_var = paste(stop_exporting_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num',family = 'binomial'),
  reg_command('firm_yr', dep_var = 'log_total_export_rev_customs_cond_detrended_var', ind_var = paste(variance_controls, collapse = "+"),
              fe = "| firmid_num + year", cluster = 'firmid_num') 
)))[['model_output']]
write_rds(model_output, paste0(raw_output_dir,'2b.3_extensive_x_variance_with_interactions'))

rm(list= setdiff(ls(), c(base_env))); gc()
# 2c iv first stage analysis  ---------------------------------------------
firm_region_yr = import_file(linkedin_firm_yr_region_path)
model_output = evaluate_variations(data.frame(command = c(
  'feols(data = firm_region_yr, log_comp_data ~ log_data_grads)',
  'feols(data = firm_region_yr,log_comp_data ~ log_nace_non_nut_comp_data + log_non_nace_nut_comp_data)'
  )))[['model_output']]
write_rds(model_output, paste0(raw_output_dir,'2c_instrument_zero_stage.rds'))

rm(list= setdiff(ls(), c(base_env))); gc()
# 2d redo export rev regressions with instrument --------------------------
firm_yr = import_file(firm_yr_path)

base_command = reg_command('firm_yr',
            dep_var = 'log_total_export_rev_BS',
            ind_var = 'log_comp_data+log_comp_data_lag1',
            controls = paste("",'log_comp_total', 'log_comp_total_lag1',
                        'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college',
                        'log_age', sep = " + "),
            fe = "| firmid_num + year",
            iv = 'log_grad_predicted_comp_data + log_grad_predicted_comp_data_lag1',
            cluster = 'firmid_num') 
model_output = evaluate_variations(data.frame(command = c(
  base_command,
  gsub("BS", 'customs', base_command),
  gsub("BS", 'customs', base_command) %>% gsub('_data ', '_data*nace_share_export_BS',.) %>%
    gsub('ta_lag1', 'ta_lag1*nace_share_export_BS',.)) %>% c(., gsub('grad_', 'leave_out_',.)
)))[['model_output']]
write_rds(model_output, paste0(raw_output_dir,'2d_export_rev_IV'))

rm(list= setdiff(ls(), c(base_env))); gc()
