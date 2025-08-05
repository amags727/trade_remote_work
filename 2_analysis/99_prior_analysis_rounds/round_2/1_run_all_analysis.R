#basic setup 
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))};
setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')}
source('2) code/0_set_parameter_values.R')
dir.create('3) output/0_raw_output')

# run analysis  -----------------------------------------------------------
source('2) code/2_analysis/2_firm_yr_lvl_analysis.R')
source('2) code/2_analysis/3_firm_ctry_yr_lvl_analysis.R')
source('2) code/2_analysis/4_ctry_entry_analysis.R')
source('2) code/2_analysis/5_total_rev_variance.R')
source('2) code/2_analysis/6_mkt_export_rev_variance.R')
source('2) code/2_analysis/7_balance_test.R')
