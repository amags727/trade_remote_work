# Setup -------------------------------------------------------------------
rm(list = ls());

setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')}
source('2) code/0_set_parameter_values.R')

# Generate component datasets ----------------------------------------------------------------------
if(make_linkedin_vars_complete) source('2) code/1_cleaning/3_it_data_preperation/3a_alpha_complete_linkedin_vars.R')
if(make_randomized) source('2) code/1_cleaning/3_it_data_preperation/3a_randomize_linkedin.R')
if(make_birth_data) source('2) code/1_cleaning/3_it_data_preperation/3b_make_firm_age.R')
if(make_firm_yr) source('2) code/1_cleaning/3_it_data_preperation/3c_make_firm_yr_lvl.R')
if(make_firm_ctry_yr) source('2) code/1_cleaning/3_it_data_preperation/3d_make_firm_ctry_yr_lvl.R')
if(make_variance) source('2) code/1_cleaning/3_it_data_preperation/3e_make_firm_variance.R')
if(make_ctry_entrance) source('2) code/1_cleaning/3_it_data_preperation/3f_make_ctry_entrance.R')


