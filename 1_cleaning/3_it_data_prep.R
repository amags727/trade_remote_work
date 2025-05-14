# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')

# Generate component datasets ----------------------------------------------------------------------

# make linkedin vars complete ---------------------------------------------
if(make_linkedin_vars_complete) source('2) code/1_cleaning/3_it_data_preperation/3a_alpha_complete_linkedin_vars.R')
# make randomized ---------------------------------------------
if(make_randomized) source('2) code/1_cleaning/3_it_data_preperation/3a_randomize_linkedin.R')
# make birth ---------------------------------------------
if(make_birth_data) source('2) code/1_cleaning/3_it_data_preperation/3b_make_firm_age.R')
# make firm yr ---------------------------------------------
if(make_firm_yr) source('2) code/1_cleaning/3_it_data_preperation/3c_make_firm_yr_lvl.R')
# make firm ctry yr ---------------------------------------------
if(make_firm_ctry_yr) source('2) code/1_cleaning/3_it_data_preperation/3d_make_firm_ctry_yr_lvl.R')
# make variance ---------------------------------------------
if(make_variance) source('2) code/1_cleaning/3_it_data_preperation/3e_make_firm_variance.R')
# make country entrance ---------------------------------------------------
if(make_ctry_entrance) source('2) code/1_cleaning/3_it_data_preperation/3f_make_ctry_entrance.R')


