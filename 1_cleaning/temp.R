# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')


# h -----------------------------------------------------------------------
processed_linkedin = '1) data/15_revelio_data/1_inputs/b_processed_data/linkedin/'
processed_admin = '1) data/15_revelio_data/1_inputs/b_processed_data/admin/'
output_dir = '1) data/15_revelio_outputs/2_outputs/'

matching_output = import_file(paste0(processed_admin, 'fuzzy_matching_output_final.parquet'))



[['rcid', 'siren']].rename(columns={'siren':'firmid'})








