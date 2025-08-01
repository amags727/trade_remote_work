## IMPORT PACKAGES AND HELPER FUNCTIONS 
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest', 'countrycode', 'survival', 'knitr', 'parallel', 'patchwork', 'scales', 'duckdb', 
             'truncnorm','sf', 'rnaturalearth', 'geosphere', 'giscoR', 'googledrive')
lapply(packages, function(package){tryCatch({library(package,character.only = T)}, error = function(cond){
  install.packages(package); library(package, character.only = T)
})})
source('2) code/00_helper_functions.R')



# key path names  ---------------------------------------------------------
raw_dir = 'C:/Users/Public/1. Microprod/0. Raw data processing/Data/'
similiarity_dir = '1) data/0_misc_data/0c_similarity_matrices/'

# paths for key dataset organized by the cleaning file in which they are created; I do this to better manage the 
# need to use dummy datasets outside the casd environment 
dummy_version = getwd() != "C:/Users/Public/Documents/Big data Project"; 

  # 1 import raw admin data 
  raw_bs_br_path = '1) data/3_bs_br_data.parquet'
  raw_ofats_path =  '1) data/4_OFATS.parquet'
  raw_customs_product_lvl_path = '1) data/5_customs_product_level.parquet'
  raw_customs_firm_lvl_path = '1) data/6_customs_firm_level.parquet'
  
  # 3 make firm age 
  firm_lvl_birth_path = '1) data/9_age_data/9b_firm_lvl_birth_data.parquet'
  firm_ctry_lvl_birth_path = '1) data/9_age_data/9c_firm_ctry_lvl_birth_data.parquet'
  firm_lvl_export_birth_path = '1) data/9_age_data/9d_firm_lvl_export_streak_info.parquet'
  
  # 4 process linkedin 
  linkedin_firm_path = '1) data/7_revelio_data/c_final_outputs/7c1_linkedin_firm_lvl.parquet'
  linkedin_firm_yr_path = '1) data/7_revelio_data/c_final_outputs/7c3_linkedin_firm_yr_lvl.parquet'
  linkedin_firm_yr_region_path = '1) data/7_revelio_data/c_final_outputs/7c4_linkedin_firm_yr_region_lvl.parquet'
  
  # 5 make firm yr data 
  firm_yr_path = '1) data/10_firm_yr_lvl_dta.parquet'
  firm_yr_summary_stats_path = '1) data/11_firm_yr_summary_stats_input.parquet'
  
  #6 make firm ctry yr data 
  extended_grav_path = '1) data/0_misc_data/0d_all_countries_all_years_all_firms_grav_data.parquet'
  firm_yr_ctry_path = '1) data/12_firm_yr_ctry_lvl_dta.parquet'
  
  # If we're working offline use / make dummy versions 
  if (dummy_version){for (path in con_fil(ls(), 'path')){assign(path, gsub('1) data', '1a) dummy data', get(path)))}}
  
  
  analysis_round = 4
  output_base = paste0('3) output/',letters[analysis_round],"_round_",analysis_round,"_analysis/")
  suppressWarnings(dir.create(output_base))
  raw_output_dir = paste0(output_base,letters[analysis_round],"1_raw_output/")
  finished_output_dir = paste0(output_base,letters[analysis_round],"2_finished_tables/")
  dummy_data_dir = ifelse(getwd() == 'C:/Users/Public/Documents/Big data Project',
                          paste0(output_base, letters[analysis_round], '3_dummy_data/'),
                          '1a) dummy data')
  
  if (dummy_version){ for (path in c('output_base','raw_output_dir', 'finished_output_dir')){
      assign(path, gsub('3) output', '1a) dummy data/99_fake_output', get(path)))
    }}
  lapply(c(raw_output_dir, finished_output_dir, dummy_data_dir),function(x) suppressWarnings(dir.create(x, recursive = T)))


# SET IMPORTANT PARAMETER VALUES  -----------------------------------------------------------------------
year_range = 2008:2021
set.seed(43)


# project specific helper functions  --------------------------------------
## NB we're not currently using the churn rates function 
calc_churn_rates = function(df, group_var, birth_var, death_var, time_var, prefix){
  df[, (paste0(prefix, "_entrance_rate")) := as.numeric(NA_mean(get(time_var) == get(birth_var))),   by = c(group_var, time_var)] %>%
    .[, (paste0(prefix, "_exit_rate"))     := as.numeric(NA_mean(get(time_var) == get(death_var))),  by = c(group_var, time_var)] %>% 
    # .[, (paste0(prefix, "_immediate_failure_rate")) :=as.numeric(NA_mean(ifelse(get(time_var)==get(birth_var), get(birth_var) == (get(death_var)*get(time_var)), NA))), by=c(group_var, time_var)] %>%
    .[,(paste0(prefix, "_churn_rate")) := as.numeric(0.5 * (get(paste0(prefix, "_entrance_rate")) + get(paste0(prefix, "_exit_rate"))))] %>%
    .[, (paste0(prefix, "_immediate_failure_rate_year_to_year")) :=as.numeric(NA_sum(get(birth_var)==(get(death_var)*get(time_var)))/NA_sum(get(time_var)==get(birth_var))), by = c(group_var, time_var)] %>%
    .[, (paste0(prefix, "_immediate_failure_rate_avg_over_time")) :=as.numeric(NA_mean(get(paste0(prefix, "_immediate_failure_rate_year_to_year")))), by = group_var]

  return(df)
}

numeric_firmid = function(df){
  if(getwd() == "C:/Users/Public/Documents/Big data Project"){
  merge(df, import_file('1) data/0_misc_data/0b_dictionaries/0b1_matched_firm_dict.parquet'), by = 'firmid') %>% select(-firmid)
  }else{
    df[, firmid_num := as.numeric(as.factor(firmid))] %>% .[,firmid := NULL]  
  }
}
numeric_ctry = function(df){
  merge(df, import_file('1) data/0_misc_data/0b_dictionaries/0b2_ctry_dict.parquet', col_select = c('ctry', 'ctry_num')), by = 'ctry') %>% select(-ctry)
}
de_dummy = function(string){ gsub("1a) dummy data/99_fake_output","3) output", string)}
## PRESERVE INITIAL STATE OF AFFAIRS 
needed_for_import = c(gpaste(c('current_',''), c('import', 'export'), '_', c('name','dir')), 'code_to_run', 'i')
base_env = c(ls(),'base_env', needed_for_import )


