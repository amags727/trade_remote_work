## IMPORT PACKAGES AND HELPER FUNCTIONS 
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest', 'countrycode', 'survival', 'knitr', 'parallel','googledrive')
lapply(packages, function(package){tryCatch({library(package,character.only = T)}, error = function(cond){
  install.packages(package); library(package, character.only = T)
})})
if (grepl('GoogleDrive-amagnuson@g.harvard.edu',getwd())) drive_auth(email = 'amagnuson@g.harvard.edu')
source('2) code/00_helper_functions.R')

## SET ADMIN PARAMETER VALUES 
  dummy_version = grepl('amagnuson', getwd());
  running_regressions = !grepl('amagnuson', getwd());
  enforce_pauses = grepl('amagnuson', getwd())
  cleaning_admin = F
  make_randomized = F #grepl('amagnuson', getwd());
  
  make_linkedin_vars_complete = T; make_birth_data = T; make_firm_yr = T; 
  make_firm_ctry_yr = T; make_ctry_entrance = T; make_variance = T
  inputs_dir = ('1) data/16_inputs_for_data_summary_stats/')
  linkedin_ctry_lvl_path = '1) data/15_revelio_outputs/15a_matched_firm_foreign_employment.parquet'
  linkedin_basic_path = '1) data/15_revelio_outputs/15b_matched_firm_empl_and_linkedin_characteristics.parquet'
  linkedin_match_path = '1) data/15_revelio_outputs/15d_all_linkedin_matched_firmids_final.parquet'
  
  if(dummy_version & !make_randomized){
    linkedin_ctry_lvl_path = gsub('.par', '_dummy.par', linkedin_ctry_lvl_path)
    linkedin_basic_path = gsub('.par', '_dummy.par',  linkedin_basic_path)
    linkedin_match_path = gsub('.par', '_dummy.par', linkedin_match_path)
  }
  
  analysis_round = 3
  output_base = paste0('3) output/',letters[analysis_round],"_round_",analysis_round,"_analysis/")
  suppressWarnings(dir.create(output_base))
  raw_output_dir = paste0(output_base,letters[analysis_round],"1_raw_output/")
  finished_output_dir = paste0(output_base,letters[analysis_round],"2_finished_tables/")
  lapply(c(raw_output_dir, finished_output_dir),function(x) suppressWarnings(dir.create(x)))
  rm(output_base)



## SET IMPORTANT PARAMETER VALUES 
year_range = 2008:2021
restricting_sample = T
set.seed(43)

age_header =gpaste("&",gpaste('\\multicolumn{4}{c}{',
                              c('All Firms', 'Young Firms (age $<$ 5)', 'Mature Firms (age $\\ge$ 5)'), "}",
                              collapse_str = "& &"),'\\\\')

### Generate Project Specific Helper Functions

variance_metrics = function(df,subset_id = NA, remove_NA_subset = T, 
                            time_id, group_id, ind_id, int_id, birth_id,
                            logged_version = T,prefix = "", full_dataset = T){
  df_og = df
  df$time_var = df[[time_id]]; df$group_var = df[[group_id]];
  df$int_var = df[[int_id]]; df$birth_var = df[[birth_id]]
  df$ind_var = df[[ind_id]]
  max_time = NA_max(df$time_var)
  
  if (logged_version) df$int_var = asinh(df$int_var)
  if(!is.na(subset_id)){df$subset_var = df[[subset_id]]}else{df$subset_var = 1}
  if(remove_NA_subset){df = df[!is.na(subset_var)]}
  
  
  temp = lapply(unique(df$subset_var), function(subset_val){ 
    df = df[subset_var == subset_val]
    churn = df[,
               .(entrance_rate = NA_mean(as.numeric(time_var == birth_var)),
                 exit_rate = NA_mean(as.numeric(time_var == max(time_var)))),
               by = .(group_var, time_var)] %>%
      .[time_var == max_time, exit_rate := NA_real_] %>%
      .[,churn_rate := .5*(entrance_rate + exit_rate)]
    
    
    variance_1 = df[,.(de_trended_variance_ind_lvl = sub_regression(int_var, time_var, asr = T),
                       variance_ind_lvl = var(int_var)), by = .(ind_var, group_var)] %>%
      .[, lapply(.SD, NA_mean), .SDcols = c('de_trended_variance_ind_lvl', 'variance_ind_lvl'), by = group_var]
    
    
    variance_2 = df[,logged := logged_version] %>%
      .[,.(int_var = ifelse(logged, asinh(NA_sum(sinh(int_var))),NA_sum(int_var))), by = .(group_var, logged, time_var)]  %>%
      .[, .(de_trended_variance_group_lvl = sub_regression(int_var, time_var, asr = T),
            variance_group_lvl = var(int_var)), by = .(group_var)]
    
    temp = merge(churn, variance_1, all = T) %>% merge(variance_2, all = T) %>% 
      select(-c(entrance_rate, churn_rate, exit_rate)) %>% 
      rename_with(.cols = -c(group_var, time_var), ~(paste0(prefix,.))) %>% 
      rename(!!group_id := group_var, !!time_id := time_var)
    if(logged_version) names(temp) = gsub('variance', 'log_variance', names(temp))
    if(!is.na(subset_id)){temp[[subset_id]] = subset_val}
    return(temp)
  }) %>% rbindlist()
  if(full_dataset){return(merge(df_og,temp, all.x = T, by = c(group_id, time_id)))}else{return(temp)}
  
}

calc_churn_rates = function(df, group_var, birth_var, death_var, time_var, prefix){
  df[, (paste0(prefix, "_entrance_rate")) := as.numeric(NA_mean(get(time_var) == get(birth_var))),   by = c(group_var, time_var)] %>%
    .[, (paste0(prefix, "_exit_rate"))     := as.numeric(NA_mean(get(time_var) == get(death_var))),  by = c(group_var, time_var)] %>% 
    .[,(paste0(prefix, "_churn_rate")) := as.numeric(0.5 * (get(paste0(prefix, "_entrance_rate")) + get(paste0(prefix, "_exit_rate"))))] %>%
    
  return(df)
}

gen_coef_orders = function(coef_names, base){
  output = lapply(1:length(coef_names), function(i){
    unique(c(1, which(grepl('multi',coef_names[[i]])),
             which(coef_names[[i]] %in% setdiff(interactions[[i]], base)),
             which(coef_names[[i]] %in% intersect(interactions[[i]], base)),
             1:length(coef_names[[i]])))})
}
## PRESERVE INITIAL STATE OF AFFAIRS 
base_env = c(ls(),'base_env')


