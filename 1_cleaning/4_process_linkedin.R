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
linkedin_input_dir = '1) data/7_revelio_data/b_processed_data/linkedin/'



# generate firm-yr lvl linkedin data --------------------------------------
role_dict = import_file(paste0(linkedin_input_dir,'revelio_role_dict.csv')) %>% select(-V1)
firm_lvl_dta = import_file(paste0(linkedin_input_dir, 'firm_lvl_info_all_matched_firms.parquet')) %>% numeric_firmid()
prestige_dta = import_file(paste0(linkedin_input_dir, 'matched_firm_user_prestige.parquet')) %>% 
  .[!is.na(highest_degree),college := highest_degree %in% c('Master', 'Bachelor', 'MBA', 'Doctor')]

role_lvl_dta = import_file(paste0(linkedin_input_dir, 'matched_firm_role_output.parquet')) %>%
  .[,comp := weight*total_compensation] %>% 
  .[,abroad := country != 'France'] %>%
  .[,abroad_total := !is.na(country)] %>% 
  merge(firm_lvl_dta[,.(rcid, firmid_num)]) %>%
  merge(role_dict,by = 'role_k1500') %>%
  merge(prestige_dta[,.(user_id, prestige, college)], all.x = T, by ='user_id') %>% 
  .[,college_total := !is.na(college)]

## generate dataset for firm information at the firmid_lvl
merge(firm_lvl_dta,role_lvl_dta[, .(weight = NA_sum(weight)), by = rcid], by = 'rcid') %>%
      setorder(., -weight) %>% distinct(firmid_num, .keep_all = T) %>% select(-rcid, -weight) %>%
      write_parquet('1) data/7_revelio_data/c_final_outputs/7c1_linkedin_firm_lvl.parquet')
  

int_vars = c('non_data_rnd','total', 'data', 'data_analyst',gpaste(c('college','abroad'), c("", "_total")))
output = rbindlist(lapply(year_range, function(year){
  print(year)
  cutoff_date = as.Date(paste0(year, "-01-01"))
  role_lvl_dta[as.Date(startdate) <= cutoff_date & (is.na(enddate) | as.Date(enddate) >= cutoff_date)] %>%
    .[, c(setNames(lapply(.SD[, ..int_vars], function(x) NA_sum(weight*x)), paste0('empl_',int_vars)),
          setNames(lapply(.SD[, ..int_vars], function(x) NA_sum(comp*x)), paste0('comp_',int_vars)),
          setNames(lapply(.SD[, ..int_vars], function(x) NA_sum(weight*prestige*x) / NA_sum(weight*x)), paste0('avg_prestige_',int_vars)),
          setNames(lapply(.SD[, ..int_vars], function(x) NA_sum(comp*prestige*x) / NA_sum(comp*x)), paste0('comp_wgted_avg_prestige_',int_vars))
    ), by = firmid_num] %>%
    .[,year := year]})) %>% 
  .[,(gpaste('share_', c('empl', 'comp'), "_",c('college','abroad'))) := 
      lapply(gpaste(c('empl', 'comp'), "_",c('college','abroad')),function(x) get(x) / get(paste0(x,"_total")))] %>%
  .[,(gpaste('share_', c('empl', 'comp'), "_data")) := lapply(c('empl', 'comp'), function(x) get(paste0(x, "_data")) / get(paste0(x, "_total")))] %>% 
  .[, `:=`(share_data_empl_analyst = empl_data_analyst / empl_data, share_data_comp_analyst = comp_data_analyst / comp_data)] %>%
  select(-con_fil(., 'abroad_total', 'college_total'))



## perform additional cleaning needed for firm-yr analysis 
vars_to_mil = con_fil(con_fil(output, 'comp'), 'prestige', 'share', inc = F)
output = output[,(vars_to_mil) := lapply(vars_to_mil, function(x) get(x)*1e-3)] %>%  ## aligns compensation data with scale used in BR 
  .[use_data := comp_data >0 ] %>% 
  .[,(paste0('log_', vars_to_mil)) := lapply(vars_to_mil, function(x) asinh(get(x)))] %>% 
  merge(import_file('1) data/9_age_data/9b_firm_lvl_birth_data.parquet', col_select = c('firmid_num', 'birth_year')), by = 'firmid_num') %>% 
  merge(import_file('1) data/3_bs_br_data.parquet', col_select = c('firmid_num', 'year', 'NACE_BR', 'empl')), all.x =T, by = c('firmid_num', 'year') ) %>% 
  
  ## We keep firms where the ratio of linkedin /admin employment is less than 10 (98% of observations)
  .[empl_total/ empl < 10 | is.na(empl)] %>% 
  within_group_filter(., 'any(!is.na(NACE_BR))', 'firmid_num')


  ## assume that if NACE_BR is missing it's given by it's closest value and then generate quartile vars 
  setorder(output, firmid_num, year)
  for (i in 1:(max(year_range)-min(year_range))){
    print(paste0(i, ": ",sum(is.na(output$NACE_BR))))
    output[, NACE_BR := ifelse(is.na(NACE_BR), lag(NACE_BR, i),NACE_BR), by = firmid_num ]
    output[, NACE_BR := ifelse(is.na(NACE_BR), lead(NACE_BR, i),NACE_BR), by = firmid_num ]
  }
  output = output[comp_data == 0,nace_comp_data_quartile:= 0] %>%
  .[comp_data > 0, nace_comp_data_quartile := ntile(comp_data,4), by = c('NACE_BR', 'year')] %>% 
  unbalanced_lag(.,'firmid_num', 'year', c(gpaste(c('', 'log_'),vars_to_mil),'nace_comp_data_quartile', 'use_data') , 1:2,expand = T,birth_var = 'birth_year') %>% 
  select(-c(birth_year,empl))

write_parquet(output, linkedin_firm_yr_path)
rm(list= setdiff(ls(), base_env)); gc()

# 
# ouput = import_file(linkedin_firm_yr_path) 
# vars_to_mil = con_fil(con_fil(output, 'comp'), 'prestige', 'share', 'nace', 'lag' ,inc = F)
# output =  output[,(paste0('log_', vars_to_mil)) := lapply(vars_to_mil, function(x) asinh(get(x)))] %>% 
#   merge(import_file('1) data/9_age_data/9b_firm_lvl_birth_data.parquet', col_select = c('firmid_num', 'birth_year')), by = 'firmid_num') %>% 
#   unbalanced_lag(.,'firmid_num', 'year', gpaste( 'log_',vars_to_mil), 1:2,expand = T,birth_var = 'birth_year') %>% select(-birth_year) %>% 
#   write_parquet(output, linkedin_firm_yr_path)
  