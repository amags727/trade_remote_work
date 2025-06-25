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
linkedin_input_dir = '1) data/7_revelio_data/b_processed_data/linkedin/'
linkedin_output_dir = '1) data/7_revelio_data/c_final_outputs/'


# import data -------------------------------------------------------------
city_location = import_file('1) data/7_revelio_data/b_processed_data/linkedin/linkedin_city_coords.parquet')
role_dict = import_file(linkedin_input_dir,'revelio_role_dict.csv', col_select = c('role_k1500', 'total', 'data', 'data_analyst'))
firm_lvl_dta = import_file(paste0(linkedin_input_dir, 'firm_lvl_info_all_matched_firms.parquet')) %>% numeric_firmid()
prestige_dta = import_file(paste0(linkedin_input_dir, 'matched_firm_user_prestige.parquet')) %>% 
  .[!is.na(highest_degree),college := highest_degree %in% c('Master', 'Bachelor', 'MBA', 'Doctor')] %>%
  select(-highest_degree)
uni_data = import_file(linkedin_input_dir, 'data_grads_across_france.parquet')

# generate firm lvl linkedin data -----------------------------------------------------------------------
firm_lvl_output = merge(
  firm_lvl_dta,
  import_file(paste0(linkedin_input_dir, 'matched_firm_role_output.parquet')) %>% .[, .(weight = NA_sum(weight)), by = rcid],
  by = 'rcid') %>%
  setorder(., -weight) %>% distinct(firmid_num, .keep_all = T) %>% select(-rcid, -weight) %>% 
  merge(city_location, by.x = 'hq_city', by.y = 'city') %>% .[hq_country != 'France', c('NUTS_ID', 'NUTS_NAME'):= NA]

write_parquet(firm_lvl_output,linkedin_firm_path)

# generate NUTS-3-year lvl graduation data--------------------------------
uni_output = merge(uni_data, city_location, by.x = 'university_city', by.y = 'city') %>% 
  .[,.(total_grads = NA_sum(total_grads), data_grads = NA_sum(data_grads)), by= .(grad_year, NUTS_ID, NUTS_NAME)] %>% 
  unbalanced_lag(., 'NUTS_ID', 'grad_year', c('total_grads', 'data_grads'), 1:4)

for (var in c('total_grads', 'data_grads')){
command = paste0(
  'uni_output[,',var,'_5ycum := ',
  paste(collapse = " + ", unlist(lapply(0:4, function(x){lag =paste0(var, "_lag",x);  paste0('ifelse(is.na(',lag,'), 0,',lag,")")}))),
  ']') %>%
  gsub('_lag0','', . )
eval(parse(text = command))
}
vars_to_log = gpaste(c('total', 'data'), "_grads", c('', '_5ycum'))
uni_output = uni_output[grad_year %in% year_range] %>% select(-con_fil(., 'lag')) %>% 
  mutate(across(vars_to_log, ~asinh(.), .names = 'log_{col}')) %>% 
  .[,log_total_grads_sq := log_total_grads^2]

write_parquet(uni_output, paste0(linkedin_output_dir, '7c2_linkedin_firm_yr_lvl.parquet'))


# generate firm-yr-lvl linkedin data --------------------------------------
role_vars = c("rcid", "weight", "total_compensation", "startdate", "enddate", "role_k1500", 'user_id')
rm(role_lvl_data); gc()
role_lvl_data = import_file(linkedin_input_dir, 'matched_firm_role_output.parquet', col_select = role_vars) %>% 
  .[,comp := weight*total_compensation] %>% 
  .[, `:=`(start_year = year(as.Date(startdate)), end_year = year(as.Date(enddate)))] %>% 
  merge(role_dict, by = 'role_k1500') %>%
  merge(firm_lvl_dta[,.(rcid, firmid_num)], by = 'rcid') %>%
  merge(prestige_dta, by = 'user_id') %>% .[,college_total := !is.na(college)] %>%  
  .[, c("startdate", "enddate", 'role_k1500', 'rcid', 'user_id') := NULL]


output = rbindlist(lapply(year_range, function(year){
  print(year)
  collapse = role_lvl_data[start_year <= year & (is.na(end_year) | end_year >= year)] %>% 
    .[, .(empl_total = NA_sum(weight), comp_total = NA_sum(comp)*1e-3,
          empl_data = NA_sum(weight*data), comp_data = NA_sum(comp*data)*1e-3,
          
          share_comp_data = NA_sum(comp*data)/ NA_sum(comp), 
          share_data_empl_analyst = NA_sum(weight*data_analyst)/ NA_sum(weight*data),
          avg_prestige_total = NA_mean(prestige),
          share_empl_college = NA_sum(weight*college)/ NA_sum(weight*college_total)), by = .(firmid_num)] %>% 
    .[,`:=`(year = year,
            use_data = empl_data >0,
            log_comp_data = asinh(comp_data),
            log_comp_total = asinh(comp_total))]
}))

## merge in firm level / uni data 
output = output %>% 
  merge(firm_lvl_output[,.(is_subsid, firmid_num, NUTS_ID)], by = 'firmid_num') %>% 
  merge(uni_output %>% rename(year = grad_year) %>% select(year, NUTS_ID, con_fil(., 'log')), by = c('year', 'NUTS_ID'), all.x =T) %>% 
  .[,(con_fil(., 'grads')):= lapply(con_fil(.,'grads'),function(x) replace_na(get(x),0))]
          
## merge in CASD data 
if(!dummy_version){
output = output %>% 
  merge(import_file('1) data/9_age_data/9b_firm_lvl_birth_data.parquet', col_select = c('firmid_num', 'birth_year')), by = 'firmid_num') %>% 
  merge(import_file('1) data/3_bs_br_data.parquet', col_select = c('firmid_num', 'year', 'NACE_BR', 'empl')), all.x =T, by = c('firmid_num', 'year')) 
}else{
    output = merge(output,data.table(
    firmid_num = sample(unique(output$firmid_num)),    
    NACE_BR = rep(1:500, length.out = length(unique(output$firmid_num)))), by = 'firmid_num') %>% 
    .[,`:=`(birth_year = NA, empl = empl_total)]
}

## We keep firms where the ratio of linkedin /admin employment is less than 10 (98% of observations) and where we successfully match them 
## to the admin data at some point 
output = output[empl_total/ empl < 10 | is.na(empl)] %>% within_group_filter(., 'any(!is.na(NACE_BR))', 'firmid_num')


## assume that if NACE_BR is missing it's given by it's closest value and then generate quartile vars 
setorder(output, firmid_num, year)
for (i in 1:(max(year_range)-min(year_range))){
  if(sum(is.na(output$NACE_BR))==0) break
  print(paste0(i, ": ",sum(is.na(output$NACE_BR))))
  output[, NACE_BR := ifelse(is.na(NACE_BR), lag(NACE_BR, i),NACE_BR), by = firmid_num ]
  output[, NACE_BR := ifelse(is.na(NACE_BR), lead(NACE_BR, i),NACE_BR), by = firmid_num ]
}


## gen leave out instrument vars
output[,nace_nut_comp_data := NA_sum(comp_data), by = .(NUTS_ID, NACE_BR, year)] %>% 
  .[,log_nace_non_nut_comp_data := asinh(NA_sum(comp_data)- nace_nut_comp_data), by = .(NACE_BR, year)] %>% 
  .[,log_non_nace_nut_comp_data := asinh(NA_sum(comp_data)- nace_nut_comp_data), by = .(NUTS_ID, year)] %>% 
  .[,nace_nut_comp_data := NULL]

## generate lagged variables
lag_vars = c('empl_data','comp_data', 'log_comp_data', 'log_comp_total',
             'nace_comp_data_quartile', 'use_data', 'share_comp_data',
             'log_nace_non_nut_comp_data','log_non_nace_nut_comp_data') 
output = output[comp_data == 0,nace_comp_data_quartile:= 0] %>%
  .[comp_data > 0, nace_comp_data_quartile := ntile(comp_data,4), by = c('NACE_BR', 'year')] %>% 
  unbalanced_lag(.,'firmid_num', 'year',lag_vars , 1,expand = T,birth_var = 'birth_year') %>% 
  .[,`:=`(empl_data_delta = empl_data - empl_data_lag1,
          share_comp_data_delta = share_comp_data - share_comp_data_lag1)] %>%
  select(-c(birth_year,empl))


write_parquet(output,  v)
rm(list= setdiff(ls(), base_env)); gc()

