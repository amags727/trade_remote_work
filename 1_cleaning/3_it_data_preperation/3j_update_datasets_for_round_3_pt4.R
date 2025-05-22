# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/Big Data Code",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')
# 1 firm yr  ----------------------------------------------------------------
death_data = import_file(file.path(inputs_dir, '16a_firm_lvl_birth_data.parquet'),
                         char_vars = 'firmid', col_select = c('firmid', 'last_observed'))
file_name = file.path(inputs_dir, '16c_firm_yr_lvl.parquet')
base = import_file(file_name) %>%  select(-intersect(names(.), 'last_observed')) %>% merge(death_data, by = c('firmid')) %>% 
  calc_churn_rates(., 'NACE_BR', 'birth_year', 'last_observed', 'year', 'nace')
write_parquet(base, file_name)

base %>% select(NACE_BR, year, con_fil(., 'rate')) %>% unique() %>% remove_if_NA('NACE_BR')
rm(list= setdiff(ls(), base_env)); gc()

# 2 firm ctry year  ---------------------------------------------------------
file_name = file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')


base = import_file(file_name) %>%
  calc_churn_rates(., 'ctry', 'streak_start', 'last_year_of_streak', 'year', 'mkt') %>%
  calc_churn_rates(., c('ctry','NACE_BR'), 'streak_start', 'last_year_of_streak', 'year', 'nace_mkt') %>% 
  select(-con_fil(.,'extended'))

## add gravity
similiarity_data = import_file ('1) data/similarity_matrices/outputs/similiarity_data.rds')
grav_cats =  c('region', 'border', 'language')
i = which(similiarity_data$ctry == 'FR')
for (x in grav_cats) assign(gpaste(x, '_list'), similiarity_data[[paste0('share_',x)]][i][[1]])
base[,(paste0('grav_', grav_cats)) := lapply(paste0(grav_cats, "_list"), function(x) ctry %in% get(x))]

## add extended gravity
extended_grav_data = rbindlist(lapply(1:nrow(similiarity_data), function(i){
  
  ## retrieve the list of countries that fulfill gravity reqs for a particular market 
  mkt =  similiarity_data$ctry[i]
  for (x in grav_cats) assign(gpaste(x, '_list'), setdiff(similiarity_data[[paste0('share_',x)]][i][[1]],mkt ))
  
  ##  subset to the firms operating in the mkt of interest in a given year 
  temp = base[, in_mkt := any(ctry == mkt), by = .(firmid,year)][in_mkt == T] %>% 
    
    # check to see if they have a market that fulfills each criteria 
    .[, setNames(lapply(paste0(grav_cats, "_list"),
     function(x) any(ctry %in% get(x))), paste0('extended_grav_', grav_cats)),
     by = .(firmid,year)] %>% 
    .[, ctry := mkt]}))
base = merge(base, extended_grav_data, all.x = T, by = c('firmid', 'ctry', 'year')) %>% 
  .[, (paste0('either_grav_', grav_cats)) :=
      lapply(grav_cats, function(x) get(paste0('grav_', x)) | get(paste0('extended_grav_', x)))] 
rm(extended_grav_data); gc()


write_parquet(base, file_name)
rm(list= setdiff(ls(), base_env)); gc()


# 3 firm total variance  --------------------------------------------------
vars_to_mean = gpaste('nace_', c('entrance', 'exit', 'churn'), "_rate")

new_meaned = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% 
  remove_if_NA(., 'year', 'comp_data') %>% .[!is.na(dom_turnover) | !is.na(total_export_rev_customs)] %>% 
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T) %>% 
  .[,c(setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean)), by = firmid]

base = import_file(file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet')) %>% select(-intersect(names(.), vars_to_mean)) %>%
  merge(new_meaned, by = 'firmid') 

write_parquet(base, file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet'))
rm(list= setdiff(ls(), base_env)); gc()

# 4 firm market variance  ---------------------------------------------------
vars_to_mean =  c(gpaste(c('mkt_', 'nace_mkt_'), c('entrance', 'exit', 'churn'), "_rate"),
                  gpaste(c('', 'extended_', 'either_'), 'grav_',  c('region', 'border', 'language')))


new_meaned = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')) %>% 
  remove_if_NA(., 'year', 'export_rev_customs', 'comp_data','NACE_BR') %>%
  .[year %in% year_range] %>% distinct(firmid, year, ctry, .keep_all = T) %>% 
  .[,c(setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean)), by = .(firmid, ctry, streak_id)]

base = import_file(file.path(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet')) %>% 
  select(-intersect(names(.),vars_to_mean)) %>% 
  merge(new_meaned, by = c('firmid', 'streak_id', 'ctry')) 

write_parquet(base, gpaste(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet'))
rm(list= setdiff(ls(), base_env)); gc()

# 5 firm ctry entrance  ---------------------------------------------------
firm_yr_vars = c('firmid', 'year', gpaste('nace_', c('entrance', 'exit', 'churn'), "_rate"))
firm_yr = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'), col_select = firm_yr_vars) 
base_ctry_lvl =  import_file(file.path(inputs_dir,'16d_firm_ctry_yr_lvl.parquet')) %>% 
  select('ctry', con_fil(con_fil(names(.),"grav"), 'extended','either', inc = F)) %>% unique()


for (file in list.files('1) data/temp_data',recursive = TRUE, full.names = TRUE)){
  import_file(file,char_vars = 'firmid') %>% select(-c(intersect(names(.), firm_yr_vars[-c(1:2)]), con_fil(names(.),"grav"))) %>% 
    merge(firm_yr, all.x = T,  by = c('firmid','year')) %>% merge(base_ctry_lvl, all.x = T, by = 'ctry') %>% 
    fwrite(.,file)
}
rm(list= setdiff(ls(), base_env)); gc()









