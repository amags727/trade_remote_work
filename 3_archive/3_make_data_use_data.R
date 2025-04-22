# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest')
lapply(packages, function(package){tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')}
source("2) code/00_helper_functions.R")
inputs_dir = ('1) data/16_inputs_for_data_summary_stats')
dir.create(inputs_dir)
base_env = c(ls(),'base_env')

# generate linkedin / bs_br data --------------------------------------------------
in_the_states = grepl("/Users/amagnuson/Library/CloudStorage/", getwd()); set.seed(1)
if(!in_the_states){
linkedin = import_file('1) data/15_french_affiliated_firm_roles_collapsed_clean.parquet') %>%
  select(-c(rcid, `__index_level_0__`)) 

## if necessary anonymized linkedin data for matching 
{
# linkedin = merge(linkedin, data.table(firmid = linkedin[!duplicated(firmid) & has_siren][['firmid']]) %>%
#                    mutate(firmid_new =  as.character(sample(seq_len(nrow(.)), size = nrow(.), replace = FALSE)),
#                           replace = sample(c(0,1), size = nrow(.), replace = T)),
#                  all.x = T, by = 'firmid') %>%
#   .[!is.na(firmid_new) & replace == 1, firmid := firmid_new] %>% select(-firmid_new, replace)
}


## import the bs_br data and  merge together
subset_vars = c('french', 'engineer','data', 'rnd','stem')
bs_br_linkedin = import_file('1) data/3_bs_br_data.csv', char_vars =  c('firmid')) %>% 
  distinct(firmid, year, .keep_all = TRUE) %>% 
  merge(linkedin, all.x = T) %>% 
  .[,`:=`(cost_per_worker_linkedin = comp_total / emp_total,
          cost_per_worker_linkedin_fr = comp_french / emp_french,
          cost_per_worker_bs = labor_cost / empl,
          tfp = turnover / (capital^.3*labor_cost^.7))]

## generate the share variables 
command = paste0('bs_br_linkedin = bs_br_linkedin[, `:=`('
                 ,gpaste('share_comp_',subset_vars, '=comp_',subset_vars ,
                         '/comp_total', no_expand = T, collapse_str = ', '),
                 ')]')
eval(parse(text = command))

write_parquet(bs_br_linkedin, file.path(inputs_dir, '16a_bs_br_linkedin.parquet'))
rm(list= setdiff(ls(), base_env))
}else{
  import_file('/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work/1) data/16_inputs_for_data_summary_stats/16a_bs_br_linkedin.parquet',
              char_vars = 'firmid') %>% write_parquet(file.path(inputs_dir, '16a_bs_br_linkedin.parquet')) 
}

# generate complete birth data ------------------------------------------
#### prepare customs birth data
customs_birth_data = import_csv('1) data/9_customs_cleaned.csv', char_vars =  c('firmid'), col_select = c('year', 'firmid','exim')) %>%
  unique() %>% .[,min_exim_year := min(year), by = .(firmid,exim)] %>% 
  pivot_wider(id_cols = c(year,firmid), names_from = exim, values_from = min_exim_year, names_prefix = 'exim') %>% as.data.table() %>% 
  .[, .(birth_year_customs = min(year), first_import_year = as.integer(NA_min(exim1)), first_export_year = as.integer(NA_min(exim2))), by = firmid] %>%
  .[, `:=`(ever_import =  !is.na(first_import_year), ever_export = !is.na(first_export_year))]

### prepare bs/br birth data
br_birth_data = import_file(file.path(inputs_dir, '16a_bs_br_linkedin.parquet'), col_select = c('firmid', 'year'), char_vars = 'firmid') %>%
  unique() %>%
  .[!is.na(year) & !is.na(firmid)] %>%
  .[,.(birth_year_BS = min(year)), by = firmid] %>%
  .[birth_year_BS == 1994, birth_year_BS := NA] #94 is the first year of our data 

### merge together component datasets to generate birth data 
first_cust = c("first_import_year", "first_export_year")
birth_data = import_parquet('1) data/14_admin_birth_data.parquet')  %>% unique() %>% 
  merge(customs_birth_data, all = T) %>%merge(br_birth_data, all = T) %>% 
  
  ## generate birth year data with order of priority:
  ## 1) bs/br if less than customs 2) admin if less than customs 3) customs if everything else missing 
  .[,birth_year := case_when(
                            birth_year_BS <= birth_year_customs ~ birth_year_BS,
                            !is.na(birth_year_BS) & is.na(birth_year_customs) ~ birth_year_BS,
                            birth_year_admin <= birth_year_customs ~ birth_year_admin,
                            !is.na(birth_year_admin) & is.na(birth_year_customs) ~ birth_year_admin,
                            birth_year_customs != 1993 & is.na(birth_year_admin) & is.na(birth_year_BS) ~ birth_year_customs,
                            T ~ NA_real_)] %>% 
  .[,type := case_when(is.na(birth_year) ~ "missing",
                       birth_year == birth_year_BS ~ "BS",
                       birth_year == birth_year_admin ~ "admin",
                       birth_year == birth_year_customs ~ "customs",
                       T ~ "missing")] %>%

  ## correct the first export / import year: it only exists if we know when the firm was born 
  ## and that year is <= first export year 
  .[, (first_cust) := lapply(.SD, function(x)
    case_when(is.na(birth_year) | (x==1993 & birth_year !=1993)~NA,
              birth_year <= x ~x,
              !is.na(birth_year) & is.na(x) ~ 2099, # it could have exported / imported but didn't --> set to 2099 for ease
              T~ NA)),  .SDcols = first_cust]



write_parquet(birth_data,file.path(inputs_dir, '16b_complete_birth_data.parquet'))
rm(list= setdiff(ls(), base_env)); gc()
# generate customs data ---------------------------------------------------
sim_vars = 'french_distance'
french_sim = read_rds('1) data/similarity_matrices/outputs/similiarity_data.rds') %>% filter(ctry == 'FR')
french_distances = fread('1) data/similarity_matrices/outputs/france_distance_data.csv') %>%
  rename(french_distance = distance_to_france) %>% 
  mutate(french_distance = 1-french_distance/max(french_distance,na.rm =  T))
birth_data = import_file(file.path(inputs_dir, '16b_complete_birth_data.parquet'), col_select = c('firmid', 'birth_year'), char_vars = 'firmid')


customs_data = import_csv('1) data/9_customs_cleaned.csv', char_vars = 'firmid') %>% 
  distinct(firmid, year,exim, ctry, .keep_all = TRUE) %>% 
  merge(birth_data, all.x = T) %>% 
  unbalanced_lag(., c("firmid", 'exim', 'ctry'), "year", "deflated_value", c(-1,1)) %>% 
  merge(french_distances, all.x= T, by = 'ctry') %>% 
  
  ## define flow type and market entry terms 
  .[, `:=`(flow_type = ifelse(exim ==1, 'import', 'export'),
           new_markets = is.na(deflated_value_lag1) & (year !=1993 | year == birth_year))] %>% #customs data starts in 1993
  .[, market_entry_failures := case_when(!new_markets| year == max(year,na.rm = T) ~ NA, is.na(deflated_value_lead1) ~ T, T~F)] %>% 
           
  ## collapse down to firmid, year, flow_type
  rename(value_customs = deflated_value) %>%
  .[,intermarket_hhi := (value_customs / NA_sum(value_customs))^2, by = .(firmid, year, flow_type)] %>%
  group_by(firmid, year, flow_type) %>% 
  summarize(across(c('new_markets', 'market_entry_failures', 'value_customs', 'intermarket_hhi'), ~NA_sum(.)), 
            currently = as.numeric(any(value_customs != 0)), 
            markets = n(), 
            across(sim_vars, ~NA_sum(1-.),.names = "{col}_wgted_markets"),
            across(sim_vars, ~NA_sum(value_customs *(1-.)), .names = "{col}_wgted_value_customs"), .groups = 'drop') %>% 
  mutate(market_entry_failure_rate = ifelse(new_markets ==0,NA, market_entry_failures/ new_markets))  %>% 
  
  ## redo the collapse to be at firmid, year level 
  pivot_wider(id_cols = c(firmid,year), names_from = flow_type, values_from =
                c('currently','markets', 'market_entry_failure_rate', 'value_customs','intermarket_hhi',
                  gpaste(sim_vars, c('_wgted_markets', '_wgted_value_customs'))))

write_parquet(customs_data,file.path(inputs_dir, '16c_customs_for_data_summ_stats.parquet'))
rm(list= setdiff(ls(), base_env)); gc()
# generate the industry level data ----------------------------------------
## code for making nace code data is at  3a_make_ancillary data 
nace_code = import_file(file.path(inputs_dir, '16d_nace_code_breakdown.csv')) %>%
  select(paste0('nace_', 1:4)) %>% 
  mutate(NACE_BR = nace_4) %>% 
  filter(!is.na(nace_4))
birth_data = import_file(file.path(inputs_dir, '16b_complete_birth_data.parquet'), 
                         char_vars = 'firmid', col_select = c('firmid', 'birth_year'))

industry_data = import_file(file.path(inputs_dir, '16a_bs_br_linkedin.parquet'), char_vars = 'firmid') %>%
  distinct(firmid, year, .keep_all = TRUE) %>% 
  merge(birth_data, all.x =T) %>%
  merge(nace_code, by = 'NACE_BR',all.x = T)

industry_data = lapply(1:4, function(level){
  temp_nace_code =  import_file(file.path(inputs_dir, '16d_nace_code_breakdown.csv')) %>% 
    select(names(.) %>% .[grepl(paste(1:level,collapse= "|"),.)]) %>% unique()
  temp_nace_code$nace_code = as.character(temp_nace_code[[paste0('nace_',level)]])
  temp_nace_code= temp_nace_code %>% filter(nace_code != "")
  
  temp_industry_data = industry_data %>% 
    mutate(nace_code = as.factor(.data[[paste0("nace_", level)]])) %>%
    .[!is.na(nace_code) & year>2008]
  
  ## generate stats on volatility in interest period (2008 ->)
  overall_vol = temp_industry_data[!is.na(turnover)] %>% 
    .[, .(turnover = mean(deflated_turnover), count = .N), by = .(nace_code, year)] %>%
    .[, .(industry_variance_turnover = var(turnover, na.rm =T),
          industry_de_trended_variance_turnover = sub_regression(turnover,year, ssr = T),
          industry_variance_log_turnover = var(asinh(turnover), na.rm =T),
          industry_de_trended_variance_log_turnover = sub_regression(asinh(turnover),year, ssr = T),
          num_period_observations = sum(count)), by= nace_code] %>% 
    merge(expand(min(temp_industry_data$year):max(temp_industry_data$year), .[['nace_code']], names = c('year', 'nace_code')))
  
  
  ## generate summary stats on churn 
  churn_summary = temp_industry_data %>%
    .[, last_seen := max(year), by = firmid] %>%
    .[year<= NA_max(year) & !is.na(birth_year)] %>%
    .[, .(industry_entrance_share = sum(birth_year == year)/ .N,
          industry_exit_share = sum(last_seen == year)/ .N,
          num_yearly_observations = .N), by = .(nace_code, year)] %>%
    .[, industry_churn_share := (industry_entrance_share + industry_exit_share)/2]
  
  ## generate summary stats on data use
  varlist = c('comp_data', 'share_comp_data','data_to_turnover')
  mean_command = gpaste('industry_mean_',varlist,rep('=mean(',length(varlist)) ,varlist, ")",no_expand = T, collapse_str = ', ')
  data_summary = temp_industry_data %>% .[!is.na(comp_data)] %>% .[,data_to_turnover := comp_data /turnover] 
  command = paste0("data_summary = data_summary[,.(", mean_command, ",",
                   gsub('mean', 'sd', mean_command),"), by = .(nace_code, year)]") %>%
    gsub('to_turnover=', 'to_turnover=NA_',.) 
  eval(parse(text = command))
  
  temp_output = merge(churn_summary, overall_vol, by = c('year', 'nace_code'), all = T) %>%
    merge(data_summary, by = c('year', 'nace_code'), all = T) %>% 
    merge(temp_nace_code, by = 'nace_code',all.x = T) %>%
    mutate(nace_level = level) %>%
    select(nace_level, nace_code, year, num_yearly_observations, num_period_observations, everything()) %>%
    group_by(year) %>% 
    mutate(across(names(.) %>% .[grepl('industry',.) & !grepl('sd_',.)],
                  ~ntile(.,4), .names = "{col}_quartile")) %>%
    ungroup()
}) %>% rbindlist(fill = T, use.names = T)

write_parquet(industry_data,file.path(inputs_dir, '16e_data_x_industry_stats.parquet'))

## generate a censored version for export 
org_vars = c(names(industry_data)[1:3], names(industry_data) %>% .[grepl('desc',.)],paste0('nace_',1:4))
period_vars = c(names(industry_data) %>% .[grepl('variance',.)],'num_period_observations')
yearly_vars = setdiff(names(industry_data), c(org_vars, period_vars))
industry_data_censored = industry_data %>% 
  mutate(across(period_vars, ~ifelse(num_period_observations < 5, NA,.)),
         across(yearly_vars, ~ifelse(num_yearly_observations < 5, NA,.))) %>%
  filter(!is.na(num_yearly_observations) | !is.na(num_period_observations))

write_parquet(industry_data_censored,file.path(inputs_dir, '16f_data_x_industry_stats_censored.parquet'))
rm(list= setdiff(ls(), base_env)); gc()
# generate the firm-year level dataset  ---------------------
## define variable groups 
  exp_imp = c('export', 'import')
  growth_vars = c('turnover', 'dom_turnover', gpaste(c('value_bs', 'value_customs', 'markets'), '_export'))
  birth_vars = c('firmid', 'birth_year', gpaste('first_',exp_imp, '_year'), gpaste("ever_", exp_imp))
  customs_to_zero= c('currently','markets', 'value_customs', 'currently')
  log_vars = c(growth_vars,'empl','age', paste0('cost_per_worker', c('_linkedin',"_linkedin_fr", '_bs')), 
               paste0('comp_',c("total", "engineer", "rnd", "stem", 'data')),
               paste0('years_since_first_', exp_imp))

## import industry / birth data 
  industry_data = import_file(file.path(inputs_dir, '16e_data_x_industry_stats.parquet')) %>%
    filter(nace_level == 4) %>% 
    select(-contains('desc'),-contains('observation'),-nace_level) %>%
    rename(NACE_BR = nace_code) %>% mutate(NACE_BR = as.numeric(NACE_BR)) 
  
  birth_data = import_file(file.path(inputs_dir, '16b_complete_birth_data.parquet'),
                           char_vars = 'firmid', col_select = birth_vars)


### MERGE TOGETHER ALL THE DATA 
  firm_yr_lvl = import_file(file.path(inputs_dir, '16a_bs_br_linkedin.parquet'), char_vars = "firmid") %>%
    
    ## merge in the customs data and zero where necessary 
    merge(import_file(file.path(inputs_dir, '16c_customs_for_data_summ_stats.parquet')), all.x = T) %>% 
    rename(value_bs_export = for_turnover) %>% 
    mutate(across(intersect(names(.),paste0(customs_to_zero,'_export')), ~ ifelse(is.na(markets_export), 0, .)),
           across(intersect(names(.),paste0(customs_to_zero,'_import')), ~ ifelse(is.na(markets_import), 0, .))) %>%
    as.data.table() %>% .[year >= 2008] %>% 
    
    # merge in birth / industry data
    merge(birth_data, all.x = T) %>% 
    .[,age := year - birth_year] %>% 
    .[,(paste0('years_since_first_',exp_imp)) := lapply(.SD, function(x) year - x), .SDcols = paste0('first_',exp_imp,'_year')] %>%
    .[,(paste0('years_since_first_',exp_imp)) := lapply(.SD, function(x) ifelse(x<0, NA, x)), .SDcols = (paste0('years_since_first_',exp_imp))] %>%
    .[, (paste0('is_first_',exp_imp,'_year')):= lapply(.SD, function(x) year == x), .SDcols = paste0('first_',exp_imp,'_year')] %>%
  
    # merge in industry data
    merge(industry_data, by= c('NACE_BR','year'), all.x = T) %>% 
    
    ## add in data quartile variables
    .[, paste0("quartile_", paste0(c("", "share_"), "comp_data")) := 
        lapply(.SD, function(x) as.factor(ntile(x, 4))), 
      .SDcols = paste0(c("", "share_"), "comp_data"), by = .(NACE_BR, year)] %>%
    
    ## add in the growth rate vars
    unbalanced_growth_rate(.,'firmid', 'year', growth_vars, c(-1,0),birth_var =  "birth_year", expand = T) %>%
    unbalanced_growth_rate(.,'firmid', 'year', growth_vars, c(0,1),birth_var  = "birth_year", alt_suffix = '_growth_rate_lead1',expand = T) %>%
    unbalanced_growth_rate(.,'firmid', 'year', growth_vars, c(-1,1),birth_var  = "birth_year", alt_suffix = '_growth_rate_2yr',  expand = T) %>%
    
    ## add in log vars 
    mutate(across(log_vars, ~asinh(.), .names = "log_{col}"))


write_parquet(firm_yr_lvl,file.path(inputs_dir, '16g_firm_yr_level_summ_stats_inputs.parquet'))
rm(list= setdiff(ls(), base_env)); gc()
# generate firm level dataset -------------------------------------------------
revenue_vars = c('turnover', 'dom_turnover', 'value_bs_export', 'value_customs_export')
comp_vars =   gpaste(c("quartile_", "quartile_share_", "", "share_"), "comp_data")
num_cores = detectCores()-2

### generate chunked versions of the data for faster processing 
chunked_data = import_file(
  file.path(inputs_dir, '16g_firm_yr_level_summ_stats_inputs.parquet'),
  col_select = c('firmid', 'year', revenue_vars, 'age', 'currently_export', comp_vars, "NACE_BR")) %>%
  .[, (comp_vars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = comp_vars] %>% 
  .[, chunk := .GRP %% num_cores + 1, by = firmid] %>%
  split(., .[['chunk']])
for (i in seq_along(chunked_data)) write_parquet(chunked_data[[i]],paste0('1) data/temp_chunk',i,'.parquet'))

## run the cluster
cl <- makeCluster(num_cores); clusterExport(cl,c(base_env,"type",'revenue_vars'));
clusterEvalQ(cl, {lapply(packages, library, character.only = T)})
firm_level_variation = rbindlist(parLapply(cl,1:num_cores, function(i){
  base = import_file(paste0('1) data/temp_chunk',i,'.parquet'))
  
  # set the NACE_BR for the firm as the NACE value it most commonly appears as 
  NACE_breakdown =  base[,.(count = .N), by = .(NACE_BR, firmid)] %>% 
    arrange(-count) %>% distinct(firmid,.keep_all = T) %>%
    select(firmid, NACE_BR) 


  ## generate the firm level variable creation code (input is a placeholder)
  base_command = paste0("variance_input = var(input, na.rm =T),",
                        "de_trended_variance_input = sub_regression(input,year, ssr = T)") %>%
    paste0(.,", ", gsub("\\(input","(asinh(input)", gsub("_input", '_log_input',.)))
  var_command = lapply(revenue_vars, function(var){ gsub('input', var, base_command)}) %>% 
    unlist() %>% paste(.,collapse = ",")
  quartile_command = 'max_quartile_comp_data = as.factor(round(NA_max(quartile_comp_data),0))' %>% c(., gsub('max','median',.)) %>%
    c(., gsub('comp', 'share_comp',.))
  misc_command = 'log_min_sample_age = asinh(NA_min(age)), log_years_exported = asinh(sum(currently_export)), log_sample_years_observed = asinh(.N), mean_log_comp_data = asinh(NA_mean(comp_data)),
  mean_share_comp_data = NA_mean(share_comp_data)'
  command = c(var_command, quartile_command, misc_command) %>%  paste(., collapse = ", ")
    
  # execute
  command = paste0('firm_level_variation = base[,.(', command,"), by = firmid]")
  eval(parse(text = command))
  firm_level_variation = merge(firm_level_variation,  NACE_breakdown, by = 'firmid', all.x =T)
  file.remove(paste0('1) data/temp_chunk',i,'.parquet'))
  return(firm_level_variation)
}))
stopCluster(cl)

write_parquet(firm_level_variation,file.path(inputs_dir, '16h_firm_level_summ_stats_inputs.parquet'))
rm(list= setdiff(ls(), base_env)); gc()
# generate the export-ctry year level dataset -----------------------------
## define varlists 
firm_level_vars = c('firmid','year', 'NACE_BR', 'dom_turnover', 'age','markets_export','years_since_first_export',
                    gpaste(c("", "quartile_"),c("", "share_"), "comp_data"))

customs_vars = c('firmid', 'ctry','streak_id', 'exim', 'products', 'deflated_value', 'streak_start', 'year_in_streak', 'year')

streak_id_vars = c('firmid','ctry','streak_id'); # note that in the non-dummy version of this streak id should be fine
growth_vars = c('deflated_value','products')
log_vars = c(gpaste(c(paste0("first_", c("streak", "ctry", "export"), "_yr_"),""), growth_vars),
             'dom_turnover', 'age', paste0(c('', "num_other_"),'markets_export'),'comp_data', 'market_size',
             paste0('years_since_',c('ctry', 'export'), "_entry"),
             'streak_age')

firm_level_data = import_file(file.path(inputs_dir, '16g_firm_yr_level_summ_stats_inputs.parquet'),
                              col_select = firm_level_vars, char_vars = 'firmid') %>% 
                  rename(years_since_export_entry = years_since_first_export)

birth_data = import_file(file.path(inputs_dir, '16b_complete_birth_data.parquet'), col_select = c('firmid', 'birth_year','first_export_year'),
            char_vars = 'firmid')

#### Import the export data and merge everything together 
export_data = import_file('1) data/9_customs_cleaned.csv', col_select = customs_vars, char_vars = 'firmid') %>%
  distinct(firmid, year,exim, ctry, .keep_all = TRUE)  %>% 
  .[exim == 2] %>% .[,exim := NULL] %>% 
   merge(birth_data, all.x = T) %>%
  .[is.na(birth_year) | (streak_start == 1993 & birth_year != 1993) | birth_year > streak_start, c("streak_start", "year_in_streak") := NA] %>% 
  .[,streak_age := year_in_streak - 1] %>% .[,year_in_streak := NULL] %>% # make streak_age match firm_age def 
  
  #### generate streak vars
  unbalanced_growth_rate(., streak_id_vars, 'year', growth_vars, c(-1,0), 'streak_start', expand = T) %>% 
  rename_with(.cols = contains('growth'), ~paste0("streak_", .)) %>%
  .[, streak_ends := year == max(year), by = streak_id_vars] %>% 
  
  
  ## fill in gaps in streaks to generate overall behavior variables 
  merge(.[, .(year = seq(min(year), max(year))), by = .(firmid,ctry)], all =T, by = c('firmid', 'ctry','year')) %>%
  .[, (growth_vars) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = growth_vars] %>% # na_replace values
  .[,cey := min(year), by = .(firmid, ctry)] %>%  
  .[,cey := case_when(is.na(birth_year) | (cey==1993 & birth_year !=1993)~NA,birth_year <= cey ~ cey, T~ NA)] %>%
  rename(ctry_entrance_yr = cey) %>% 
  .[,years_since_ctry_entry := year -  ctry_entrance_yr] %>% 
   unbalanced_growth_rate(., c('firmid','ctry'), 'year', growth_vars, c(-1,0), 'ctry_entrance_yr', expand = T) %>%
  
  # add in firm level  data
  merge(firm_level_data, by = c('firmid', 'year')) %>%
  .[,`:=`(ctry_NACE_BR = paste0(ctry, NACE_BR), num_other_markets_export = markets_export - 1)]  %>%
    
  # add the "first" variables 
  .[,first_ctry_streak := ctry_entrance_yr  == streak_start & !is.na(ctry_entrance_yr) ] %>% 
  .[,first_export_streak := first_export_year == streak_start & !is.na(first_export_year)] %>% 
  mutate(across(growth_vars, ~ifelse(year == streak_start,.,NA), .names = 'first_streak_yr_{col}'),
         across(growth_vars, ~ifelse(year == streak_start & first_ctry_streak,.,NA), .names = 'first_ctry_yr_{col}'),
         across(growth_vars, ~ifelse(year == streak_start & first_export_streak,.,NA), .names = 'first_export_yr_{col}')) %>% 
  
  # add in the size of the industry overall in that year as an interaction term 
  merge(.[,.(market_size = NA_sum(deflated_value)), by = .(NACE_BR, year, ctry)] %>% na.omit(), 
        by = c('NACE_BR', 'ctry', 'year'), all.x = T) %>% 
  
  ## add in log vars 
  .[, paste0("log_", log_vars) := lapply(.SD, asinh), .SDcols = log_vars]
  
write_parquet(export_data,file.path(inputs_dir, '16i_export_firm_ctry_level_summ_stats_inputs.parquet'))
rm(list= setdiff(ls(), base_env)); gc()








