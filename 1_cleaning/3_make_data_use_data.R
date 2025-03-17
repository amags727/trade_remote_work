# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')}
source("2) code/00_helper_functions.R")
inputs_dir = ('1) data/16_inputs_for_data_summary_stats')
dir.create(inputs_dir)
helper_functions = ls() 

# generate linkedin / bs_br data --------------------------------------------------
linkedin = import_file('1) data/15_french_affiliated_firm_roles_collapsed_clean.parquet') %>%
  select(-c(rcid, `__index_level_0__`)) 

## if necessary anonymize linkedin data 
dummy_version = grepl("/Users/amagnuson/Library/CloudStorage/", getwd()); set.seed(1)
if (dummy_version){
  linkedin = merge(linkedin, data.table(firmid = linkedin[!duplicated(firmid) & has_siren][['firmid']]) %>%
                     mutate(firmid_new =  as.character(sample(seq_len(nrow(.)), size = nrow(.), replace = FALSE)),
                            replace = sample(c(0,1), size = nrow(.), replace = T)),
                   all.x = T, by = 'firmid') %>%
    .[!is.na(firmid_new) & replace == 1, firmid := firmid_new] %>% select(-firmid_new, replace)
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

# generate complete birth data ------------------------------------------
# admin_birth_data = import_file('../../IWH/data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv', char_vars = 'siren',
#                col_select = c("siren", "dateDebut", "etatAdministratifUniteLegale")) %>%
#  .[, `:=`(start_year = year(dateDebut), firmid = siren)] %>%
#  .[etatAdministratifUniteLegale != 'C' & !is.na(start_year) & start_year <= 2024 & start_year > 1901]  %>%
#  .[, .(birth_year_admin = min(start_year)), by = firmid]
# write_parquet(admin_birth_data, '1) data/14_admin_birth_data.parquet')
birth_data = import_parquet('1) data/14_admin_birth_data.parquet')  %>% unique() %>% 
  
  ## note year of first appearence in bs /br 
  merge(import_file(file.path(inputs_dir, '16a_bs_br_linkedin.parquet'), col_select = c('firmid', 'year'), char_vars = 'firmid') %>%
          unique() %>%
          .[!is.na(year) & !is.na(firmid)] %>%
          .[,.(birth_year_BS = min(year)), by = firmid] %>%
          .[birth_year_BS == 1994, birth_year_BS := NA] #94 is the first year of our data 
        , all = T) %>%
  
  ## note year of first appearence in customs  
  merge(import_csv('1) data/9_customs_cleaned.csv', char_vars =  c('firmid'), col_select = c('year', 'firmid','exim')) %>%
          unique() %>% .[,min_exim_year := min(year), by = .(firmid,exim)] %>% 
          pivot_wider(id_cols = c(year,firmid), names_from = exim, values_from = min_exim_year, names_prefix = 'exim') %>% 
          group_by(firmid) %>% summarize(
            birth_year_customs = min(year), 
            first_import_year = NA_min(exim1),
            first_export_year = NA_min(exim2)) %>% 
          
          # before we remove 1993 as a valid year, note if they ever imported / exported 
          mutate(ever_import = !is.na(first_import_year),
                 ever_export = !is.na(first_export_year), 
          across(-firmid, ~ifelse(.==1993,NA_real_, .))) #93 is the first year of our data 
        , all = T) %>%
  
  .[,birth_year := case_when(
    birth_year_BS <= birth_year_customs ~ birth_year_BS,
    !is.na(birth_year_BS) & is.na(birth_year_customs) ~ birth_year_BS,
    birth_year_admin <= birth_year_customs ~ birth_year_admin,
    !is.na(birth_year_admin) & is.na(birth_year_customs) ~ birth_year_admin,
   !is.na(birth_year_customs) & is.na(birth_year_admin) & is.na(birth_year_BS) ~ birth_year_customs,
   T ~ NA_real_)] %>% 
  
  .[,`:=`(type = case_when(birth_year == birth_year_BS ~ "BS",
                       birth_year == birth_year_admin ~ "admin",
                       T ~ "missing"),
          years_alive_before_export = first_export_year - birth_year,
          years_alive_before_import = first_import_year - birth_year)] 

write_parquet(birth_data,file.path(inputs_dir, '16b_complete_birth_data.parquet'))




# generate customs data ---------------------------------------------------
sim_vars = 'french_distance'
french_sim = read_rds('1) data/similarity_matrices/outputs/similiarity_data.rds') %>% filter(ctry == 'FR')
french_distances = fread('1) data/similarity_matrices/outputs/france_distance_data.csv') %>%
  rename(french_distance = distance_to_france) %>% 
  mutate(french_distance = 1-french_distance/max(french_distance,na.rm =  T))

customs_data = import_csv('1) data/9_customs_cleaned.csv', char_vars = 'firmid') %>% 
  distinct(firmid, year,exim, ctry, .keep_all = TRUE) %>% 
  unbalanced_lag(., c("firmid", 'exim', 'ctry'), "year", "deflated_value", c(-1,1)) %>% 
  merge(french_distances, all.x= T) %>% 
  mutate(flow_type = ifelse(exim ==1, 'import', 'export'),
         
         # define market entry terms 
         new_markets = is.na(deflated_value_lag1),
         market_entry_failures = case_when((!new_markets| year == max(year,na.rm = T)) ~ NA,
                                           is.na(deflated_value_lead1) ~ T,
                                           T~F)) %>% 
  
  ## collapse down to firmid, year, flow_type 
  group_by(firmid, year, flow_type) %>%  rename(value_customs = deflated_value) %>% 
  mutate(share = value_customs / NA_sum(value_customs), intermarket_hhi = share^2) %>% 
  summarize(across(c('new_markets', 'market_entry_failures', 'value_customs', 'intermarket_hhi'), ~NA_sum(.)), 
            currently = as.numeric(any(value_customs != 0)), 
            markets = n(), 
            across(sim_vars, ~NA_sum(1-.),.names = "{col}_wgted_markets"),
            across(sim_vars, ~NA_sum(value_customs *(1-.)), .names = "{col}_wgted_value_customs"), .groups = 'drop') %>% 
  mutate(market_entry_failure_rate = ifelse(new_markets ==0,NA, market_entry_failures/ new_markets))  %>% 
  
  ## redo the collapse to be at firmid, year level 
  pivot_wider(id_cols = c(firmid,year), names_from = flow_type, values_from =
                c('currently','markets','new_markets', 'market_entry_failures', 
                  'market_entry_failure_rate', 'value_customs','intermarket_hhi',
                  gpaste(sim_vars, c('_wgted_markets', '_wgted_value_customs'))))

write_parquet(customs_data,file.path(inputs_dir, '16c_customs_for_data_summ_stats.parquet'))

# generate the industry level data ----------------------------------------
#import and process nace version 2 codes 
{
  #  nace_code <- import_file("https://gist.githubusercontent.com/b-rodrigues/4218d6daa8275acce80ebef6377953fe/raw/99bb5bc547670f38569c2990d2acada65bb744b3/nace_rev2.csv") %>%
  #    rename_with(~tolower(.)) %>% mutate(code = gsub("\\.", "",code)) %>%
  #    select(level, code,description)
  # 
  #  for (lev in 1:4){
  #    command = paste0(
  #      "nace_code = nace_code %>% mutate(nace_",lev,
  #      " = ifelse(level == ", lev,", code, NA), nace_descrip_",lev,
  #      " = ifelse(level == ", lev,", description, NA))", 
  #      ifelse(lev != 1, paste0(" %>% group_by(nace_",lev-1,")"), ""),
  #      ifelse(lev != 4,gpaste(" %>% fill(nace_", c("", "descrip_"), lev, ", .direction = 'down')",
  #             collapse_str = ""), "")
  #      )
  #    eval(parse(text = command))
  #  }
  #    nace_code = nace_code %>%
  #    mutate(nace_descrip_short_1 = case_when(
  #      nace_1 == "A" ~ "AGRICULTURE",
  #      nace_1 == "B" ~ "MINING",
  #      nace_1 == "C" ~ "MANUFACTURING",
  #      nace_1 == "D" ~ "UTILITIES",
  #      nace_1 == "E" ~ "WATER MANAGEMENT",
  #      nace_1 == "F" ~ "CONSTRUCTION",
  #      nace_1 == "G" ~ "WHOLESALE /\n RETAIL TRADE",
  #      nace_1 == "H" ~ "TRANSPORTATION",
  #      nace_1 == "I" ~ "ACCOMMODATION /\n FOOD SERV.",
  #      nace_1 == "J" ~ "INFO /\nCOMMUNICATION",
  #      nace_1 == "K" ~ "FINANCE /\nINSURANCE",
  #      nace_1 == "L" ~ "REAL ESTATE",
  #      nace_1 == "M" ~ "PROFESSIONAL,\nSCIENCE + TECH",
  #      nace_1 == "N" ~ "ADMIN",
  #      nace_1 == "O" ~ "PUBLIC ADMIN + DEFENCE",
  #      nace_1 == "P" ~ "EDUCATION",
  #      nace_1 == "Q" ~ "HHS",
  #      nace_1 == "R" ~ "ARTS + ENTERTAINMENT",
  #      nace_1 == "S" ~ "OTHER SERVICES",
  #      nace_1 == "T" ~ "HOUSEHOLD ACTIVITY",
  #      nace_1 == "U" ~ "EXTRATERRITORIAL ORGS",
  #      TRUE ~ NA_character_),
  #      
  #      nace_descrip_short_2 = case_when(
  #      nace_2 == 58 ~ "Publishing",
  #      nace_2 == 59 ~ "Video + Music /nProduction",
  #      nace_2 == 60 ~ "Programming/\nBroadcast",
  #      nace_2 == 61 ~ "Telecom",
  #      nace_2 == 62 ~ "Computer programming",
  #      nace_2 == 63 ~ "Information Service",
  #      nace_2 == 69 ~ "Legal and Accounting",
  #      nace_2 == 70 ~ "Consultancy",
  #      nace_2 == 71 ~ "Architectural \n Engineering",
  #      nace_2 == 72 ~ "R&D",
  #      nace_2 == 73 ~ "Advertising\nmarket research",
  #      nace_2 == 74 ~ "Other",
  #      nace_2 == 75 ~ "Veterinary",
  #      TRUE ~ nace_descrip_2), 
  #      
  #      nace_descrip_short_3 =  nace_descrip_3,
  #      nace_descrip_short_4 = nace_descrip_4)
  # 
  # fwrite(nace_code, file.path(inputs_dir, '16d_nace_code_breakdown.csv'))
}

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



#generate the firm-year level dataset  ---------------------
## define variable groups 
growth_vars = c('turnover', 'dom_turnover', gpaste(c('value_bs', 'value_customs', 'markets'), '_export'))
birth_vars = c('firmid', 'birth_year','first_import_year', 'first_export_year',
               gpaste(c('ever','years_alive_before'),c('_import', '_export')))
customs_to_zero= c('currently','markets', 'new_markets', 'market_entry_failures', 'value_customs', 'currently')
log_vars = c(growth_vars,'empl','age','cost_per_worker_linkedin', 'cost_per_worker_bs', 'comp_data',
             paste0('years_since_first_', c('export', 'import')))

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
  mutate(age = year - birth_year,
         years_since_first_import = year - first_import_year,
         years_since_first_export = year - first_export_year,
         across(contains('years_since_first'), ~ifelse(.<0, NA, .))) %>% 
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
#generate firm level dataset -------------------------------------------------
revenue_vars = c('turnover', 'dom_turnover', 'value_bs_export', 'value_customs_export')
comp_vars =   gpaste(c("quartile_", "quartile_share_", "", "share_"), "comp_data")
num_cores = detectCores()-2
chunked_data = import_file(
  file.path(inputs_dir, '16g_firm_yr_level_summ_stats_inputs.parquet'),
  col_select = c('firmid', 'year', revenue_vars, 'age', 'currently_export', comp_vars, "NACE_BR")) %>%
  .[, (comp_vars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = comp_vars] %>% 
  .[, chunk := .GRP %% num_cores + 1, by = firmid] %>%
  split(., .[['chunk']])
for (i in seq_along(chunked_data)) write_parquet(chunked_data[[i]],paste0('1) data/temp_chunk',i,'.parquet'))


cl <- makeCluster(num_cores); clusterExport(cl,c(helper_functions,"type",'revenue_vars'));
clusterEvalQ(cl, {lapply(packages, library, character.only = T)})
firm_level_variation = rbindlist(parLapply(cl,1:num_cores, function(i){
  base = import_file(paste0('1) data/temp_chunk',i,'.parquet'))
  NACE_breakdown =  base[,.(count = .N), by = .(NACE_BR, firmid)] %>% 
    arrange(-count) %>% distinct(firmid,.keep_all = T) %>%
    select(firmid, NACE_BR)


  ## generate the firm level variable creation code 
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


# Generate the export-ctry year level dataset -----------------------------

firm_level_vars = c('firmid','year', 'NACE_BR', 'dom_turnover', 'age','markets_export','years_since_first_export',
                    gpaste(c("", "quartile_"),c("", "share_"), "comp_data"))
customs_vars = c('firmid', 'ctry','streak_id', 'exim', 'products', 'deflated_value', 'streak_start', 'year_in_streak', 'year',
                 'streak_birth_observed')
growth_vars = c('deflated_value','products')
log_vars = c('dom_turnover', 'age', 'markets_export','years_since_first_export', 'comp_data', 
             'deflated_value')

firm_level_data = import_file(file.path(inputs_dir, '16g_firm_yr_level_summ_stats_inputs.parquet'),
                              col_select = firm_level_vars, char_vars = 'firmid')

#### Import the export data and merge everything together 
export_data = import_file('1) data/9_customs_cleaned.csv', col_select = customs_vars, char_vars = 'firmid') %>%
  distinct(firmid, year,exim, ctry, .keep_all = TRUE)  %>% 
  .[exim == 2] %>% 
  .[streak_birth_observed == 0, c("streak_start", "year_in_streak") := NA] %>% 
  
  #### generate growth rates 
  unbalanced_growth_rate(., c("firmid", 'ctry'), 'year', growth_vars, c(-1,0), 'streak_start', expand = T) %>% 
  unbalanced_growth_rate(., c("firmid", 'ctry'), 'year', growth_vars, c(0,1), 'streak_start', expand = T, alt_suffix = "_growth_rate_lead1") %>%
  unbalanced_growth_rate(., c("firmid", 'ctry'), 'year', growth_vars, c(-1,1), 'streak_start', expand = T, alt_suffix = "_growth_rate_2yr") 

  export_data = export_data %>%
  
  ## generate indicator for whether this is the firm's first time in the country 
  merge(export_data %>% distinct(firmid, ctry, streak_start) %>%
          .[,.(first_observed_ctry_entrance_yr = NA_min(streak_start)), by = .(firmid, ctry)]) %>%
  .[, first_observed_ctry_streak := 
      case_when(streak_start == first_observed_ctry_entrance_yr & streak_birth_observed ~ T, 
                !streak_birth_observed ~ NA,
                T ~ F)] %>%
 
  # add in firm level  data
  merge(firm_level_data, by = c('firmid', 'year')) %>% 
  
  # add in the size of the industry overall in that year as an interaction term 
  merge(.[,.(market_size = NA_sum(deflated_value)), by = .(NACE_BR, year, ctry)] %>% na.omit(), 
        by = c('NACE_BR', 'ctry', 'year'), all.x = T) %>% 
  
  ## add in log vars 
  mutate(across(log_vars, ~asinh(.), .names = "log_{col}"))
  
write_parquet(export_data,file.path(inputs_dir, '16i_export_firm_ctry_level_summ_stats_inputs.parquet'))
