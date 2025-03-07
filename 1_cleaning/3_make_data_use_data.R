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

## merge together
subset_vars = c('french', 'engineer','data', 'rnd','stem')
bs_br_linkedin = import_file('1) data/3_bs_br_data.csv', char_vars =  c('firmid')) %>% 
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
write_parquet(bs_br_linkedin, '1) data/16_bs_br_linkedin.parquet')
# generate complete birth data ------------------------------------------
# admin_birth_data = import_file('../../IWH/data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv', char_vars = 'siren',
#                col_select = c("siren", "dateDebut", "etatAdministratifUniteLegale")) %>%
#  .[, `:=`(start_year = year(dateDebut), firmid = siren)] %>%
#  .[etatAdministratifUniteLegale != 'C' & !is.na(start_year) & start_year <= 2024 & start_year > 1901]  %>%
#  .[, .(birth_year_admin = min(start_year)), by = firmid]
# write_parquet(admin_birth_data, '1) data/14_admin_birth_data.parquet')
birth_data = import_parquet('1) data/14_admin_birth_data.parquet')  %>% 
  
  merge(import_file('1) data/16_bs_br_linkedin.parquet', col_select = c('firmid', 'year'), char_vars = 'firmid') %>%
          .[!is.na(year) & !is.na(firmid)] %>%
          .[,.(birth_year_BS = min(year)), by = firmid] %>%
          .[birth_year_BS == 1994, birth_year_BS := NA] #94 is the first year of our data 
        , all = T) %>%
  
  merge(import_csv('1) data/9_customs_cleaned.csv', char_vars =  c('firmid'), col_select = c('year', 'firmid')) %>%
          .[!is.na(year) & !is.na(firmid)] %>%
          .[,.(birth_year_customs = min(year)), by = firmid] %>%
          .[birth_year_customs == 1993, birth_year_customs := NA] #93 is the first year of our data 
        , all = T) %>%
  
  .[,birth_year := case_when(
    birth_year_BS <= birth_year_customs ~ birth_year_BS,
    !is.na(birth_year_BS) & is.na(birth_year_customs) ~ birth_year_BS,
    T ~ birth_year_admin)] %>% 
  
  .[,type := case_when(birth_year == birth_year_BS ~ "BS",
                       birth_year == birth_year_admin ~ "admin",
                       T ~ "missing")]
write_parquet(birth_data,'1) data/17_complete_birth_data.parquet')




# generate customs data ---------------------------------------------------
sim_vars = 'french_distance'
french_sim = read_rds('1) data/similarity_matrices/outputs/similiarity_data.rds') %>% filter(ctry == 'FR')
french_distances = fread('1) data/similarity_matrices/outputs/france_distance_data.csv') %>%
  rename(french_distance = distance_to_france) %>% 
  mutate(french_distance = 1-french_distance/max(french_distance,na.rm =  T))

customs_data = import_csv('1) data/9_customs_cleaned.csv', char_vars = 'firmid') %>% 
  unbalanced_lag(., c("firmid", 'exim', 'ctry'), "year", "deflated_value", c(-1,1)) %>% 
  merge(french_distances, all.x= T) %>% 
  mutate(flow_type = ifelse(exim ==1, 'import', 'export'),
         
         # define market entry terms 
         new_markets = is.na(deflated_value_lag1),
         market_entry_failures = case_when((!new_markets| year == max(year,na.rm = T)) ~ NA,
                                           is.na(deflated_value_lead1) ~ T,
                                           T~F)) %>% 
  
  ## collapse down to firmid, year, flow_type 
  group_by(firmid, year, flow_type) %>%  rename(value = deflated_value) %>% 
  mutate(share = value / NA_sum(value), intermarket_hhi = share^2) %>% 
  summarize(across(c('new_markets', 'market_entry_failures', 'value', 'intermarket_hhi'), ~NA_sum(.)), 
            currently = as.numeric(any(value != 0)), 
            markets = n(), 
            across(sim_vars, ~NA_sum(1-.),.names = "{col}_wgted_markets"),
            across(sim_vars, ~NA_sum(value *(1-.)), .names = "{col}_wgted_value"), .groups = 'drop') %>% 
  mutate(market_entry_failure_rate = ifelse(new_markets ==0,NA, market_entry_failures/ new_markets)) %>%
  
  ## redo the collapse to be at firmid, year level 
  pivot_wider(id_cols = c(firmid,year), names_from = flow_type, values_from =
                c('currently','markets','new_markets', 'market_entry_failures', 
                  'market_entry_failure_rate', 'value','intermarket_hhi',
                  gpaste(sim_vars, c('_wgted_markets', '_wgted_value'))))

write_parquet(customs_data,'1) data/18_customs_for_data_summ_stats.parquet')

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
  # fwrite(nace_code, '1) data/19_nace_code_breakdown.csv')
}

nace_code = import_file('1) data/19_nace_code_breakdown.csv') %>%
  select(paste0('nace_', 1:4)) %>% 
  mutate(NACE_BR = nace_4) %>% 
  filter(!is.na(nace_4))


industry_data = import_file('1) data/16_bs_br_linkedin.parquet', char_vars = 'firmid') %>%
  merge(import_file('1) data/17_complete_birth_data.parquet', char_vars = 'firmid', col_select = c('firmid', 'birth_year')), all.x =T) %>%
  merge(nace_code, by = 'NACE_BR',all.x = T)

industry_data = lapply(1:4, function(level){
  temp_nace_code =  import_file('1) data/18_nace_code_breakdown.csv') %>% 
    select(names(.) %>% .[grepl(paste(1:level,collapse= "|"),.)]) %>% unique()
  temp_nace_code$nace_code = as.character(temp_nace_code[[paste0('nace_',level)]])
  temp_nace_code= temp_nace_code %>% filter(nace_code != "")
  
  temp_industry_data = industry_data %>% 
    mutate(nace_code = as.factor(.data[[paste0("nace_", level)]])) %>%
    .[!is.na(nace_code) & year>2008]
  
  ## generate stats on volatility in interest period (2008 ->)
  overall_vol = temp_industry_data[year>2008] %>% 
    .[!is.na(deflated_turnover)] %>% 
    .[, .(mean = mean(deflated_turnover), count = .N), by = .(nace_code, year)] %>% 
    .[, .(industry_sd = sd(mean),
          industry_coef_variation = sd(mean)/mean(mean),
          num_period_observations = sum(count)), by = .(nace_code)] %>%
    merge(expand(min(temp_industry_data$year):max(temp_industry_data$year), .[['nace_code']], names = c('year', 'nace_code')))
 
  ## generate summary stats on churn 
  churn_summary = temp_industry_data %>%
    .[, last_seen := max(year), by = firmid] %>%
    .[year<= NA_max(year) & !is.na(birth_year)] %>%
    .[, .(entrance_share = sum(birth_year == year)/ .N,
          exit_share = sum(last_seen == year)/ .N,
          num_yearly_observations = .N), by = .(nace_code, year)] %>%
    .[, churn_share := entrance_share + exit_share]
  
  ## generate summary stats on data use
  varlist = c('comp_data', 'share_comp_data','data_to_turnover')
  mean_command = gpaste('mean_',varlist,rep('=mean(',length(varlist)) ,varlist, ")",no_expand = T, collapse_str = ', ')
  data_summary = temp_industry_data %>% .[!is.na(comp_data)] %>% .[,data_to_turnover := comp_data /turnover] 
  command = paste0("data_summary = data_summary[,.(", mean_command, ",",
                   gsub('mean', 'sd', mean_command),"), by = .(nace_code, year)]") %>%
    gsub('to_turnover=', 'to_turnover=NA_',.) 
  eval(parse(text = command))
  
  temp_output = merge(churn_summary, overall_vol, by = c('year', 'nace_code'), all = T) %>%
    merge(data_summary, by = c('year', 'nace_code'), all = T) %>% 
    merge(temp_nace_code, by = 'nace_code',all.x = T) %>%
    mutate(nace_level = level) %>%
    select(nace_level, nace_code, year, num_yearly_observations, num_period_observations, everything())
}) %>% rbindlist(fill = T, use.names = T)
write_parquet(industry_data,'1) data/20_data_x_industry_stats.parquet')

## generate a censored version for export 
org_vars = c(names(industry_data)[1:3], names(industry_data) %>% .[grepl('desc',.)])
period_vars = c('industry_sd', 'industry_coef_variation','num_period_observations')
yearly_vars = setdiff(names(industry_data), c(org_vars, period_vars))
industry_data_censored = industry_data %>% 
  mutate(across(period_vars, ~ifelse(num_period_observations < 5, NA,.)),
         across(yearly_vars, ~ifelse(num_yearly_observations < 5, NA,.))) %>%
  filter(!is.na(num_yearly_observations) | !is.na(num_period_observations))
write_parquet(industry_data_censored,'1) data/20a_data_x_industry_stats_censored.parquet')



#generate the combined dataset used in summary stats ---------------------
# define variable groups
{
  comp_vars = c("comp_total", "comp_engineer", "comp_data", "comp_rnd", "comp_stem")
  bs_vars = c('turnover', 'empl', 'age','tfp','cost_per_worker_linkedin', 'cost_per_worker_bs')
  industry_vars = c('entrance_share', 'exit_share','churn_share',
                    'industry_volatility', 'size_wgted_industry_volatility')
  
  customs_discrete_vars = gpaste(c('currently', 'ever', 'market_entry_failure_rate','intermarket_hhi'), c('_export', '_import'), order = c(2:1))
  customs_cont_vars = gpaste(c('markets', 'new_markets', 'market_entry_failures', 'value_customs', 'value_bs'),
                             c('_export', '_import'), order = c(2,1)) %>% gsub("(?:customs|bs)_import", "import", .) %>% unique()
  customs_to_zero = c(customs_discrete_vars, customs_cont_vars) %>% 
    gsub("(?:import|export)", "", .) %>% unique() %>% .[!grepl('ever|value_bs|hhi|failure_rate',.)]
  
  log_vars = c(comp_vars, bs_vars, customs_cont_vars)
}  

industry_data = import_file('1) data/20_data_x_industry_stats.parquet') %>%
  filter(nace_level == 4) %>% 
  select(-contains('desc'),-contains('observation'),-nace_level) %>%
  rename(NACE_BR = nace_code) %>% mutate(NACE_BR = as.numeric(NACE_BR)) %>% 
  rename_with(.cols = -c(contains('nace'),'year'), ~paste0('industry_',.)) %>%
  rename_with(~gsub('industry_industry', 'industry',.))
birth_data = import_file('1) data/17_complete_birth_data.parquet', char_vars = 'firmid', col_select = c('firmid', 'birth_year'))

summary_stats_data = import_file('1) data/16_bs_br_linkedin.parquet', char_vars = "firmid") %>%
  merge(import_file('1) data/18_customs_for_data_summ_stats.parquet'), all.x = T) %>% 
  rename(value_customs_export = value_export, value_bs_export = for_turnover) %>% 
  mutate(across(intersect(names(.),paste0(customs_to_zero,'export')), ~ ifelse(is.na(markets_export), 0, .)),
         across(intersect(names(.),paste0(customs_to_zero,'import')), ~ ifelse(is.na(markets_import), 0, .))) %>%
  group_by(firmid) %>% 
  mutate(ever_export = as.numeric(any(markets_export !=0)),
         ever_import = as.numeric(any(markets_import != 0))) %>%
  ungroup() %>%  as.data.table() %>% .[year >= 2008] %>% 
  
  # merge in birth / industry data
  merge(birth_data, all.x = T) %>% mutate(age = year - birth_year) %>% 
  merge(industry_data, by= c('NACE_BR','year'), all.x = T) %>% 
  
  ## add in quartile variables
 .[, paste0("quartile_", paste0(c("", "share_"), "comp_data")) := 
     lapply(.SD, function(x) as.factor(ntile(x, 4))), 
   .SDcols = paste0(c("", "share_"), "comp_data"), by = .(NACE_BR, year)] %>% 
  
  ## log variables 
  .[, paste0("log_", log_vars) := lapply(.SD, asinh), .SDcols = log_vars]

write_parquet(summary_stats_data,'1) data/21_data_summ_stats_input.parquet')

  

