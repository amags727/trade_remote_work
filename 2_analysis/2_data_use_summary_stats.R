# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../00_helper_functions.R")
setwd('../..')

dummy_version = T
# import/ process datasets -----------------------------------------------------------------------
linkedin = import_parquet('1) data/14_WRDS_output/linkedin/french_affiliated_firm_roles_collapsed_clean.parquet') %>%
  select(-c(rcid, `__index_level_0__`)) 

## if necessary anonymize linkedin data 
if (dummy_version){
  linkedin = merge(linkedin, data.table(firmid = linkedin[!duplicated(firmid) & has_siren][['firmid']]) %>%
                     mutate(firmid_new =  as.character(sample(seq_len(nrow(.)), size = nrow(.), replace = FALSE)),
                            replace = sample(c(0,1), size = nrow(.), replace = T)),
                   all.x = T, by = 'firmid') %>%
    .[!is.na(firmid_new) & replace == 1, firmid := firmid_new] %>% select(-firmid_new, replace)
}

data =data.frame(firmid = '2', exim = 1, ctry = c(rep('GB', 4), "HK"), year = c(2012:2015, 2014), deflated_value = 10) %>%
  unbalanced_lag(., c("firmid", 'exim', 'ctry'), "year", "deflated_value", c(-1,1)) %>% 
  mutate(new_market = is.na(deflated_value_lag1),
         market_entry_failure = case_when((!new_market| year == max(year,na.rm = T)) ~ NA, is.na(deflated_value_lead1) ~ T, T~F))
      

  
  

# import / clean customs data 
{
sim_vars = c(gpaste(c('french_', 'export_'), c('region', 'language', 'border')), 'french_distance')
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
                                          T~F),
         # define gravity terms 
         french_border = ctry %in% french_sim$share_border[[1]],
         french_region = ctry %in% french_sim$share_region[[1]],
         french_language = ctry %in% french_sim$share_language[[1]]) %>% 
        
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
}

# define variable groups
{
comp_vars = c("comp_total", "comp_engineer", "comp_data", "comp_rnd", "comp_stem")
bs_vars = c('cost_per_worker_linkedin', 'cost_per_worker_bs', 'tfp', 'turnover', 'empl')


customs_discrete_vars = gpaste(c('currently', 'ever', 'market_entry_failure_rate','intermarket_hhi'), c('_export', '_import'), order = c(2:1))
customs_cont_vars = gpaste(c('markets', 'new_markets', 'market_entry_failures', 'value_customs', 'value_bs'),
                           c('_export', '_import'), order = c(2,1)) %>% gsub("(?:customs|bs)_import", "import", .) %>% unique()
customs_to_zero = c(customs_discrete_vars, customs_cont_vars) %>% 
  gsub("(?:import|export)", "", .) %>% unique() %>% .[!grepl('ever|value_bs|hhi|failure_rate',.)]
}  

# import bs_br and merge together all the data 
{
bs_br = import_csv('1) data/3_bs_br_data.csv', char_vars =  c('firmid')) %>% 
  
  ## add in the customs data and clean 
  merge(customs_data, all.x = T) %>%
  rename(value_customs_export = value_export, value_bs_export = for_turnover) %>% 
  mutate(across(intersect(names(.),paste0(customs_to_zero,'export')), ~ ifelse(is.na(markets_export), 0, .)),
         across(intersect(names(.),paste0(customs_to_zero,'import')), ~ ifelse(is.na(markets_import), 0, .))) %>%
  group_by(firmid) %>% 
  mutate(ever_export = as.numeric(any(markets_export !=0)),
         ever_import = as.numeric(any(markets_import != 0))) %>%
  ungroup() %>%  as.data.table() %>% .[year >= 2008] %>% 

  # merge linkedin 
  merge(linkedin, all.x = T) %>%
  
 # generate supplementary variables 
  mutate(cost_per_worker_linkedin = comp_total / emp_total,
         cost_per_worker_bs = labor_cost / empl,
         tfp = turnover / (capital^.3*labor_cost^.7),
         across(comp_vars, ~./ comp_total, .names = "share_{col}"),
         across(c(comp_vars, bs_vars, customs_cont_vars),~asinh(.), .names = "log_{col}")) %>% 
  
  ## generate quartiles within industry 
  group_by(NACE_BR, year) %>% 
  mutate(across(paste0(c("","share_"),'comp_data'), ~ as.factor(ntile(.,4)), .names = 'ind_quartile_{col}'))

  comp_vars = setdiff(comp_vars, 'comp_data')
}
# output summary statistics  ----------------------------------------------
#define variations 
{
variations = rbindlist(use.names = T, fill = T, list(
  expand(paste0('log_',c(comp_vars,bs_vars)), 'comp_data', "1a", names = c('dep_var', 'group_var', 'block')),
  
  expand(c(paste0('share_', setdiff(comp_vars, 'comp_total')), paste0('log_',bs_vars)),
         'share_comp_data', '1b', names = c('dep_var', 'group_var','block')),
  
  expand(c(customs_discrete_vars,paste0('log_', customs_cont_vars)), 
         c('comp_data', 'share_comp_data'), '2a',names = c('dep_var', 'group_var', 'block')) %>%
           mutate(block = ifelse(grepl('share',group_var), '2b', block))
  ))
}  

# run variations 
variation_output = list(); all_models = list()
failed_output = list();
for (i in 1:nrow(variations)){
  for (name in names(variations)){assign(name, variations[[name]][i])} 
  command = paste0('feols(data = bs_br,',dep_var, "~ind_quartile_",group_var, "| year + NACE_BR)")       
  print(command); 
  model = tryCatch(
    {eval(parse(text = command))
    },
    error = function(e) {
      return(data.table(counter = i, command = command, reason = e$message))
      } 
  )
  if(!'reason' %in% names(model)){
    temp_output = merge(variations[i] %>% mutate(counter = i),
                        model_to_df(model) %>% mutate(counter = i))
    variation_output = c(variation_output, list(temp_output))
    all_models = c(all_models, list(model))
  }else{
    failed_output = c(failed_output, list(model))
    all_models = c(all_models, list(NA))
  }
 
}
if(length(failed_output) >0) print(rbindlist(failed_output))

# clean 
variation_output = rbindlist(variation_output) %>% 
  mutate(quartile = str_extract(regressor, "\\d+$"),
         coef = as.character(round(coef, 3)),
         coef = case_when(p_val<.01 ~ paste0(coef,"***"),
                          p_val<.05 ~ paste0(coef,"**"),
                          p_val<.10 ~ paste0(coef,"*"),
                          T~ coef),
         p_val = paste0("(", p_val, ")"))  



summary_stats_output = lapply(1:nrow(variations), function(i){
  if (i %in% variation_output$counter){
  for (name in names(variations)){assign(name, variations[[name]][i])} 
  temp =variation_output %>% filter(counter == i)
  temp = as.data.frame(temp %>% select('coef', 'p_val') %>% mutate(space = "") %>% t()) %>%
    mutate(var = c(dep_var, "", "")) %>% select(var, everything()) %>% rename_with(~c('var', 2:4)) %>%
    mutate(block = block, counter = i)
  }
}) %>% rbindlist()

View(summary_stats_output %>% filter(grepl('2', block)))

###







