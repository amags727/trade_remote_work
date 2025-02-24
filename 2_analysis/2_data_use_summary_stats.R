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

# import/ linkedin data, match it to the business registry, generate a dummy dataset for offline work  -----------------------------------------------------------------------
if (!file.exists('1) data/16_bs_br_linkedin.parquet')){
  # prepare for randomization if necessary 
  dummy_version = grepl("/Users/amagnuson/Library/CloudStorage/", getwd()); set.seed(1)
  
  linkedin = import_parquet('1) data/15_french_affiliated_firm_roles_collapsed_clean.parquet') %>%
    select(-c(rcid, `__index_level_0__`)) 
  
  ## if necessary anonymize linkedin data 
  if (dummy_version){
    linkedin = merge(linkedin, data.table(firmid = linkedin[!duplicated(firmid) & has_siren][['firmid']]) %>%
                       mutate(firmid_new =  as.character(sample(seq_len(nrow(.)), size = nrow(.), replace = FALSE)),
                              replace = sample(c(0,1), size = nrow(.), replace = T)),
                     all.x = T, by = 'firmid') %>%
      .[!is.na(firmid_new) & replace == 1, firmid := firmid_new] %>% select(-firmid_new, replace)
  }
  
  ## merge together
  bs_br_linkedin = import_csv('1) data/3_bs_br_data.csv', char_vars =  c('firmid')) %>% 
    merge(linkedin, all.x = T)
  
  #### output both a real and dummy version 
  {
    write_parquet(bs_br_linkedin, '1) data/16_bs_br_linkedin.parquet') 
    
    ### generate the dummy dataset for later use   
    firm_id_threshold = 4
    data = import_file('1) data/16_bs_br_linkedin.parquet') %>%
      select(-c('has_siren', 'rcid_count','needs_collapse')) %>% 
      .[, matched := !is.na(comp_data)] %>%
      .[, count:= .N, by = .(NACE_BR, year, matched)] %>%
      .[count > firm_id_threshold] %>% .[,count := NULL]
    
    num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data)
    year_nace_matched = data[, .(NACE_BR, year, matched)]
    
    # for each obs. randomly assign an id and then draw from the joint distrib of year-NACE industry combinations
    data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T)) %>%
      cbind(.,year_nace_matched[sample(1:num_data_points, num_data_points, T)]) %>% unique()
    
    # for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)
    group_vars = c('NACE_BR', 'year', 'matched')
    discrete_vars = c('public', 'likely_french', 'subsidiary', 'has_lei')
    linkedin_only_vars = names(data) %>% .[grepl('emp_|comp_',.)]
    data_dummy = lapply(c(F,T), function(linkedin_match){
      continuous_vars = setdiff( names(data),c('empl_bucket','replace','group_code', names(data_dummy), discrete_vars)) 
      if(linkedin_match){
        temp = simulate_discrete_vars(data, data_dummy[matched==T], group_vars, discrete_vars)
      } else{
        temp = data_dummy[matched== F] 
        continuous_vars = setdiff(continuous_vars,linkedin_only_vars) 
      }
      temp = simulate_continuous_vars(data, temp, group_vars, continuous_vars)
    }) %>% rbindlist(use.names = T, fill = T)
    
    
    # add any vars still missing
    data_dummy[, empl_bucket := ifelse(empl < 10, "0-10", ifelse(empl < 50, '10-50', ifelse(empl < 200, '50-200', ifelse(empl >=200, '200+', NA))))]
    if(dir.exists('1a) dummy data/')){
      write_parquet(data_dummy, '1a) dummy data/16_bs_br_linkedin.parquet') 
    } else{
      print('dummy data directory doesnt exist')
    }
    }
  
}else{
  bs_br_linkedin =  import_file('1) data/16_bs_br_linkedin.parquet')
}



# carry out the rest of the data cleaning  -----------------------------------------------------------------------
## generate birth year data
{
  #admin_birth_data = import_csv('../../IWH/data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv', char_vars = 'siren',
  #                col_select = c("siren", "dateDebut", "etatAdministratifUniteLegale")) %>%
  #  .[, `:=`(start_year = year(dateDebut), firmid = siren)] %>%
  #  .[etatAdministratifUniteLegale != 'C' & !is.na(start_year) & start_year <= 2024 & start_year > 1900]  %>%
  #  .[, .(birth_year_admin = min(start_year)), by = firmid]
  #write_parquet(admin_birth_data, '1) data/14_admin_birth_data.parquet')
  birth_data = import_parquet('1) data/14_admin_birth_data.parquet')  %>% 
    
    merge(import_csv('1) data/3_bs_br_data.csv', char_vars =  c('firmid'), col_select = c('year', 'firmid')) %>%
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
}

## generate industry characteristics 
{
  industry_data = import_csv('1) data/3_bs_br_data.csv', char_vars =  c('firmid')) %>% 
    merge(birth_data %>% select(firmid, birth_year), all.x =T) %>% 
    .[, last_seen := max(year), by = 'firmid']
  
  industry_data = merge(
    ### generate the churn values
    industry_data[year<= NA_max(year) & !is.na(birth_year) & !is.na(last_seen)] %>% 
      .[, .(entrance_share = sum(birth_year == year)/ .N,
            exit_share = sum(last_seen == year)/ .N,
            num_firms_industry_yr = .N), by = .(NACE_BR, year)] %>%
      .[, churn_share := entrance_share + exit_share],
    
    ### generate the revenue volatility values 
    industry_data[year>=2008] %>% 
      .[, .(firm_variation = NA_sd(deflated_dom_turnover)/ NA_mean(deflated_dom_turnover),
            firm_mean = NA_mean(deflated_dom_turnover)), by = .(firmid, NACE_BR)] %>% 
      group_by(NACE_BR) %>%
      summarize(industry_volatility = NA_mean(firm_variation),
                size_wgted_industry_volatility = NA_sum(firm_variation*firm_mean) / NA_sum(firm_mean)) %>%
      mutate(across(contains('volat'), ~asinh(.), .names = "log_{col}")),
    all = T) %>% 
    
    ## create quartiles for summary stats
    group_by(NACE_BR, year) %>% mutate(across(everything(), ~ntile(.,4), .names = 'quartile_{col}')) %>% ungroup()
}
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
  bs_vars = c('turnover', 'empl', 'age','tfp','cost_per_worker_linkedin', 'cost_per_worker_bs')
  industry_vars = c('entrance_share', 'exit_share','churn_share',
                    'industry_volatility', 'size_wgted_industry_volatility')
  
  customs_discrete_vars = gpaste(c('currently', 'ever', 'market_entry_failure_rate','intermarket_hhi'), c('_export', '_import'), order = c(2:1))
  customs_cont_vars = gpaste(c('markets', 'new_markets', 'market_entry_failures', 'value_customs', 'value_bs'),
                             c('_export', '_import'), order = c(2,1)) %>% gsub("(?:customs|bs)_import", "import", .) %>% unique()
  customs_to_zero = c(customs_discrete_vars, customs_cont_vars) %>% 
    gsub("(?:import|export)", "", .) %>% unique() %>% .[!grepl('ever|value_bs|hhi|failure_rate',.)]
}  

# merge together all the data 
{
  bs_br = bs_br_linkedin %>% 
    
    ## add in the customs data and clean 
    merge(customs_data, all.x = T) %>%
    rename(value_customs_export = value_export, value_bs_export = for_turnover) %>% 
    mutate(across(intersect(names(.),paste0(customs_to_zero,'export')), ~ ifelse(is.na(markets_export), 0, .)),
           across(intersect(names(.),paste0(customs_to_zero,'import')), ~ ifelse(is.na(markets_import), 0, .))) %>%
    group_by(firmid) %>% 
    mutate(ever_export = as.numeric(any(markets_export !=0)),
           ever_import = as.numeric(any(markets_import != 0))) %>%
    ungroup() %>%  as.data.table() %>% .[year >= 2008] %>% 
    
    # merge in birth / industry data
    merge(birth_data %>% select(birth_year, firmid), all.x = T) %>% mutate(age = year - birth_year) %>%
    merge(industry_data, by = c('NACE_BR', 'year'), all.x = T) %>% 
    
    # generate supplementary variables 
    mutate(cost_per_worker_linkedin = comp_total / emp_total,
           cost_per_worker_bs = labor_cost / empl,
           tfp = turnover / (capital^.3*labor_cost^.7),
           across(comp_vars, ~./ comp_total, .names = "share_{col}"),
           across(c(comp_vars, bs_vars, customs_cont_vars),~asinh(.), .names = "log_{col}")) %>% 
    
    ## generate quartiles within industry 
    group_by(NACE_BR, year) %>% 
    mutate(across(paste0(c("","share_"),'comp_data'), ~ as.factor(ntile(.,4)), .names = 'quartile_{col}'))
  
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
      mutate(block = ifelse(grepl('share',group_var), '2b', block)),
    
    expand(paste0(c('log', 'share'),'_comp_data' ), industry_vars, names = c('dep_var', 'group_var')) %>%
      mutate(fe = 'year', block = ifelse(grepl('share',dep_var), '3b', '3a'))
  )) %>%
    mutate(fe = replace_na(fe, 'year + NACE_BR'))
}  

# run variations 
variation_output = list(); all_models = list()
failed_output = list();
for (i in 1:nrow(variations)){
  for (name in names(variations)){assign(name, variations[[name]][i])} 
  command = paste0('feols(data = bs_br,',dep_var, "~quartile_",group_var, " | ", fe,")")       
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
if(length(failed_output) >0){
  print('regressions failed')
  failed_output = rbindlist(failed_output)
}

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

### final output 
output_dir = '3) output/4) data_summary_stats'
dir.create(output_dir)
files_to_output = c('summary_stats_output', 'variation_output', 'failed_output', 'all_models')
for (file in files_to_output){
  write_rds(get(file), paste0(output_dir, "/",file, '.rds'))
}


