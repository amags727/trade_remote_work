# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest', 'kableExtra', 'survival')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')}
source("2) code/00_helper_functions.R")
output_dir = '3) output/4) data_summary_stats'
exporting_files = F
inputs_dir = ('1) data/16_inputs_for_data_summary_stats')
base_env = c(ls(),'base_env')
# import data  -------------------------------------------------------------------
firm_yr_lvl = import_file(file.path(inputs_dir, '16g_firm_yr_level_summ_stats_inputs.parquet'))
firm_lvl = import_file(file.path(inputs_dir, '16h_firm_level_summ_stats_inputs.parquet'))
firm_ctry_yr_lvl = import_file(file.path(inputs_dir, '16i_export_firm_ctry_level_summ_stats_inputs.parquet'))

# set up variations and run ------------------------------------


growth_suffixes = c("_growth_rate") #, "_growth_rate_lead1", "_growth_rate_2yr")
feols_command = function(dataset, dep_var,ind_var, controls, fe, cluster){
  command = paste0('feols( data = ',dataset, ", ", dep_var, "~", ind_var, controls, fe, ",cluster = ~", cluster, ")")
}
flogit_command = function(dataset, dep_var,ind_var, controls, fe, cluster){
  command = paste0('feols( data = ',dataset, ", ", dep_var, "~", ind_var, controls, fe, ",cluster = ~", cluster,
                   ", family = binomial(link = 'logit'), glm.iter = 1000)")
}
fcox_command = function(dataset, dep_var, ind_var, controls, fe, cluster, time_var){
  paste0("coxph(formula = Surv(", time_var, ",", time_var, "+ 1,", dep_var, ") ~ ", ind_var, controls, '+ strata(',fe ,") ,data = ", dataset,
         ", cluster = ", cluster, ", control = coxph.control(iter.max = 1000))")
}
## block 1 relationship to bs variables
{
block_1_dep = c('log_turnover', paste0('turnover',growth_suffixes),
                paste0('log_',c('empl','age',paste0('cost_per_worker', c('_bs','_linkedin',"_linkedin_fr")))))
  
block_1_ind =  gpaste(c("quartile", "quartile_share", "log", "share"), "_comp_data")

block_1 = expand(block_1_dep,block_1_ind,c("", " + log_age"), names = c('dep_var', 'ind_var', "controls"),
                 order = c(3,2,1)) %>% filter(!(dep_var == 'log_age' & controls == 'log_age')) %>%
  mutate(block = 1, dataset = 'firm_yr_lvl', fe = " | NACE_BR + year",   cluster = "firmid",
         command = feols_command(dataset, dep_var,ind_var, controls, fe, cluster))
}
## block 2 relationship to firm level variation
{
block_2_dep =  gpaste(order = c(4,3,2,1),c('','de_trended_'),"variance_",c('',"log_"),
                   c('turnover', 'dom_turnover', 'value_bs_export', 'value_customs_export'))
block_2_ind =  gpaste(c(gpaste(c('max_', 'median_'),c("quartile", "quartile_share"),order = c(2,1)),
                        "mean_share", 'mean_log'), "_comp_data")    
block_2 = expand(block_2_dep, block_2_ind, c("", " + log_min_sample_age + log_sample_years_observed"),
                 names = c('dep_var', 'ind_var', 'controls'), order = c(3,2,1)) %>% 
  mutate(block = 2, dataset = 'firm_lvl', fe = '| NACE_BR', cluster = "NACE_BR", 
         across('controls', ~ifelse(grepl('export',dep_var) &.!="", paste0(., " + log_years_exported"), .)),
         command = feols_command(dataset, dep_var,ind_var, controls, fe, cluster))
}
## block 3 relationship to overall export performance
{
block_3_dep = c(
  'currently_export',
  'value_bs_export' %>% c(paste0('log_',.), paste0(.,growth_suffixes)) %>% c(., gsub('bs', 'customs',.)),
  'market_entry_failure_rate_export', 'intermarket_hhi_export')
block_3_ind = block_1_ind
block_3_controls =  c("+ log_age", '+ log_age + log_dom_turnover' %>% c(., paste0(., ' + log_years_since_first_export')))
block_3 = expand(block_3_dep, block_3_ind, block_3_controls, names = c('dep_var', 'ind_var', 'controls'),  order = c(3,2,1)) %>%
  mutate(block = 1, dataset = 'firm_yr_lvl', fe = " | NACE_BR + year",   cluster = "firmid",
         command = feols_command(dataset, dep_var,ind_var, controls, fe, cluster))
         
}
## block 4 individual market performance
{
block_4_dep  = c('deflated_value', 'products')
block_4_dep = c(gpaste('log_',block_4_dep),gpaste(block_4_dep,growth_suffixes)) %>% .[order(!grepl("defl", .))] 
interactions =  gpaste(block_1_ind, c('first_export_streak','first_ctry_streak', 'log_markets_export') %>% paste0("*",.," + ",.))
block_4_ind = c(block_1_ind, interactions)
block_4_controls = "+ log_age +log_dom_turnover" %>% c(., paste0(.,"+log_years_since_ctry_entry + log_years_since_export_entry")) 
block_4 = expand(block_4_dep, block_4_ind, block_4_controls, names = c('dep_var', 'ind_var', 'controls')) %>%
  mutate(block =4, dataset = 'firm_ctry_yr_lvl', cluster = 'firmid', fe = "| NACE_BR  + ctry + year",
         command = feols_command(dataset, dep_var,ind_var, controls, fe, cluster))
}
## block 5: entrance values in individual markets 
{
block_5_dep = gpaste('log_first_',c('streak', 'ctry', 'export'),"_yr_", c('deflated_value', 'products'), order = rev(1:4))
block_5_ind =  c(block_1_ind, gpaste(block_1_ind, c('log_markets_export') %>% paste0("*",.," + ",.)))
block_5 = expand(block_5_dep, block_5_ind, names = c('dep_var', 'ind_var')) %>%
  
  ## handle the controls 
  mutate(controls = "+ log_age +log_dom_turnover", counter = 1:nrow(.)) %>% 
  rbind(., .[!grepl('export', dep_var)] %>% 
          mutate(controls = paste0(controls, "+ log_years_since_export_entry + log_markets_export" ),
                 across('controls', ~ifelse(grepl('streak', dep_var), paste0(.,"+log_years_since_ctry_entry"),.)))) %>%
  arrange(counter) %>% select(-counter) %>%
  
  ## handle rest of variables 
  mutate(block = 5, dataset = 'firm_ctry_yr_lvl', fe = " | NACE_BR + year + ctry", 
         cluster = "ctry_NACE_BR",
         command = feols_command(dataset, dep_var,ind_var, controls, fe, cluster))
         
}
## block 6: relationship to other comp variables 
{
block_6 = expand(paste0('comp_',c("total", "engineer", "rnd", "stem")), block_1_ind, names = c('dep_var', 'ind_var'), order = 2:1) %>%
  mutate(across('dep_var', ~ifelse(grepl('share', ind_var), paste0('share_',.), paste0('log_',.))),
         controls = "+ log_age", block = 6, dataset = 'firm_yr_lvl',
         fe = "| NACE_BR + year", cluster = 'firmid', 
         command = feols_command(dataset, dep_var,ind_var, controls, fe, cluster)) %>% 
    filter(dep_var != 'share_comp_total')
}
## block 7: relationship to industry level variables 
{
block_7_dep = c("log_comp_data","share_comp_data")
block_7_ind = gpaste("industry_",c(gpaste(c('entrance', 'exit', 'churn'), "_share"),
              gpaste(c("", 'de_trended_'),'variance_' ,c("","log_"), 'turnover', order = c(3,4,1,2))),'_quartile') 

block_7 = expand(block_7_dep, block_7_ind, names = c('dep_var', 'ind_var')) %>% 
  mutate(controls = "+ log_age", block = 7, dataset = 'firm_yr_lvl', fe = "| year", cluster = 'firmid',
         command = feols_command(dataset, dep_var,ind_var, controls, fe, cluster)) 
}
## Block 8: Logit time to enter
{
block_8 = expand(block_1_ind, c('+log_age', '+log_age + log_dom_turnover') %>% c(., gsub("\\+log", "*log",.)),
                 names = c('ind_var', 'controls')) %>%
  mutate(dep_var = 'is_first_export_year', block = 9, dataset = "firm_yr_lvl[year <= first_export_year] ",
        fe = "| NACE_BR + year", cluster ='firmid',
         command = flogit_command(dataset, dep_var,ind_var, controls, fe, cluster)) 
}

## Block 9: Logit time to exit market 
{
block_9_controls = c("+log_streak_age + log_dom_turnover + log_num_other_markets_export",
                      "*log_streak_age + log_dom_turnover +  log_num_other_markets_export",
                      "*log_num_other_markets_export + log_streak_age + log_dom_turnover",
                      "*first_export_streak + log_streak_age + log_dom_turnover + log_num_other_markets_export",
                      "*first_ctry_streak +  log_streak_age + log_dom_turnover + log_num_other_markets_export")
block_9 = expand('streak_ends', block_1_ind, block_9_controls, names =  c('dep_var', 'ind_var', 'controls')) %>%
  mutate(block= 9, dataset = 'firm_ctry_yr_lvl',fe =  "| NACE_BR + year + ctry",  cluster ='firmid',
         command = flogit_command(dataset, dep_var,ind_var, controls, fe, cluster)) 
}

## Block 10: Cox survival analysis 
{
block_10_controls = c(""," + log_dom_turnover", "+ log_num_other_markets_export + log_dom_turnover",
                      paste0('*first_',c('export', 'ctry'), '_streak + log_num_other_markets_export + log_dom_turnover'))
                    
block_10 = expand(dep_var = c('is_first_export_year', 'streak_ends'), block_1_ind, block_10_controls, names = c('dep_var', 'ind_var', 'controls')) %>%
  ## filter out variations we're not interested in  
  filter((controls %in% c("", " + log_dom_turnover") & dep_var == 'is_first_export_year') |
         (!controls %in% c(""," + log_dom_turnover") & dep_var != 'is_first_export_year')) %>%
  
  ## assign the remaining variables 
  mutate(block = 10,first_export = dep_var == 'is_first_export_year',
         time_var = ifelse( first_export, "age", 'streak_age'),
         fe = ifelse(first_export, "NACE_BR, year",  "NACE_BR, year, ctry"),
         dataset =  ifelse(first_export, "firm_yr_lvl[year <= first_export_year]", 'firm_ctry_yr_lvl'),
         cluster = 'firmid',
         command = fcox_command(dataset, dep_var, ind_var, controls, fe, cluster, time_var),
         first_export = NULL)
}

# combine together all the variations 
variations = list(); for(block in paste0('block_',1:10)) variations = append(variations, list(get(block))); 
variations = rbindlist(variations, use.names = T, fill = T)

## run 
full_output = evaluate_variations(variations)
View(full_output$failed_output) 

## export outputs 
unlink("3) output/4) data_summary_stats", recursive = TRUE)
dir.create("3) output/4) data_summary_stats")
write_rds(full_output, "3) output/4) data_summary_stats/all_output.rds")
import_file('1) data/16_inputs_for_data_summary_stats/16f_data_x_industry_stats_censored.parquet') %>%
  write_parquet("3) output/4) data_summary_stats/data_x_industry_stats_censored.parquet")
rm(list= setdiff(ls(), c(base_env, c('full_output','variations'))))


# output results nicely ---------------------------------------------------
save_for_later = T

if (!save_for_later){
### clean variation output 
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
    text = ifelse(grepl("3", block), group_var, dep_var )
    temp =variation_output %>% filter(counter == i)
    temp = as.data.frame(temp %>% select('coef', 'p_val') %>% mutate(space = "") %>% t()) %>%
      mutate(var = c(text, "", "")) %>% select(var, everything()) %>% rename_with(~c('var', 2:4)) %>%
      mutate(block = block, counter = i)
  }
}) %>% rbindlist()

## make BS output table 
{
  base_table = cbind(summary_stats_output %>% filter(counter %in% 5:10) %>% .[,1:4] %>%
                       rename_with(~c(" ", 2:4)),
                     summary_stats_output %>% filter(counter %in% 14:19) %>% .[,2:4]) 
  
  headers =  '&\\multicolumn{3}{c}{Total Data Comp}& & \\multicolumn{3}{c}{Share Data Comp}\\\\'
  output_path = file.path(output_dir, "bs_x_data_summary_stats.tex")
  notes = "Presents coefficients for the 2nd-4nd quartile indicators of data usage by firms (within industry-year). 
All regressions control for year and industry FE."
  format_summary_table(base_table, divisions_before = 4, headers, notes, 
                       note_width = 1, output_path)
}
## make growth graphs 
growth_graph = bs_br[ ! is.na(quartile_share_comp_data)] %>% 
  .[, max_data_quartile :=  as.numeric(as.character(quartile_share_comp_data))] %>% 
  .[,max_data_quartile := max(max_data_quartile), by = firmid] %>% 
  .[,.(mean_turnover = NA_mean(turnover)), by = .(max_data_quartile, year)] %>% 
  .[, growth := mean_turnover / max(mean_turnover * (year== 2009)) -1, by = max_data_quartile] %>%
  ggplot(., aes(x = year, y = growth, color = as.factor(max_data_quartile))) + geom_line()

}
