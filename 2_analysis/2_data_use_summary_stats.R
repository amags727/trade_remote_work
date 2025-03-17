# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest', 'kableExtra')
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
# setup -------------------------------------------------------------------
firm_yr_lvl = import_file(file.path(inputs_dir, '16g_firm_yr_level_summ_stats_inputs.parquet'))
firm_lvl = import_file(file.path(inputs_dir, '16h_firm_level_summ_stats_inputs.parquet'))


#export_censored_firm_level_data = firm_level_data[is.na(first_export_year) | year <= first_export_year] %>%
 # .[,has_first_year_export := ifelse(is.na(first_export_year), F, year == first_export_year)] %>% 
  #select(firmid,has_first_year_export, contains('comp_data') & !contains('industry'), age, NACE_BR, year  )



# construct the inputs for each table  ------------------------------------
growth_suffixes = c("_growth_rate", "_growth_rate_lead1", "_growth_rate_2yr")


## block 1 relationship to bs variables --> quartiles 
block_1_dep = c('log_turnover', paste0('turnover',growth_suffixes),
                paste0('log_',c('empl','age','cost_per_worker_linkedin', 'cost_per_worker_bs')))
block_1_ind =  gpaste(c("quartile", "quartile_share", "log", "share"), "_comp_data")

block_1 = expand(block_1_dep,block_1_ind,c("", " + log_age"), names = c('dep_var', 'ind_var', "controls"),
                 order = c(3,2,1)) %>% filter(!(dep_var == 'log_age' & controls == 'log_age')) %>%
  mutate(block = 1, dataset = 'firm_yr_lvl,', fe = " | NACE_BR + year",   cluster = ", cluster = ~firmid)",
         regression_type = 'feols')


## block 2 relationship to firm level variation
block_2_dep =  gpaste(order = c(4,3,2,1),c('','de_trended_'),"variance_",c('',"log_"),
                   c('turnover', 'dom_turnover', 'value_bs_export', 'value_customs_export'))
block_2_ind =  gpaste(c(gpaste(c('max_', 'median_'),c("quartile", "quartile_share"),order = c(2,1)),
                        "mean_share", 'mean_log'), "_comp_data")    
block_2 = expand(block_2_dep, block_2_ind, c("", " + log_min_sample_age + log_sample_years_observed"),
                 names = c('dep_var', 'ind_var', 'controls'), order = c(3,2,1)) %>% 
  mutate(block = 2, dataset = 'firm_lvl,', fe = '| NACE_BR', cluster = ",cluster = ~ NACE_BR)", 
         regression_type = 'feols',
         across('controls', ~ifelse(grepl('export',dep_var) &.!="", paste0(., " + log_years_exported"), .)))

## block 3 relationship to overall export performance
block_3_dep = c(
  'currently_export',
  'value_bs_export' %>% c(paste0('log_',.), paste0(.,growth_suffixes)) %>% c(., gsub('bs', 'customs',.)),
  'market_entry_failure_rate_export', 'intermarket_hhi_export')
block_3_ind = block_1_ind
block_3_controls = c('', '+ log_age') %>% c(., paste0(., ' + log_years_since_first_export')) 
block_3 = expand(block_3_dep, block_3_ind, block_3_controls, names = c('dep_var', 'ind_var', 'controls'),  order = c(3,2,1)) %>%
  mutate(block = 1, dataset = 'firm_yr_lvl,', fe = " | NACE_BR + year",   cluster = ", cluster = ~firmid)",
         regression_type = 'feols')

  
              


#model = coxph(data = temp, Surv(year-1, year, exported) ~ quartile_comp_data + age + strata(NACE_BR, year))  



# run the variations  -----------------------------------------------------
variations = block_3

variation_output = list(); all_models = list()
failed_output = list();

for (i in 1:nrow(variations)){
for (name in names(variations)){assign(name, variations[[name]][i])} 
regression_intro = case_when(regression_type == "feols"~ 'feols( data = ')
command = paste0(regression_intro, dataset, dep_var, "~", ind_var, controls, fe, cluster)  
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

# Run Base Analysis ---------------------
# define variable groups
{
  comp_vars = c("comp_total", "comp_engineer", "comp_rnd", "comp_stem")
  bs_vars = c('turnover', 'empl', 'age','tfp','cost_per_worker_linkedin', 'cost_per_worker_bs')
  industry_vars =  c('entrance_share', 'exit_share', 'churn_share', 'sd', 'coef_variation') %>%
    paste0('industry_', ., '_quartile')
  
  customs_discrete_vars = gpaste(c('currently', 'ever', 'market_entry_failure_rate','intermarket_hhi'), '_export')
  customs_cont_vars = gpaste(c('markets', 'new_markets', 'value_customs', 'value_bs'),
                             c('_export'), order = c(2,1)) %>% gsub("(?:customs|bs)_import", "import", .) %>% unique()
}  


# Define Variations
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
  if(!grepl('quartile', group_var)) group_var = paste0('quartile_', group_var)
  command = paste0('feols(data = bs_br,',dep_var, "~as.factor(",group_var, ") | ", fe,")")       
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


# run variations 
variation_output = list(); all_models = list()
failed_output = list();
for (i in 1:nrow(variations)){
  for (name in names(variations)){assign(name, variations[[name]][i])} 
  
  command = paste0('feols(data = bs_br,',dep_var, "~as.factor(quartile_",group_var, ") | ", fe,")")       
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
    text = ifelse(grepl("3", block), group_var, dep_var )
    temp =variation_output %>% filter(counter == i)
    temp = as.data.frame(temp %>% select('coef', 'p_val') %>% mutate(space = "") %>% t()) %>%
      mutate(var = c(text, "", "")) %>% select(var, everything()) %>% rename_with(~c('var', 2:4)) %>%
      mutate(block = block, counter = i)
  }
}) %>% rbindlist()



base_table = cbind(summary_stats_output %>% filter(block == '3a') %>% .[,1:4] %>%
                     rename_with(~c(" ", 2:4)),
                   summary_stats_output %>% filter(block == '3b') %>% .[,2:4]) 

headers =  '&\\multicolumn{3}{c}{Total Data Comp}& & \\multicolumn{3}{c}{Share Data Comp}\\\\'
output_path = file.path(output_dir, "variance_x_data_summary_stats.tex")
format_summary_table(base_table, divisions_before = 4, headers, 
                     note_width = 1, output_path = output_path)



# chart growth  -----------------------------------------------------------

growth_graph = bs_br[ ! is.na(quartile_share_comp_data)] %>% 
  .[, max_data_quartile :=  as.numeric(as.character(quartile_share_comp_data))] %>% 
  .[,max_data_quartile := max(max_data_quartile), by = firmid] %>% 
  .[,.(mean_turnover = NA_mean(turnover)), by = .(max_data_quartile, year)] %>% 
  .[, growth := mean_turnover / max(mean_turnover * (year== 2009)) -1, by = max_data_quartile] %>%
  ggplot(., aes(x = year, y = growth, color = as.factor(max_data_quartile))) + geom_line()


# output summary statistics from quartile regressions ----------------------------------------------
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

  command = paste0('feols(data = bs_br,',dep_var, "~as.factor(quartile_",group_var, ") | ", fe,")")       
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
    text = ifelse(grepl("3", block), group_var, dep_var )
    temp =variation_output %>% filter(counter == i)
    temp = as.data.frame(temp %>% select('coef', 'p_val') %>% mutate(space = "") %>% t()) %>%
      mutate(var = c(text, "", "")) %>% select(var, everything()) %>% rename_with(~c('var', 2:4)) %>%
      mutate(block = block, counter = i)
  }
}) %>% rbindlist()

### final output 
if (exporting_files){
dir.create(output_dir)
files_to_output = c('summary_stats_output', 'variation_output', 'failed_output')
for (file in files_to_output){
  write_rds(get(file), paste0(output_dir, "/",file, '.rds'))
}
}




summary_stats_output = import_file( paste0(output_dir, "/summary_stats_output.rds"))
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

## make linkedin output table 
{
base_table = cbind(
  summary_stats_output %>% filter(counter %in% 1:4) %>% .[,1:4] %>% rename_with(~c(" ", 2:4)),
  summary_stats_output %>% filter(counter %in% 10:13) %>% .[,2:4] %>% 
    mutate(across(everything(), ~ replace(., 1:2, "-"))))
headers =  '& \\multicolumn{3}{c}{Total Data Comp}& & \\multicolumn{3}{c}{Share Data Comp}\\\\'
notes = "Presents coefficients for the 2nd-4nd quartile indicators of data usage by firms (within industry-year). 
All regressions control for year and industry FE."
table = format_summary_table(base_table, headers = headers, divisions_before = 4, notes = notes, note_width = 1,
                             output_path = file.path(output_dir, "linkedin_x_data_summary_stats.tex")) 

}

## make BS output table 
{
  import_regs = summary_stats_output %>% filter(grepl('import', var)) %>% pull(counter) %>% unique()
  base_table = cbind(summary_stats_output %>% filter(block == '2a' & !counter %in% import_regs) %>% .[,1:4] %>%
                       rename_with(~c(" ", 2:4)),
                     summary_stats_output %>% filter(block == '2b' & !counter %in% import_regs) %>% .[,2:4]) 
  
  headers =  '&\\multicolumn{3}{c}{Total Data Comp}& & \\multicolumn{3}{c}{Share Data Comp}\\\\'
  output_path = file.path(output_dir, "customs_x_data_summary_stats.tex")
  notes = "Presents coefficients for the 2nd-4nd quartile indicators of data usage by firms (within industry-year). 
All regressions control for year and industry FE."
  format_summary_table(base_table, divisions_before = 4, headers, notes, 
                       note_width = 1, output_path)
}






