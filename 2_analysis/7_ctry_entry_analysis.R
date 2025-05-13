# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')

## import base data 
base = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')) %>% remove_if_NA('comp_data','age')


# run regressions -----------------------------------------------------------------------

## Import Data and Set Parameters 
min_pop_rank = 20
base = rbindlist(lapply(list.files('1) data/temp_data',recursive = TRUE, full.names = TRUE),
                        function(file) import_file(file)[mkt_all_time_popularity_rank <= min_pop_rank]))
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean')
interactions = list(
  # domestic scale effects 
  c('log_dom_turnover','log_empl', 'log_capital'),
  
  # international exp / scale effects 
  c('not_yet_exported','log_other_export_market_rev','log_num_other_export_markets','log_comp_now'),
  
  # destination mkt uncertainty 
  c(gpaste('mkt_', c('entrance', 'exit', 'failure', 'churn'), "_rate"), gpaste('mkt_de_trended_log_variance_', c('ind', 'group'), "_lvl")),
  
  # destination mkt popularity / gravity 
  c('log_distance_to_france', 'log_mkt_size_rev', 'mkt_share_active_exporters',gpaste('grav_',c('region', 'language', 'border'))))
interactions = lapply(interactions, function(x) append('', gpaste('*',x)))
control_vars = paste(" ", 'log_comp_rnd', 'log_age', 'comp_weighted_prestige', sep = "+")


variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j'))%>% .[,counter := 1:nrow(.)]
variations = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(dep_var =  'log_dom_turnover', ind_var = ind_vars[j], block = paste0('3',letters[i],".", j), 
           command = reg_command(
             dataset = 'base',
             dep_var = "entered_market",
             ind_var = ind_var, 
             controls = paste0(interaction, control_vars), 
             fe = 'NACE_BR, ctry, year',
             cluster = 'firmid',
             family = 'cox',
             time_var = 'age'))}))



# import data and run regressions ----------------------------------------------------------------------
# import the data 
#base_data = import_file(file.path(inputs_dir, '16e_ctry_entrance.parquet'))
base_data = rbindlist(lapply(list.files('1) data/temp_data',recursive = TRUE, full.names = TRUE),import_file))

#set parameters 
interactions_1 = c('log_num_other_export_markets', 'not_yet_exported')
interactions_2 = gpaste('log_comp_', c('now', 'l5', 'ever'))
interactions_3 = gpaste(c('mkt_', 'nace_mkt_'), c('churn_rate', 'failure_rate', 'log_variance_ind_lvl', 'log_variance_group_lvl'))
interactions_4 = c('log_distance_to_france', 'log_mkt_size_rev', 'mkt_share_active_exporters') %>% c(., gsub('mkt', 'nace_mkt',.))
controls =  " + log_dom_turnover + log_comp_rnd + comp_weighted_prestige"
young_filter = gpaste('base_data',c('', '[young_at_start == T]', '[young_at_start == F]'))

for (i in 1:4){
file_path = paste0("3) output/0_raw_output/4",letters[i],"_output_raw.rds")
interactions =  c("", gpaste("*",get(paste0('interactions_',i))))
block = expand(young_filter,interactions, names = c('filter', 'interaction')) %>% rowwise() %>% mutate(
  command = reg_command(filter, "entered_market", 'log_comp_data', paste0(interaction, controls), 
                        'NACE_BR, ctry, year', cluster = 'firmid', family = 'cox', time_var = 'age'))

if (running_regressions) write_rds(evaluate_variations(block),file_path)
assign(paste0('block_4',letters[i]),block)
assign(paste0('block_4',letters[i],'_output'),read_rds(file_path))
rm(block, interactions, file_path)
}

# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()

