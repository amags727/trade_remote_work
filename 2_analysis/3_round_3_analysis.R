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

## NB Block 1 is the balance tests; Block 2 is the comparison of different explanatory variables
# 3 firm yr lvl analysis -------------------------------------------------------------------------
block_num =3 
input_file = paste0(inputs_dir,'16c_firm_yr_lvl.parquet')

## total domestic revenue 
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean')
interactions = list(c('log_empl', 'log_capital'),
                    gpaste('nace_', c('entrance_rate', 'exit_rate', 'churn_rate')),
                    gpaste('nace_de_trended_log_variance_', c('ind', 'group'), "_lvl"))
interactions = lapply(interactions, function(x) append('', gpaste('*',x)))
control_vars = paste(" ", 'log_comp_rnd', 'log_age', 'comp_weighted_prestige', sep = "+")

variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j'))%>% .[,counter := 1:nrow(.)]
variations_dom_rev = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(dep_var =  'log_dom_turnover', ind_var = ind_vars[j], block = paste0(block_num,letters[i],".", j), 
           command = reg_command(
             dataset = 'base_data',
             dep_var = dep_var,
             ind_var = ind_var,
             controls = paste0(interaction, control_vars),
             fe = "|NACE_BR + year",cluster = 'firmid'))}))

## total export revenue 
interactions[[1]] = append('', gpaste('*',c('asinh(empl)', 'asinh(capital)', 'log_export_mkt_avg_rev_wgted_comp_now', 'log_dom_turnover')))
control_vars = paste0('+ log_dom_turnover', control_vars)
variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j')) %>% .[,`:=`(counter = 1:nrow(.), block_letter = letters[i +max(variations$i)])] 
variations_export_rev = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(dep_var = 'log_total_export_rev_customs', ind_var = ind_vars[j], block = paste0(block_num,block_letter,".", j), 
           command = reg_command(
             dataset = 'base_data',
             dep_var = dep_var,
             ind_var = ind_var,
             controls = paste0(interaction, control_vars),
             fe = "|NACE_BR + year",
             cluster = 'firmid'))}))

## likelihood of entry into exporting
interactions[[1]] = append('', gpaste('*',c('asinh(empl)', 'asinh(capital)', 'log_comp_abroad', 'log_dom_turnover')))
control_vars = gsub("\\+log_age", '', control_vars)
variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j')) %>% .[,`:=`(counter = 1:nrow(.), block_letter = letters[i +max(variations$i)])] 
variations_export_entry = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(dep_var = 'is_first_export_year',
           ind_var = ind_vars[j],
           block = paste0(block_num,block_letter,".", j), 
           command = reg_command(
             dataset = 'base_data[year <= first_export_year]',
             dep_var = dep_var,
             ind_var = ind_var,
             time_var = 'age',
             controls = paste0(interaction, control_vars),
             fe = 'NACE_BR, year',
             cluster = 'firmid',
             family = 'cox'))}))

## run regressions
if(running_regressions){
  variation_output = rbind(variations_dom_rev,variations_export_rev,variations_export_entry)
  nec_vars = c(extract_model_vars(variation_output$command))
  base_data = import_file(input_file, col_select = nec_vars) %>% remove_if_NA('age', 'log_comp_data')
  output = evaluate_variations(variation_output, full_df = F)
  write_rds(output, paste0(raw_output_dir,paste0('block_',block_num,'.rds')))
  }
## cleanup 
rm(list= setdiff(ls(), base_env)); gc()
# 4 firm-ctry-year level -------------------------------------------
block_num = 4
input_file = paste0(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')
## set up mkt revenue regressions 
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean')
controls =  "+ log_age + log_dom_turnover + log_comp_rnd + comp_weighted_prestige"
interactions = {list(c('log_other_market_rev', 'log_dom_turnover','log_num_markets','log_comp_now','first_time_in_ctry',  'first_time_exporting'),
                     c(gpaste('mkt_', c('entrance', 'exit', 'failure', 'churn'), "_rate"),
                       gpaste('mkt_de_trended_log_variance_', c('ind', 'group'), "_lvl")),
                     c('log_distance_to_france', 'log_mkt_size_rev', 'mkt_share_active_exporters',
                       gpaste('grav_',c('region', 'language', 'border')))
)}
interactions = lapply(interactions, function(x) append('', gpaste('*',x)))

variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j'))%>% .[,counter := 1:nrow(.)]
variations_mkt_rev = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(dep_var = 'log_export_rev_customs',
           ind_var = ind_vars[j],
           block = paste0(block_num,letters[i],".", j), 
           command = reg_command(
             dataset = 'base_data',
             dep_var = dep_var,
             ind_var = ind_var,
             controls = paste0(interaction, controls),
             fe = "|NACE_BR + year + ctry",cluster = 'firmid'))}))

## Setup mkt time to exit regressions 
controls = gsub("\\+ log_age", '', controls)
interactions[[1]] = append(interactions[[1]], "*log_export_rev_customs", after = 1)
variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j')) %>% .[,`:=`(counter = 1:nrow(.), block_letter = letters[i +max(variations$i)])] 
variations_mkt_exit = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(dep_var = 'last_year_of_streak',
           ind_var = ind_vars[j],
           block = paste0(block_num,letters[i],".", j), 
           command = reg_command(
             dataset = 'base_data',
             dep_var = dep_var,
             ind_var = ind_var, 
             controls = paste0(interaction, controls),
             fe =  'NACE_BR, ctry, year',
             family = 'cox', 
             cluster = 'firmid',
             time_var = 'streak_age'))}))

#combine 
variation_output = rbind(variations_mkt_rev,variations_mkt_exit)
## run regressions
if(running_regressions){
  nec_vars = c(extract_model_vars(variation_output$command))
  base_data = import_file(input_file, col_select = nec_vars) %>% remove_if_NA('log_age', 'log_comp_data')
  output = evaluate_variations(variation_output, full_df = F)
  write_rds(output, paste0(raw_output_dir,paste0('block_',block_num,'.rds')))
}
## cleanup 
rm(list= setdiff(ls(), base_env)); gc()

# 5 total rev variance ---------------------------------------------------
input_file = file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet')
block_num= 5
## run dom variance regressions 
dep_var =  'detrended_var_log_dom_turnover'
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean')
control_vars = '+log_dom_turnover + log_comp_rnd + comp_weighted_prestige +log_min_age_dom_rev_observed'
fe = "| NACE_BR + years_dom_rev_observed + min_first_year_dom_rev_observed"
interactions = {list(gpaste('log_', c('empl', 'capital', 'dom_turnover')),
                     gpaste('nace_', c('entrance', 'exit', 'churn'), '_rate'),
                     gpaste('nace_de_trended_log_variance_', c('ind', 'group'), "_lvl"))}
interactions = lapply(interactions, function(x) append('', gpaste('*',x)))

variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j'))%>% .[,counter := 1:nrow(.)]
variations_dom_rev = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>% mutate(block_letter = letters[i]) %>% 
    mutate(block = paste0(block_num,block_letter,".", j), 
           command = reg_command(
             dataset = 'base_data',
             dep_var = dep_var,
             ind_var = ind_vars[j],
             controls = paste0(interaction, control_vars),
             fe = fe,cluster = 'NACE_BR'))}))

## run export variance regressions 
dep_var = gsub('dom_turnover', 'total_export_rev_customs', dep_var)
control_vars = gsub('dom_rev', 'exports', paste0("+log_total_export_rev_customs", control_vars))
fe = gsub('dom_rev', 'exports',fe)
interactions[[1]] = c('',gpaste('*log_', c('empl', 'capital', 'dom_turnover', 'total_export_rev_customs', 'export_mkt_avg_rev_wgted_comp_now')))

variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j')) %>% .[,`:=`(counter = 1:nrow(.), block_letter = letters[i +max(variations$i)])] 
variations_export_var = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(block = paste0(block_num,block_letter,".", j), 
           command = reg_command(
             dataset = 'base_data',
             dep_var = dep_var,
             ind_var = ind_vars[j],
             controls = paste0(interaction, control_vars),
             fe = fe,cluster = 'NACE_BR'))}))

## combine 
variation_output = rbindlist(list(variations_dom_rev, variations_export_var), fill = T, use.names = T)
## run regressions
if(running_regressions){
  nec_vars = c(extract_model_vars(variation_output$command))
  base_data = import_file(input_file, col_select = nec_vars) %>% remove_if_NA('log_comp_data') %>% filter(!(is.na(log_min_age_exports_observed & log_min_age_dom_rev_observed)))
  output = evaluate_variations(variation_output, full_df = F)
  write_rds(output, paste0(raw_output_dir,paste0('block_',block_num,'.rds')))
}
## cleanup 
rm(list= setdiff(ls(), base_env)); gc()
# 6 mkt export rev variance ------------------------------------------------
input_file = file.path(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet')
block_num = 6

## Setup mkt revenue regressions 
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean')
controls = "+ log_export_rev_customs + log_dom_turnover + log_min_age + log_comp_rnd + comp_weighted_prestige + log_years_observed"
interactions = {list(c('log_export_rev_customs','log_other_market_rev', 'log_dom_turnover',
                      'log_num_markets','log_comp_now','first_time_in_ctry',  'first_time_exporting'),
                    c(gpaste('mkt_', c('entrance', 'exit', 'failure', 'churn'), "_rate"),
                      gpaste('mkt_de_trended_log_variance_', c('ind', 'group'), "_lvl")),
                    c('log_distance_to_france', 'log_mkt_size_rev', 'mkt_share_active_exporters',
                      gpaste('grav_',c('region', 'language', 'border'))))}; interactions = lapply(interactions, function(x) append('', gpaste('*',x)))

variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j'))%>% .[,counter := 1:nrow(.)]
variation_output = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(block = paste0(block_num,letters[i],".", j), 
           command = reg_command(
             dataset = 'base_data',
             dep_var = 'detrended_var_log_export_rev_customs',
             ind_var = ind_vars[j],
             controls = paste0(interaction, controls),
             fe ='| NACE_BR + min_year +ctry',
             cluster = 'firmid'))}))

## run regressions
if(running_regressions){
  nec_vars = c(extract_model_vars(variation_output$command))
  base_data = import_file(input_file, col_select = nec_vars) %>% remove_if_NA('log_comp_data', 'log_min_age')
  output = evaluate_variations(variation_output, full_df = F)
  write_rds(output, paste0(raw_output_dir,paste0('block_',block_num,'.rds')))
}
## cleanup 
rm(list= setdiff(ls(), base_env)); gc()

# 7 ctry entry analysis ----------------------------------------------------
block_num = 7; min_pop_rank = 20
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean')
interactions = {list(
  # domestic scale effects 
  c('log_dom_turnover','log_empl', 'log_capital'),
  
  # international exp / scale effects 
  c('not_yet_exported','log_other_export_market_rev','log_num_other_export_markets','log_comp_now'),
  
  # destination mkt uncertainty 
  c(gpaste('mkt_', c('entrance', 'exit', 'failure', 'churn'), "_rate"), gpaste('mkt_de_trended_log_variance_', c('ind', 'group'), "_lvl")),
  
  # destination mkt popularity / gravity 
  c('log_distance_to_france', 'log_mkt_size_rev', 'mkt_share_active_exporters',gpaste('grav_',c('region', 'language', 'border'))))}
interactions = lapply(interactions, function(x) append('', gpaste('*',x)))
control_vars = paste(" ",'log_dom_turnover','log_comp_rnd','comp_weighted_prestige', sep = "+")

variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j'))%>% .[,counter := 1:nrow(.)]
variation_output = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
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


## run regressions
if(running_regressions){
  nec_vars = c(extract_model_vars(variation_output$command), 'mkt_all_time_popularity_rank')
  base = rbindlist(lapply(list.files('1) data/temp_data',recursive = TRUE, full.names = TRUE), function(file){
    import_file(file, col_select = nec_vars)[mkt_all_time_popularity_rank <= min_pop_rank]}))
  output = evaluate_variations(variation_output, full_df = F)
  write_rds(output, paste0(raw_output_dir,paste0('block_',block_num,'.rds')))
}
## cleanup 
rm(list= setdiff(ls(), base_env)); gc()


