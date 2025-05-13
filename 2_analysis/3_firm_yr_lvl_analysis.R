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
base_data = import_file(file.path(inputs_dir,'16c_firm_yr_lvl.parquet')) %>% remove_if_NA('age', 'comp_data')
# run the regressions  ----------------------------------------------------
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
    mutate(dep_var =  'log_dom_turnover', ind_var = ind_vars[j], block = paste0('3',letters[i],".", j), 
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
    mutate(dep_var = 'log_total_export_rev_customs', ind_var = ind_vars[j], block = paste0('3',block_letter,".", j), 
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
           block = paste0('3',block_letter,".", j), 
           command = reg_command(
             dataset = 'base_data[year <= first_export_year]',
             dep_var = dep_var,
             ind_var = ind_var,
             time_var = 'age',
             controls = paste0(interaction, control_vars),
             fe = 'NACE_BR, year',
             cluster = 'firmid',
             family = 'cox'))}))

## combine and run 
variation_output = rbind(variations_dom_rev,variations_export_rev,variations_export_entry)
if(running_regressions){write_rds(evaluate_variations(variation_output, full_df = F), paste0(raw_output_dir,'block_3.rds'))}

# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()




