# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work",
    
    ## update as makes sense for CASD / your own use 
    T ~ "G:/My Drive/IWH/PhD/GitHub/MDI/Big Data Project_local")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')

## set the ind vars that we're going to use for all the blocks 
ind_base = 'log_comp_data'; ind_base_event_study = gpaste(ind_base,'_y', year_range, collapse_str = "+")
base_env = c(base_env, 'ind_base', 'ind_base_event_study')

# 1 firm-ctry-year level -------------------------------------------
block_num = 1
input_file = paste0(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')


## set up mkt revenue regressions 
control_vars =  "+ log_age + log_dom_turnover + log_comp_rnd + comp_weighted_prestige + nace_mkt_share_active_exporters + mkt_share_active_exporters"
interactions = {list(gpaste(c("extended_", "", "either_"), 'grav_',c('region', 'language', 'border')))}
interactions = lapply(interactions, function(x) append('', gpaste('*',x)))
variations_rev= {rbindlist(lapply(1:length(interactions), function(i){
  data.frame(interaction = interactions[[i]], block_letter = i, ind_var = ind_base)})) %>% 
    add_row(.,   interaction = "", block_letter = length(interactions) +1, ind_var = ind_base_event_study) %>% 
    mutate(dep_var = 'log_export_rev_customs',control_vars = control_vars,  fe = "|NACE_BR + year + ctry", family = 'feols')}

## set up market exit regressions 
control_vars = gsub("\\+ log_age", '', control_vars)
interactions[[1]] = append(interactions[[1]], "*log_export_rev_customs", after = 1)
variations_mkt_exit = {rbindlist(lapply(1:length(interactions), function(i){
  data.frame(interaction = interactions[[i]], block_letter = i, ind_var = ind_base)})) %>% 
    add_row(.,   interaction = "", block_letter = length(interactions) +1, ind_var = ind_base_event_study) %>% 
    mutate(dep_var = 'last_year_of_streak', control_vars = control_vars, fe =  'NACE_BR, ctry, year', family = 'cox', time_var = 'streak_age')}

## run regressions
variation_output = {rbindlist(list(variations_rev, variations_mkt_exit), fill =T, use.names = T) %>% as.data.table() %>% 
    .[,`:=`(block = paste0(block_num, letters[rleid(block_letter)]), block_letter =NULL)] %>%rowwise() %>% mutate(command = reg_command(
      dataset = 'base_data',
      dep_var= dep_var, 
      ind_var = ind_var, 
      controls= paste0(interaction, control_vars), 
      fe = fe, 
      cluster = 'firmid',
      family = family,
      time_var = time_var
    ))}
nec_vars = c(extract_model_vars(variation_output$command))
base_data = import_file(input_file, col_select = nec_vars) %>% remove_if_NA('log_age', 'log_comp_data')
output = evaluate_variations(variation_output, full_df = F); pause_for_check(enforce_pauses)
write_rds(output, paste0(raw_output_dir,paste0('block_',block_num,'.rds')))

## cleanup 
rm(list= setdiff(ls(), base_env)); gc()
