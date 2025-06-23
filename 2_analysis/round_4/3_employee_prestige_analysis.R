# setup -------------------------------------------------------------------
rm(list = ls()); gc();

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/Big Data Code",
    
    ## update as makes sense for CASD / your own use 
    T ~ "C:/Users/Public/Documents/Big data Project")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')


# firm yr analysis -----------------------------------------------------------------------
## Total Export Revenue
data = import_file(firm_yr_path)
controls_base = '+  weighted_college + log_comp_abroad+ log_dom_turnover + log_dom_turnover_sq'
controls_expanded = paste0(controls_base, "+", gpaste('weight_PC',as.character(1:10), collapse_str = "+"))

variations = data.table(command = rep(reg_command(
  dataset = 'data', 
  dep_var = 'log_total_export_rev_customs',
  ind_var = 'comp_weighted_prestige',
  controls_expanded, 
  fe = "|age_bracket +  NACE_BR + year",
  cluster = 'firmid'
),3)) %>%
  .[2, command := gsub('= data', '= data[currently_export == T]',command)] %>% # restrict to exporters with no additional controls
  .[3, command := gsub('turnover_sq', 'turnover_sq + ctry_pop + log_num_mkts', command)] # restrict to exporters and add controls for network composition and size 
variations = rbind(variations, variations %>% mutate(command = gsub('NACE_BR', 'prodfra_plus_numeric', command)))
evaluate_variations(variations)[['model_output']]


## likelihood of starting to export 
variations = data.frame(controls = c(controls_base, controls_expanded)) %>% mutate(command = reg_command(
  dataset = 'data[year <= first_export_year]', 
  dep_var = "is_first_export_year",
  ind_var = 'comp_weighted_prestige', 
  controls = controls, 
  fe = 'NACE_BR, year',
  cluster = 'firmid',
  family = 'cox',
  time_var = 'age'))
variations = rbind(variations, variations %>% mutate(command = gsub('NACE_BR', 'prodfra_plus_numeric', command)))
evaluate_variations(variations)[['model_output']]

# cleanup
rm(list= setdiff(ls(), base_env)); gc()
# firm ctry year analysis  ------------------------------------------------
data = import_file(firm_ctry_yr_path)
## base analysis 
controls_base = paste('+  weighted_college +  log_dom_turnover + log_dom_turnover_sq +log_num_mkts + ctry_pop +',gpaste('weight_PC',as.character(1:10), collapse_str = "+"))
variations = data.frame(controls = controls_base) %>% mutate(command = reg_command(
  dataset = 'data', 
  dep_var = 'asinh(export_rev_customs)',
  ind_var = 'comp_weighted_prestige',
  controls, 
  fe = "|age_bracket +  NACE_BR + year + ctry",
  cluster = 'firmid'
))
evaluate_variations(variations)[['model_output']]

## survival rate in foreign markets 
variations = data.frame(controls = controls_base) %>% mutate(command = reg_command(
  dataset = 'data', 
  dep_var = "(year == streak_end)",
  ind_var = 'comp_weighted_prestige', 
  controls = controls, 
  fe = 'NACE_BR, year, ctry',
  cluster = 'firmid',
  family = 'cox',
  time_var = '(year - streak_start)'))
evaluate_variations(variations)

# firm variance analysis  ------------------------------------------------
data = import_file(firm_variance_path)%>% .[,always_exporter := (years_exporting == years_observed)]

## impact on variance  
controls = c('','weighted_college', 'log_dom_turnover', 'log_dom_turnover_sq',
           gpaste('weight_PC',as.character(1:10))) %>%
           paste(., collapse = "+")
dep_vars = 'log_total_export_rev_customs_detrended_var'
variations = data.frame(dep_var = dep_vars) %>% mutate(command = reg_command(
  dataset = 'data', 
  dep_var,
  ind_var = 'comp_weighted_prestige',
  controls, 
  fe = "|age_bracket +NACE_BR + year +years_observed",
  cluster = 'NACE_BR'
))
evaluate_variations(variations)[['model_output']]
















