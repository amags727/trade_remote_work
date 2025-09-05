# setup -------------------------------------------------------------------
rm(list = ls()); gc();

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data",
    
    ## update as makes sense for CASD / your own use 
    T ~ "C:/Users/Public/Documents/Big data Project")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')
# set parameter values  ---------------------------------------------------
base_controls =  paste("", 'log_comp_total', 'log_comp_total_lag1', 'log_dom_turnover', 'log_dom_turnover_sq',
                       'avg_prestige_total', 'share_empl_college', "capital_intensity", 'ctry_pop_among_exporters', 'log_age', sep = " + ")

base_command = reg_command(dataset = 'firm_yr_ctry', dep_var =  'log_export_rev_customs', ind_var = 'log_comp_data + log_comp_data_lag1', 
                           controls = base_controls, fe = "| firmid_num + year + ctry_num", cluster = 'firmid_num')
restrictions = c('', '[currently_export_customs_any_ctry == T]', '[currently_export_customs== T]', '[ever_export_customs==T]')

base_coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
                      gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige',
                    'share empl. college grad', "log capital intensity", 'country\npopularity', 'log firm age')


interaction_vars_and_terms<-fread(
"var, term, time_varying
nace_share_export_customs_any_ctry, share industry exporting, T
grav_region, gravity region, F
grav_border, gravity border, F
grav_language, gravity language, F
log_grav_dist, log gravity distance, F
either_grav_region, extended grav. region, T
either_grav_border, extended grav. border, T
either_grav_language, either gravity language, T
either_grav_dist, extended grav. distance , T
log_either_grav_dist,log extended grav. distance, T
ctry_pop_among_exporters, country popularity, T
ctry_log_variance_group_lvl, country-level total export variance, F
ctry_log_variance_ind_lvl, country-level avg. export variance, F
ctry_detrended_var_yr_to_yr, country-year-level detrended export variance, T
ctry_detrended_var, country-level detrended export variance, F
ctry_churn_rate_yr_to_yr, country-year-level exporter churn rate, T
ctry_churn_rate, country-level exporter churn rate, F
ctry_immediate_failure_rate_yr_to_yr, country-year-level export failure rate, T
ctry_immediate_failure_rate, country-level export failure rate, F
network_closeness_inc_france, log avg distance to export markets, T
network_closeness_excl_france, log avg distance to export markets, T") %>% mutate(time_varying = as.booltype(time_varying))


firm_yr_ctry = import_file(firm_yr_ctry_path) %>% 
  .[, ever_export_customs:=any(currently_export_customs == T), by=.(ctry_num, firmid_num)] %>%
  .[, ctry_num_year := .GRP, by = .(ctry_num, year)]

# 3a test different base specifications --------------------------------------------------------
#define variations 
{
rev_variations = data.table(restriction = restrictions) %>% rowwise() %>%
  mutate(command = gsub('firm_yr_ctry', paste0('firm_yr_ctry', restriction), base_command)) %>% 
  mutate(dep_var = 'rev')

currently_export_variations = data.table(restriction = restrictions[c(1, 2, 4)]) %>% rowwise() %>% 
  mutate(command = gsub('firm_yr_ctry', paste0('firm_yr_ctry', restriction), base_command)) %>% mutate(command = 
  gsub('feols', 'feglm', command) %>% gsub(')', ", family = 'binomial')",.) %>%  # update the regression type 
  gsub('log_export_rev_customs', 'currently_export_customs',.)) %>% # update the dep var 
  mutate(dep_var = 'currently_export')

streak_death_variations = data.table(command = base_command %>% 
  gsub('feols', 'feglm', .) %>% gsub(')', ", family = 'binomial')",.) %>%  # update the regression type 
  gsub('log_export_rev_customs', 'is_streak_death',.) %>% # update the dep var
  gsub('\\| firmid', '+ log_years_since_streak_start | firmid',. )) %>% # update controls 
  mutate(dep_var = 'streak_death')

detrended_var_variations = data.table(command = base_command %>% 
  gsub('log_export_rev_customs', 'log_export_rev_customs_cond_detrended_var',.) %>% # update the dep var                            
  gsub('\\| firmid', '+ log_years_since_streak_start | firmid',. )) %>% # update controls 
  mutate(dep_var = 'detrended_var')

variations = rbindlist(list(rev_variations, currently_export_variations, streak_death_variations, detrended_var_variations),
  use.names = T, fill = T)  %>%
  
  ## add fixed effect variations 
  mutate(fe = 'firm, ctry, year') %>%  # baseline 
  bind_rows(mutate(.,command = gsub("\\| firmid_num", ' | NACE_BR', command), fe = 'industry, ctry, year'),
            mutate(.,command = gsub("year \\+ ctry_num", 'ctry_num_year', command), fe = 'firm, ctry_year')) %>% 
  
 ## add extensive margin variations 
 bind_rows(mutate(.[fe == 'firm, ctry, year'], command = gsub('log_comp_data', 'use_data', command))) %>%
 .[,extensive_margin := grepl('use_data', command)] %>% 

 ## add index and cleanup 
.[, idx := .I] %>% select(idx, dep_var, restriction, fe, extensive_margin, everything())
}

# run variations 
if (!dummy_version){
  model_output = evaluate_variations(variations)
  if (nrow(model_output$failed_output) >0) print('CHECK WHAT WENT WRONG WITH REGRESSIONS')
  write_rds(model_output,paste0(raw_output_dir, "3a_ctry_lvl_baseline_analysis.RDS"))
}

## output table includes the rev / entry results under 
model_output = import_file(de_dummy(raw_output_dir), "3a_ctry_lvl_baseline_analysis.RDS")
reg_lists = c(1,5,4,7,2,6,3) %>% list(., . +9, . + 18, .+27)
label_list = gpaste('3a.', 1:4,"_rev_entry (", c('base','industry_fe', 'ctry_yr_fe', 'ext_margin'), ")", no_expand =T )
notes_list = "Robust Standard Errors clustered at the firm level. All Regressions include firm, year, and country FE." %>%c(
  ., gsub('firm,', 'industry,',.), gsub(', year, and country', ' and country-year',.), .) 
final_commands_list = rep(c('', "table = gsub('log payroll', 'use', table)"), each = 2)

for (i in 1:4){
format_table(model_output[reg_lists[[i]]], label = label_list[i],
             coef_names = if (i != 3){base_coef_names}else{con_fil(base_coef_names, 'popularity', inc =F)},
             headers = paste0('Sample',sub("2(?!.*2)", "1", perl= T, make_headers(2,  rep(c('Ever Exporter', 'Currently Exporter'), each = 2))),
                              'Restriction',sub("2(?!.*2)", "1", perl= T, make_headers(2,  rep(c('(overall)', '(to country)'),  2)))),
             column_names = c(rep(c('Log Rev', 'P(In Country)'), 3), 'Log Rev'),
             divisions_before = c(3,5,7),
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 12,
             notes = notes_list[i],
             final_commands = final_commands_list[i],
             note_width = 1,
             output_path =  paste0(de_dummy(finished_output_dir), label_list[i], '.tex'), make_tex = F)
}
  
# 3b variation analysis  --------------------------------------------------
base_variations = variations[c(3, 7),] %>% .[,idx := .I]
interaction_variations = base_variations %>% bind_rows(
  rbindlist(lapply(1:nrow(interaction_vars_and_terms),function(i){
  t_variations = base_variations
  interaction = interaction_vars_and_terms$var[i]; interaction_name = interaction_vars_and_terms$term[i]
  ind_var_options = gpaste(c('log_comp_data', 'use_data'), c('_lag1', ''))
  for (option in ind_var_options){
    t_variations = t_variations %>% mutate(
      command = gsub(paste0("(?<!_lag1)\\b", option, "\\b"),paste0(option, "*", interaction),command,perl = TRUE))
  }
  t_variations = t_variations %>% mutate(interaction = interaction_name)
}))) %>% mutate(idx = 1:nrow(.))

if (!dummy_version){
  model_output = evaluate_variations(interaction_variations)
  if (nrow(model_output$failed_output) >0) print('CHECK WHAT WENT WRONG WITH REGRESSIONS')
  write_rds(model_output$model_output,paste0(raw_output_dir,"3b_ctry_lvl_interaction_analysis.RDS"))
}
model_output = import_file(de_dummy(raw_output_dir), "3b_ctry_lvl_interaction_analysis.RDS")

## output select tables 
lapply(c('grav_region', 'log_grav_dist'), function(c_var){
i = match(c_var, interaction_vars_and_terms$var)
c_term = interaction_vars_and_terms$term[i]; 
c_time_varying = interaction_vars_and_terms$time_varying[i]
c_indices = interaction_variations[interaction %in% c(NA,c_term)] %>% arrange(desc(dep_var)) %>% pull(idx)
c_model_output = model_output[c_indices]
c_label = paste0('3b_',c_var);
# assign variable names
if(c_time_varying){
  c_coef_names = c(base_coef_names,gpaste( c("", "\\hspace{5 pt}lx ", "\\hspace{5 pt}x "), c_term)); end = length(c_coef_names)
  c_coef_order = c(1,end, 2, end-1, end-2, 3:(end-3))
}else{
  c_coef_names = c(base_coef_names,gpaste( c("lx ", "x "), c_term)); end = length(c_coef_names)
  c_coef_order = c(1,end, 2, end-1,  3:(end-2))
}

format_table(
  c_model_output, label = c_label,
  coef_names = c_coef_names,
  coef_order = c_coef_order,
  headers = make_headers(2,  rep(c('Log Revenue', 'In Country'))),
  divisions_before = 3,
  rescale_factor = 1,
  custom_rows = list(""),
  custom_row_placement = ifelse(c_time_varying, 18,16),
  final_commands = "table = gsub('lx','x', table)",
  notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm, year, and country FE.",
  note_width = .7,
  output_path =  paste0(de_dummy(finished_output_dir), c_label, '.tex'), make_tex = F)
})













