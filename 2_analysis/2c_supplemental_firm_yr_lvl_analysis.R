# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}; gc();

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


#import data / set parameters  -----------------------------------------------------------------------
firm_yr = import_file(firm_yr_path)

base_controls =  paste("", 'log_comp_total', 'log_comp_total_lag1', 'log_dom_turnover', 'log_dom_turnover_sq',
                       'avg_prestige_total', 'share_empl_college', "capital_intensity", 'log_age', sep = " + ")

rev_command = reg_command(dataset = 'firm_yr', dep_var =  'log_total_export_rev_BS', ind_var = 'log_comp_data + log_comp_data_lag1', 
                           controls = base_controls, fe = "| firmid_num + year", cluster = 'firmid_num')
entry_command = reg_command(dataset = 'firm_yr', dep_var = 'currently_export_BS', 'log_comp_data + log_comp_data_lag1', 
                            controls = base_controls, fe = "| firmid_num + year", cluster = 'firmid_num', family = 'binomial')
death_command = reg_command(dataset = 'firm_yr', dep_var = 'stop_exporting_BS', ind_var =  'log_comp_data + log_comp_data_lag1',
                            controls = gsub('log_age','log_export_streak_age_BS', base_controls),
                            fe = "| firmid_num + year", cluster = 'firmid_num', family = 'binomial')
detrended_var_command = reg_command(dataset = 'firm_yr', dep_var = 'log_total_export_rev_BS_cond_detrended_var', ind_var = 'log_comp_data + log_comp_data_lag1',
                                    controls = gsub('log_age','log_export_streak_age_BS', base_controls), fe = "| firmid_num + year", cluster = 'firmid_num')

unq_prods_command = reg_command(dataset = 'firm_yr', dep_var ='num_unique_export_products', ind_var =  'log_comp_data + log_comp_data_lag1',
                                controls = base_controls, fe = "| firmid_num + year", cluster = 'firmid_num', family = 'poisson')

### make interaction boiler plate 
for (command_type in c('rev','entry','death','detrended_var','unq_prods')){
  assign(paste0('int_', command_type, '_command'),
         gsub('log_comp_data \\+ log_comp_data_lag1',
              'log_comp_data*interaction + log_comp_data_lag1*interaction',
              get(paste0(command_type, "_command"))))
  }

interactions = c('log_years_since_first_export_year_BS', 'log_export_streak_age_BS', 'log_age', 
                 'for_to_dom_rev_ratio_BS_init','log_num_mkts',  'log_num_mkts_init','log_dom_turnover', 'log_dom_turnover_init', 'log_empl', 'log_empl_init',
                 'log_comp_non_data_rnd', 'nace_HHI_dom', 'nace_share_dom', 'nace_share_dom_init')

restrictions = fread("underlying, discrete_var, string
      years_since_first_export_year_BS, years_since_first_export_year_BS_bracket, T
      export_streak_age_BS, export_streak_age_BS_bracket, T
      age, age_bracket, T
      age, young, F
      log_num_mkts, num_mkts_bracket, T
      log_num_mkts_init, num_mkts_bracket_init, T
      log_empl, empl_bin,T
      log_empl_init, empl_bin_init,T
      dom_turnover, dom_turnover_quartile, F
      dom_turnover_quartile_init, dom_turnover_quartile_init, F
      nace_comp_non_data_rnd_quartile, nace_comp_non_data_rnd_quartile, F
      nace_leader_dom, nace_leader_dom, F")
restrictions = rbindlist(lapply(1:nrow(restrictions), function(i){
discrete_var = restrictions$discrete_var[i]; string_i = restrictions$string[i]
ordered_levels = setdiff(firm_yr %>% arrange(get(restrictions$underlying[i])) %>% pull(discrete_var) %>% unique(), NA)
data.frame(restriction_var =  discrete_var, restriction = gpaste('[',discrete_var, '==',
           ifelse(string_i, "'",""), ordered_levels, ifelse(string_i, "'",""), ']'))
})) %>% as.data.table()


# 2 generate variations ----------------------------------------------------------------------

## BASE 
base_variations = data.table(dep_var = c('rev','entry','death','detrended_var', 'unq_prods'), extra_control = NA_character_,
                             interaction_var = NA_character_, restriction_var = NA_character_, restriction = NA_character_) %>% rowwise() %>% 
  mutate(command = get(paste0(dep_var, "_command"))) %>% as.data.table()

### ADD CONTROLS 
extra_control_variations = base_variations %>% mutate(
  extra_control = 'log_comp_non_data_rnd',
  command = gsub('total_lag1', 'total_lag1 + log_comp_non_data_rnd + log_comp_non_data_rnd_lag1', command)) 
  
### ADD INTERACTIONS 
interaction_variations = data.table(dep_var = rep(c('rev','entry','death','detrended_var', 'unq_prods'), each = length(interactions)),
                                    interaction_var = rep(interactions, 5))  %>% rowwise() %>% 
  mutate(command = gsub('interaction', interaction_var, get(paste0('int_',dep_var, "_command")))) %>% as.data.table() %>% 
.[!(dep_var == 'entry' & (interaction_var %in% c('log_export_streak_age_BS', 'log_num_mkts')))]

### ADD RESTRICTIONS 
restriction_variations = data.table(dep_var = rep(c('rev','entry','death','detrended_var', 'unq_prods'),each = nrow(restrictions))) %>%
  cbind(restrictions[rep(seq_len(nrow(restrictions)), 5), ]) %>% rowwise() %>% 
  mutate( command =  gsub('firm_yr',paste0('firm_yr',restriction),  get(paste0(dep_var, "_command")))) %>% as.data.table() %>% 
  .[!(dep_var == 'entry' & (restriction_var %in% c('export_streak_age_BS_bracket', 'num_mkts_bracket')))]

variations = rbindlist(list(base_variations, interaction_variations, restriction_variations), fill = T, use.names = T )

if (!dummy_version){
  model_output = evaluate_variations(variations)
  if(nrow(model_output$failed_output)!= 0) print('CHECK WHAT WENT WRONG')
  model_output$variations = variations
  write_rds(model_output, paste0(raw_output_dir, '2c_firm_yr_supp_variations.rds'))
  
  #### IF TAKING PICTURES 
  View(variations)
  View(model_output$variation_output %>% rowwise() %>%  filter(grepl('data', regressor) | grepl(interaction_var, regressor)) %>% 
      select(counter, regressor, p_val, con_fil(., names(variations)), everything()))
}


# generate output tables -----------------------------------------------------------------------
# reg_output = import_file(de_dummy(raw_output_dir), '2c_firm_yr_supp_variations.rds')
# for (name in names(reg_output)){assign(name, reg_output[[name]])}
# 
# graph_inputs = variation_output[!is.na(quartile_num) & grepl('data|comp_total', regressor)] %>%
#   .[,quartile_num := as.factor(quartile_num)] %>% 
#   .[,regressor := ifelse(grepl('data', regressor),'data', 'total')] %>% 
#   .[, entry := rep(rep(c(F,F,T), each = 16),2)] %>% 
#   .[, .(lb = sum(lb), ub = sum(ub), coef = sum(coef)), by = .(regressor,quartile_num, entry, intensive_margin,version)] %>% 
#   .[, group := .GRP, by = .(intensive_margin, version, entry)] %>% 
#   arrange(group, regressor, quartile_num) %>% 
#   select(regressor, quartile_num, lb, coef,ub, everything())
# 
# 
# g1 = pretrend_graph(
#   graph_inputs[group == 1], 'quartile_num', 'regressor',
#   subtitle = 'Export Rev') +
#   theme_minimal() + theme(legend.position = 'none') + labs(y = 'elasticity')
# g3 = pretrend_graph(
#   graph_inputs[group == 3], 'quartile_num', 'regressor',
#   subtitle = 'P(exporting)') + theme_minimal() + labs(color = 'Payroll\nType', y = 'log odds')
# 
# output_graph  = g1 + g3 + plot_annotation(title = 'Two Year Impact of Payroll Changes', 
#                           caption = "Firm Size Quartile") & 
#   theme(plot.caption = element_text(hjust = 0.45, size = 12, vjust = 5))
# 
# ggsave(paste0(de_dummy(finished_output_dir),'2c.1_quartile_regs.png'), output_graph, width = 7.71, height = 5.96)
# 






