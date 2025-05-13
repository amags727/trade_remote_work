# set proper directory -------------------------------------------------------------------
analysis_round = 2
output_base = paste0('3) output/',letters[analysis_round],"_round_",analysis_round,"_analysis/")
suppressWarnings(dir.create(output_base))
raw_output_dir = paste0(output_base,letters[analysis_round],"1_raw_output/")
finished_output_dir = paste0(output_base,letters[analysis_round],"2_finished_tables/")
lapply(c(raw_output_dir, finished_output_dir),function(x) suppressWarnings(dir.create(x)))
rm(output_base)

# run regressions -------------------------------------------------------------------
base_data = import_file(file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet'))

dom_young_filter = gpaste('base_data',c('', '[young_at_start_dom_rev_observed == T]', '[young_at_start_dom_rev_observed == F]'))
export_young_filter = gsub('dom_rev', 'exports', dom_young_filter)

dom_controls = '+log_dom_turnover + log_comp_rnd + comp_weighted_prestige +log_min_age_dom_rev_observed'
export_controls = gsub('dom_rev', 'exports', paste0("+log_total_export_rev_customs", dom_controls))

dom_fe = "| NACE_BR + years_dom_rev_observed + min_first_year_dom_rev_observed"
export_fe = gsub('dom_rev', 'exports',dom_fe)

interactions_1 = c("nace_churn_rate", "nace_de_trended_log_variance_ind_lvl", "nace_de_trended_log_variance_group_lvl")
interactions_2 = c("export_mkt_avg_rev_wgted_comp_now", "export_mkt_avg_rev_wgted_comp_l5", "export_mkt_avg_rev_wgted_comp_ever")


variations = rbind(data.frame(dom = T, interaction_num = 1),
      expand(c(F),1:2, names = c('dom', 'interaction_num')))

for (i in 1:nrow(variations)){
  for (name in names(variations)) assign(name, variations[[name]][i])
  file_path = paste0(raw_output_dir,"5",letters[i],"_output_raw.rds")
  filter = if(dom) dom_young_filter else export_young_filter
  interaction = c("", gpaste("*",get(paste0('interactions_', interaction_num))))
  
  block = expand(filter, interaction, names = c('dataset', 'interaction')) %>%
    rowwise() %>% mutate(command =reg_command(
      dataset,
      dep_var = paste0('detrended_var_log_', ifelse(dom,'dom_turnover', 'total_export_rev_customs')),
      ind_var = 'log_comp_data', 
      controls = paste0(interaction,ifelse(dom, dom_controls, export_controls)),
      fe = ifelse(dom, dom_fe, export_fe),
      cluster = "NACE_BR"))
  
  if (running_regressions) write_rds(evaluate_variations(block),file_path)
  rm(block, interaction, file_path)
}
                     

# generate output  --------------------------------------------------
interactions_1 = gpaste(c('','\\multicolumn{1}{r}{x '),'NACE ', c('churn rate', 'avg. firm var', 'var'),
                              order = 3:1) %>% index_paste(., grepl('multi',.), '}')
interactions_2 = gpaste('\\multicolumn{1}{r}{', c('currently in mkt','\\hspace{5 pt}with recent mkt exp.', 'with any mkt exp.'), "}",
                              c('', 'x\n\\multicolumn{1}{r}{log comp data}')) %>% gsub('}x', " x}",.) 

dom_controls = c(gpaste('log ', c('comp data', 'dom.\nrevenue', 'comp r\\&d', 'age')),  'avg worker\nprestige') %>% .[c(1:3,5,4)]
export_controls = append(dom_controls, 'log export\nrevenue', after = 1)  
table_notes = paste0('Robust standard errors clustered at the firm level. All regressions include industry,',
                     'initial-year, and observation period length fixed effects')


## output 5a
output = import_file(gpaste(raw_output_dir,'5a_output_raw.rds')); for (name in names(output)) assign(name, output[[name]])
label = '5a_dom_rev_variance_x_nace_char'
format_table(model_output,
             label = label,
             coef_names =  c(dom_controls, interactions_1),
             headers = age_header, 
             divisions_before = c(5,9),
             coef_order = c(1,7,9,11,6,8,10,2:5),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0(finished_output_dir, label,'.tex')
)

## output 5b 
output = import_file(gpaste(raw_output_dir,'5b_output_raw.rds')); for (name in names(output)) assign(name, output[[name]])
label = '5b_total_export_rev_variance_x_nace_char'
format_table(model_output,
             label = label,
             coef_names =  c(export_controls, interactions_1),
             headers = age_header, 
             divisions_before = c(5,9),
             coef_order = c(1,8,10,12,7,9,11,2:6),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0(finished_output_dir, label,'.tex')
)


## output 5c
output = import_file(gpaste(raw_output_dir,'5c_output_raw.rds')); for (name in names(output)) assign(name, output[[name]])
label = '5c_total_export_rev_variance_x_worker_exp'
format_table(model_output,
             label = label,
             coef_names =  c(export_controls, interactions_2),
             headers = age_header, 
             divisions_before = c(5,9),
             coef_order = c(1,7:12,2:6),
             custom_row_placement = 10,
             custom_rows = list(c('Log comp. for employees:', rep('', 12))),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0(finished_output_dir, label,'.tex')
)






# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()


  