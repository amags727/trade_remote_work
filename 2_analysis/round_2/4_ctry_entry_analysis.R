# set proper directory -------------------------------------------------------------------
analysis_round = 2
output_base = paste0('3) output/',letters[analysis_round],"_round_",analysis_round,"_analysis/")
suppressWarnings(dir.create(output_base))
raw_output_dir = paste0(output_base,letters[analysis_round],"1_raw_output/")
finished_output_dir = paste0(output_base,letters[analysis_round],"2_finished_tables/")
lapply(c(raw_output_dir, finished_output_dir),function(x) suppressWarnings(dir.create(x)))
rm(output_base)

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
# export regressions  --------------------------------------------------------
base_vars= c(gpaste('log ', c('comp data','dom.\nrevenue', 'comp r\\&d')),  'avg worker\nprestige')
entry_notes = 'Robust standard errors clustered at the firm level. All regressions include industry, country, and year FE.'
interactions_1 = gpaste(c('', '\\hspace{5 pt} x '), c('log num markets', 'not yet exported'),
                 order = 2:1)
interactions_2= gpaste('\\multicolumn{1}{r}{', c('currently in mkt','\\hspace{5 pt}with recent mkt exp.', 'with any mkt exp.'), "}",
                              c('', 'x\n\\multicolumn{1}{r}{log comp data}')) %>% gsub('}x', " x}",.) 


## 4a 
output = import_file(gpaste(raw_output_dir,'4a_output_raw.rds')); for (name in names(output)) assign(name, output[[name]])
label = '4a_entry_timing_x_export_exp'
table = format_table(model_output, label = label, 
                     header = gsub('\\{4\\}', '{3}',age_header),
                     divisions_before = c(4,7), rescale_factor = 1,
                     output_path = paste0(finished_output_dir, label,'.tex'),
                     coef_order = c(1,6,8,5,7,2:4),
                     spacer_size = .25,
                     coef_names = c(base_vars, interactions_1),
                     notes = entry_notes,
                     note_width = 1.2,
                     cox = T)

## 4b 
output = import_file(gpaste(raw_output_dir,'4b_output_raw.rds')); for (name in names(output)) assign(name, output[[name]])
label = '4b_export_entry_x_worker_experience'
table = format_table(model_output, label = label, 
                     header = age_header,
                     divisions_before = c(5,9), rescale_factor = 1,
                     output_path = paste0(finished_output_dir, label,'.tex'),
                     coef_order = c(1,5:10, 2:4),
                     spacer_size = .25,
                     coef_names = c(base_vars, interactions_2),
                     custom_row_placement = 10,
                     custom_rows = list(c('Log comp. for employees:', rep('', 12))),
                     notes = entry_notes,
                     note_width = 1.2,
                     cox = T)
# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()

