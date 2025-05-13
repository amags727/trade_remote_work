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

# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()

