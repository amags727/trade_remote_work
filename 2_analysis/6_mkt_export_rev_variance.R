

# run regressions  --------------------------------------------------------
base_data = import_file(file.path(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet')) 
  
#set parameters 
interactions_1 = c('log_num_markets','first_time_in_ctry',  'first_time_exporting')
interactions_2 = gpaste('log_comp_', c('now', 'l5', 'ever'))
interactions_3 = gpaste(c('mkt_', 'nace_mkt_'), c('churn_rate', 'failure_rate', gpaste('de_trended_log_variance_', c('ind','group'), '_lvl')))  
interactions_4 = c('log_distance_to_france', 'log_mkt_size_rev', 'mkt_share_active_exporters') %>% c(., gsub('mkt', 'nace_mkt',.))
controls =  "+ log_export_rev_customs + log_dom_turnover + log_min_age + log_comp_rnd + comp_weighted_prestige"
young_filter = gpaste('base_data',c('', '[young_at_start == T]', '[young_at_start == F]'))
fe = '| NACE_BR +years_observed + min_year +ctry'

# setup blocks and run 
for ( i in 1:4){
  file_path = paste0("3) output/0_raw_output/6",letters[i],"_output_raw.rds")
  interactions = c("", gpaste("*",get(paste0('interactions_',i))))
  block = expand(young_filter, interactions, names = c('dataset', 'interaction')) %>%
    rowwise() %>% mutate(command = reg_command(
      dataset,
      dep_var = 'detrended_var_log_export_rev_customs',
      ind_var = 'log_comp_data', 
      controls = paste0(interaction, controls),
      fe = fe, 
      cluster = 'firmid'
    ))
  if (running_regressions) write_rds(evaluate_variations(block),file_path)
  assign(paste0('block_6',letters[i]),block)
  assign(paste0('block_6',letters[i],'_output'),read_rds(file_path))
  rm(block, interactions, file_path)
}


# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()



