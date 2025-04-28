# import data and run regressions ----------------------------------------------------------------------

# import the data 
base_data = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet'))

#set parameters 
interactions_1 = c('log_num_markets','first_time_in_ctry',  'first_time_exporting')
interactions_2 = gpaste('log_comp_', c('now', 'l5', 'ever'))
interactions_3 = gpaste(c('mkt_', 'nace_mkt_'), c('churn_rate', 'failure_rate', gpaste('de_trended_log_variance_', c('ind','group'), '_lvl')))
interactions_4 = c('log_distance_to_france', 'log_mkt_size_rev', 'mkt_share_active_exporters') %>% c(., gsub('mkt', 'nace_mkt',.))
base_controls =  "+ log_age + log_dom_turnover + log_comp_rnd + comp_weighted_prestige"
cox_controls  =  "+ log_dom_turnover + log_comp_rnd + comp_weighted_prestige"
young_filter = gpaste('base_data',c('', '[young == T]', '[young == F]'))
variations = expand(c(F,T) ,1:4, names = c('cox', 'interaction_num'))

# run the regressions 
for (i in 1:nrow(variations)){
  file_path = paste0("3) output/0_raw_output/3",letters[i],"_output_raw.rds")
  interactions =  c("", gpaste("*",get(paste0('interactions_',variations$interaction_num[i]))))
  block = expand(young_filter,interactions, variations$cox[i], names = c('filter', 'interaction', 'cox')) %>% mutate(
    controls = ifelse(cox, cox_controls, base_controls),
    dep_var = ifelse(cox,  'last_year_of_streak','log_export_rev_customs'),
    fe= ifelse(cox, 'NACE_BR, ctry, year',"| NACE_BR + ctry + year"),
    family = ifelse(cox, 'cox', 'feols')) %>%
    rowwise() %>% mutate(
    command = reg_command(filter, dep_var, 'log_comp_data', paste0(interaction, controls), fe,
                          cluster = 'firmid', family = family, time_var = 'streak_age')
    )
  if (running_regressions) write_rds(evaluate_variations(block),file_path)
  assign(paste0('block_3',letters[i]),block)
  assign(paste0('block_3',letters[i],'_output'),read_rds(file_path))
  rm(block, interactions, file_path)
}


# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()


