

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


# run regressions  --------------------------------------------------------
table_notes =  ' Robust standard errors clustered at the firm level. All regressions include industry, country, streak duration, and streak start year FE'
base = c(gpaste('log ', c('comp data', 'mkt rev', 'dom.\nrevenue','age', 'comp r\\&d')),  'avg worker\nprestige')
interactions_1_table = gpaste(c('', '\\hspace{5 pt} x '), 
                              c('log num markets', 'first time in market', 'first time exporting'),
                              order = 2:1)
interactions_2_table = gpaste('\\multicolumn{1}{r}{', c('currently in mkt','\\hspace{5 pt}with recent mkt exp.', 'with any mkt exp.'), "}",
                              c('', 'x\n\\multicolumn{1}{r}{log comp data}')) %>% gsub('}x', " x}",.) 

interactions_3_table = gpaste(c('','\\multicolumn{1}{r}{x '),'mkt ', c('churn rate', 'fail rate', 'avg. firm var', 'var'),
                              order = 3:1) %>% index_paste(., grepl('multi',.), '}')
interactions_4_table = gpaste(c('','\\multicolumn{1}{r}{x '), c('log mkt dist.', 'log mkt size', 'mkt pop.'),
                              order = 2:1) %>% index_paste(., grepl('multi',.), '}')



# output 6a  ------------------------------------------------------------------
for (name in names(block_6a_output)) assign(name, block_6a_output[[name]])
label = '6a_ctry_var_x_firm_experience'
format_table(model_output,
             label = label,
             coef_names =  c(base, interactions_1_table),
             headers = age_header,
             divisions_before = c(5,9),
             coef_order = c(1,8,10, 12,7,9,11,2:6),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex')
)


# output 6b ------------------------------------------------------------------
for (name in names(block_6b_output)) assign(name, block_6b_output[[name]])


label = '6b_ctry_var_x_firm_employ_abroad'
format_table(model_output,
             label = label,
             coef_names =  c(base, interactions_2_table),
             headers = age_header,
             divisions_before = c(5,9),
             coef_order = c(1,7:12,2:6),
             custom_row_placement = 10,
             custom_rows = list(c('Log comp. for employees:', rep('', 12))),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex')
)


# output 6c ---------------------------------------------------------------
for (name in names(block_6c_output)) assign(name, block_6c_output[[name]])
for (i in 1:2){
  if (i == 1){ 
    model_inputs = model_output[c(1:5, 10:14,19:23)]
    label = '6c_mkt_char_1_mkt_lvl'; 
    coef_names = c(base,interactions_3_table)
    
  }
  if(i == 2){
    model_inputs = model_output[c(1,6:10, 15:19, 24:27)]
    label = '6c_mkt_char_1_mkt_nace_lvl';
    coef_names = c(base,interactions_3_table)
  }
  
  format_table(model_inputs,
               label = label,
               coef_names =  coef_names,
               headers = gsub('\\{4\\}', '{5}',age_header),
               divisions_before = c(6,11),
               coef_order = c(1,8,10,12,14,7,9,11,13,2:6),
               notes = table_notes,
               note_width = 1.2,
               rescale_factor = 1,
               output_path = paste0("3) output/", label,'.tex')
  )
}

# output 6d ---------------------------------------------------------------
for (name in names(block_6d_output)) assign(name, block_6d_output[[name]])
for (i in 1:2){
  if (i == 1){ 
    model_inputs = model_output[c(1:4, 8:11, 15:18)]
    label = '6d_mkt_char_2_mkt_lvl'; 
  }
  if(i == 2){
    model_inputs = model_output[c(1,5:8,12:15, 19:21)]
    label = '6d_mkt_char_2_mkt_nace_lvl';
  }
  
  format_table(model_inputs,
               label = label,
               coef_names =  c(base, interactions_4_table[-1]),
               headers = age_header,
               divisions_before = c(5,9),
               coef_order =  c(1,7,9,11,8,10,2:6),
               notes = table_notes,
               note_width = 1.2,
               rescale_factor = 1,
               output_path = paste0("3) output/", label,'.tex')
               )
}






# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()



