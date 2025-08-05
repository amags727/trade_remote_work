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

## set table versions of each parameter set
table_notes =  ' Robust standard errors clustered at the firm level. All regressions include industry, country, and year FE.'
feols_base_table = c(gpaste('log ', c('comp data', 'age', 'dom.\nrevenue', 'comp r\\&d')),  'avg worker\nprestige')
interactions_1_table = gpaste(c('', '\\hspace{5 pt} x '), 
                              c('log num markets', 'first time in market', 'first time exporting'),
                              order = 2:1)
interactions_2_table = gpaste('\\multicolumn{1}{r}{', c('currently in mkt','\\hspace{5 pt}with recent mkt exp.', 'with any mkt exp.'), "}",
                              c('', 'x\n\\multicolumn{1}{r}{log comp data}')) %>% gsub('}x', " x}",.) 

interactions_3_table = gpaste(c('','\\multicolumn{1}{r}{x '),'mkt ', c('churn rate', 'fail rate', 'avg. firm var', 'var'),
                              order = 3:1) %>% index_paste(., grepl('multi',.), '}')
interactions_4_table = gpaste(c('','\\multicolumn{1}{r}{x '), c('log mkt dist.', 'log mkt size', 'mkt pop.'),
                              order = 2:1) %>% index_paste(., grepl('multi',.), '}')


  

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

# Output 3a  -----------------------------------------------------
for (name in names(block_3a_output)) assign(name, block_3a_output[[name]])
label = '3a_ctry_rev_x_firm_experience'
table = format_table(model_output,
             label = label,
             coef_names =  c(feols_base_table, interactions_1_table),
             headers = age_header,
             divisions_before = c(5,9),
             coef_order = c(1,7,9,11,6,8,10,2:5),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex')
             )
            
# Output 3b ---------------------------------------------------------------
for (name in names(block_3b_output)) assign(name, block_3b_output[[name]])

label = '3b_ctry_rev_x_firm_employ_abroad'
format_table(model_output,
             label = label,
             coef_names =  c(feols_base_table, interactions_2_table),
             headers = age_header,
             divisions_before = c(5,9),
             coef_order = c(1,6:11,2:5),
             custom_row_placement = 10,
             custom_rows = list(c('Log comp. for employees:', rep('', 12))),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex')
)


# output 3c ---------------------------------------------------------------
for (name in names(block_3c_output)) assign(name, block_3c_output[[name]])
for (i in 1:2){
if (i == 1){ 
  model_inputs = model_output[c(1:5, 10:14,19:23)]
  label = '3c_mkt_char_1_mkt_lvl'; 
  coef_names = c(feols_base_table,interactions_3_table[-5][c(1:5,7,6)])
  coef_order = c(1,7,9,10,11,6,8,2:5)
}
if(i == 2){
  model_inputs = model_output[c(1,6:10, 15:19, 24:27)]
  label = '3c_mkt_char_1_mkt_nace_lvl';
  coef_names = c(feols_base_table,interactions_3_table)
  coef_order = c(1,7,9,11,13,6,8,2:5)
}

format_table(model_inputs,
             label = label,
             coef_names =  coef_names,
             headers = gsub('\\{4\\}', '{5}',age_header),
             divisions_before = c(6,11),
             coef_order = coef_order,
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex')
            )
}
# output 3d ---------------------------------------------------------------
for (name in names(block_3d_output)) assign(name, block_3d_output[[name]])
for (i in 1:2){
if (i == 1){ 
  model_inputs = model_output[c(1:4, 8:11, 15:18)]
  label = '3d_mkt_char_2_mkt_lvl'; 
}
if(i == 2){
  model_inputs = model_output[c(1,5:8,12:15, 19:21)]
  label = '3d_mkt_char_2_mkt_nace_lvl';
}

format_table(model_inputs,
             label = label,
             coef_names =  c(feols_base_table, interactions_4_table),
             headers = age_header,
             divisions_before = c(5,9),
             coef_order =  c(1,7,9,11,6,8,10,2:5),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex'),
            final_commands = "table = table[-c(18,19)]; table = append(table,'\\\\\\\\',after = 17)"
)
}



# output 3e ---------------------------------------------------------------
for (name in names(block_3e_output)) assign(name, block_3e_output[[name]])
label = '3e_ctry_duration_x_firm_experience'
format_table(model_output,
             label = label,
             coef_names =  c(feols_base_table[-2], interactions_1_table),
             headers = age_header,
             divisions_before = c(5,9),
             coef_order = c(1,6,8,10,5,7,9,2:4),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex'),
             cox = T
)


# output 3f ---------------------------------------------------------------
for (name in names(block_3f_output)) assign(name, block_3f_output[[name]])
model_output[1] = block_3e_output$model_output[1]
label = '3f_ctry_duration_x_firm_employ_abroad'
format_table(model_output,
             label = label,
             coef_names =  c(feols_base_table[-2], interactions_2_table),
             headers = age_header,
             divisions_before = c(5,9),
             coef_order = c(1,5:10,2:4),
             custom_row_placement = 10,
             custom_rows = list(c('Log comp. for employees:', rep('', 12))),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex'),
             cox = T
)

# output 3g ---------------------------------------------------------------
for (name in names(block_3g_output)) assign(name, block_3g_output[[name]])
block_3g = block_3g %>% as.data.table() %>% mutate(num = 1:nrow(.))
for( i in 1:2){
if (i == 1){
 model_inputs = block_3g[!grepl('nace', interaction)][['num']]  %>% model_output[.]
 label = '3g_ctry_duration_x_mark_char_1_mkt_lvl'
}
if (i == 2){
  model_inputs = block_3g[interaction =='' | (grepl('\\*n', interaction))][['num']] %>% model_output[.]
  label = '3g_ctry_duration_x_mark_char_1_nace_lvl'
  
}

format_table(model_inputs,
             label = label,
             coef_names =  c(feols_base_table[-2], interactions_3_table),
             headers = age_header,
             divisions_before = c(6,11),
             coef_order = c(1,6,8,10,12,2:4),
             notes = table_notes,
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0("3) output/", label,'.tex'),
             cox = T
)
}

# output 3h ---------------------------------------------------------------
for (name in names(block_3h_output)) assign(name, block_3_output[[name]])
for (i in 1:2){
  if (i == 1){ 
    model_inputs = model_output[c(1:4, 8:11, 15:18)]
    label = '3h_ctry_duration_mkt_char_2_mkt_lvl'; 
  }
  if(i == 2){
    model_inputs = model_output[c(1,5:8,12:15, 19:21)]
    label = '3h_ctry_duration_mkt_char_2_mkt_nace_lvl';
  }
  
  format_table(model_inputs,
               label = label,
               coef_names =  c(feols_base_table[-2], interactions_4_table),
               headers = age_header,
               divisions_before = c(5,9),
               coef_order =  c(1,6,8,10,2:4),
               notes = table_notes,
               note_width = 1.2,
               rescale_factor = 1,
               output_path = paste0("3) output/", label,'.tex'),
                            cox = T
  )
}






# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()


