# Setup -------------------------------------------------------------------
## set parameter values 
uncertainty_values = c("nace_churn_rate", "nace_de_trended_log_variance_ind_lvl", "nace_de_trended_log_variance_group_lvl")
overseas_employment_vars = c("export_mkt_avg_rev_wgted_comp_now", "export_mkt_avg_rev_wgted_comp_l5", "export_mkt_avg_rev_wgted_comp_ever")
young_filter = c('', '[young == T]', '[young == F]')
young_and_export_filter = c(young_filter,'[currently_export ==T]','[currently_export ==T & young == T]','[currently_export ==T & young == F]')
entry_filter = gpaste('[year <= first_export_year ', c("", gpaste('& young_at_start ==',c('T','F'))),']')

## import data 
base_data = import_file(file.path(inputs_dir,'16c_firm_yr_lvl.parquet')) 
if(dummy_version){ 
  discrete_rand = c('young', 'currently_export', 'young_at_start')
  base_data[,(discrete_rand) := sample(c(TRUE, FALSE), .N, replace = TRUE)]
  base_data[,nace_churn_rate := runif(.N)]
}

# 2a) total dom revenue ----------------------------------------------------------------------
block_2a = expand(young_filter,c('',gpaste('*',uncertainty_values)),names = c('filter', 'interaction')) %>%
  rowwise() %>% mutate(command = 
  reg_command(dataset = paste0('base_data', filter),
              dep_var = 'log_dom_turnover',
              ind_var = 'log_comp_data',
              controls = paste0(interaction, '+ log_comp_rnd + log_age + comp_weighted_prestige'),
              fe = '|NACE_BR + year',
              cluster = 'firmid'))

if (running_regressions){write_rds(evaluate_variations(block_2a),"3) output/0_raw_output/2a_output_raw.rds")}else{

## output results 
block_2a_output = read_rds("3) output/0_raw_output/2a_output_raw.rds")
for (name in names(block_2a_output)) assign(name, block_2a_output[[name]])
label = '2a_total_rev'
table = format_table(model_inputs = model_output, label = label, header = age_header,
                     divisions_before = c(5,9), rescale_factor = 1,
                     output_path = paste0("3) output/", label,'.tex'),

             coef_order = c(1,6,7,8,5,2:4),       
             coef_names =  c('log comp data', 'log comp r\\&d', 'log age',
                             'avg worker\nprestige', 'NACE churn rate',
                             gpaste('\\hspace{5 pt} x NACE ', c('churn rate', 'var', 'avg firm var'))),
             notes = ' Robust standard errors clustered at the firm level. All regressions include industry and year FE.',
             note_width = 1.2
              )
}

# 2b) total export revenue x industry uncertainty------------------------------------------------

block_2b = expand(young_and_export_filter, c('',gpaste('*',uncertainty_values)),names = c('filter', 'interaction')) %>%
  rowwise() %>% mutate(command = 
  reg_command(dataset = paste0('base_data', filter),
              dep_var = 'log_total_export_rev_customs',
              ind_var = 'log_comp_data',
              controls = paste0(interaction, '+log_dom_turnover + log_comp_rnd + log_age + comp_weighted_prestige'),
              fe = '|NACE_BR + year',
              cluster = 'firmid'))

if(running_regressions){write_rds(evaluate_variations(block_2b),"3) output/0_raw_output/2b_output_raw.rds")}else{

## output results 
block_2b_output = read_rds("3) output/0_raw_output/2b_output_raw.rds")  
for (name in names(block_2b_output)) assign(name, block_2b_output[[name]])
for (i in 1:2){
if(i ==1){model_inputs = model_output[1:12]; label = '2b_export_rev_everyone'}
if(i ==2){model_inputs = model_output[13:24]; label = '2b_export_rev_exporters_only'}

table = format_table(model_inputs, label = label, header = age_header,
                     divisions_before = c(5,9), rescale_factor = 1,
                     output_path = paste0("3) output/", label,'.tex'),
                     
                     coef_order = c(1,7,8,9,6,2:5),       
                     coef_names =  c('log comp data', 'log dom. revenue' , 'log comp r\\&d', 'log age',
                                     'avg worker\nprestige', 'NACE churn rate',
                                     gpaste('\\hspace{5 pt} x NACE ', c('churn rate', 'var', 'avg firm var'))),
                     notes = ' Robust standard errors clustered at the firm level. All regressions include industry and year FE.',
                     note_width = 1.2
)
}
}
# 2c) total export revenue x avg foreign emp ------------------------------------------------
current_filter = if(dummy_version){young_and_export_filter[1:3]}else{young_and_export_filter[4:6]}

block_2c = expand(current_filter, c('',gpaste('*',overseas_employment_vars)),names = c('filter', 'interaction')) %>%
  rowwise() %>% mutate(command = 
  reg_command(dataset = paste0('base_data', filter),
              dep_var = 'log_total_export_rev_customs',
              ind_var = 'log_comp_data',
              controls = paste0(interaction, '+log_dom_turnover + log_comp_rnd + log_age + comp_weighted_prestige'),
              fe = '|NACE_BR + year',
              cluster = 'firmid'))

if (running_regressions){write_rds(evaluate_variations(block_2c),"3) output/0_raw_output/2c_output_raw.rds")}else{
block_2c_output = read_rds("3) output/0_raw_output/2c_output_raw.rds")  
for (name in names(block_2c_output)) assign(name, block_2c_output[[name]])
label = '2c_export_rev_x_foreign_employment'
table = format_table(model_output, label = label, header = age_header,
                     divisions_before = c(5,9), rescale_factor = 1,
                     output_path = 'hi.tex',  #output_path = paste0("3) output/", label,'.tex'),
                     coef_order = c(1, 6:11, 2:5),
                     spacer_size = .25,
                     custom_row_placement = 10,
                     custom_rows =   list(c('Comp. for employees:', rep('', 12))),
                     coef_names =   c('log comp data', 'log dom. revenue' , 'log comp r\\&d', 'log age',
                                      'avg worker\nprestige',
                                      gpaste('\\multicolumn{1}{r}{', c('currently in mkt','\\hspace{5 pt}with recent mkt exp.', 'with any mkt exp.'), "}",
                                      c('', 'x\n\\multicolumn{1}{r}{log comp data}')) %>% gsub('}x', " x}",.)) ,
                     notes = ' Robust standard errors clustered at the firm level. All regressions include industry and year FE.',
                     note_width = 1.2
)
}
# 2d) likelihood of entry into exporting ----------------------------------
block_2d =  expand(entry_filter, c('',gpaste('*',c('log_comp_abroad',uncertainty_values))), names = c('filter', 'interaction')) %>%
  rowwise() %>% mutate(command = 
  reg_command(dataset = paste0('base_data', filter),
              dep_var = 'is_first_export_year',
              ind_var = 'log_comp_data',
              time_var = 'age',
              controls = paste0(interaction, '+log_dom_turnover + log_comp_rnd + comp_weighted_prestige'),
              fe = 'NACE_BR, year',
              cluster = 'firmid',
              family = 'cox'))

if (running_regressions) write_rds(evaluate_variations(block_2d),"3) output/0_raw_output/2d_output_raw.rds")
block_2d_output = read_rds("3) output/0_raw_output/2d_output_raw.rds")  
for (name in names(block_2d_output)) assign(name, block_2d_output[[name]])

# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()




