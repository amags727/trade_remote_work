# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}


source('2) code/0_set_parameter_values.R')
# 1) make balance test  ---------------------------------------------------
base_vars =  c('turnover', 'capital', 'intangible_fixed_assets', 'empl', 'age', 'comp_weighted_prestige')
linkedin_vars =  gpaste(c("", 'share_'), 'comp_', c('data', 'stem', 'rnd', 'engineer'))
exporter_vars = c('currently_export','total_export_rev_customs', "num_export_countries",
                "avg_products_per_ctry", 'avg_streak_age','intermarket_hhi',
                gpaste('export_mkt_avg_comp_', c('now', 'l5', 'ever')))

headers = gpaste("&",gpaste('\\multicolumn{5}{c}{',
          c('All Firms', 'Young Firms (age $<$ 5)', 'Mature Firms (age $\\ge$ 5)'), "}",
          collapse_str = "& &"),'\\\\')

notes = c("Quartile Ranks are at the four digit NACE code $\\times$ year level. Standard Deviation in parentheses.")

base_data =  import_file(file.path(inputs_dir,'16c_firm_yr_lvl.parquet')) %>% 
  .[!is.na(age)] %>% 
  .[currently_export == F, (setdiff(exporter_vars, 'currently_export')) := NA] %>% 
  .[,is_subsid := as.logical(is_subsid)]

# generate the base summary tables  ---------------------------------------
if (running_regressions){
balance_table_1 = cbind(reshape_to_summary(base_data, base_vars, 'comp_data_nace_quartile'),
                     reshape_to_summary(base_data[young == T], base_vars, 'comp_data_nace_quartile_age')[-1],
                     reshape_to_summary(base_data[young == F], base_vars, 'comp_data_nace_quartile_age')[-1])

balance_table_2 = cbind(reshape_to_summary(base_data, linkedin_vars, 'comp_data_nace_quartile'),
                        reshape_to_summary(base_data[young == T],  linkedin_vars, 'comp_data_nace_quartile_age')[-1],
                        reshape_to_summary(base_data[young == F],  linkedin_vars, 'comp_data_nace_quartile_age')[-1])

balance_table_3 = cbind(reshape_to_summary(base_data, exporter_vars, 'comp_data_nace_quartile'),
                        reshape_to_summary(base_data[young == T],  exporter_vars, 'comp_data_nace_quartile_age')[-1],
                        reshape_to_summary(base_data[young == F],  exporter_vars, 'comp_data_nace_quartile_age')[-1])
write_rds(list(balance_table_1 = balance_table_1, balance_table_2 = balance_table_2, balance_table_3 = balance_table_3),
          paste0(raw_output_dir,'block_1.rds'))
}

# output the results  -----------------------------------------------------
if(!running_regressions){
#### BALANCE SHEET VARS 
label = '1a_balance_test_balance_sheet_vars'
format_table(
  summary_table_input = import_file('3) output/0_raw_output/7a_balance_table_1.rds'),
  label =  label, rescale_factor = 1,  headers = headers, spacer_size = 1, notes = notes, note_width =1.4, 
  divisions_before = c(6,11), output_path = paste0('3) output/',label,'.tex'),
        
  coef_names =c('total rev', 'capital', 'intangible\nassets', 'total\nemployment', 'firm age','worker\nprestige'),
  caption = 'Firm Balance Sheet Characteristics by Age and Data Use Quartile'
 )

#### LINKEDIN EXPENDITURE VARS 
label = '1b_balance_test_linkedin_vars'
format_table(
  summary_table_input = import_file('3) output/0_raw_output/7b_balance_table_2.rds')[1:8, -c(6,11,16)],
  label =  label, rescale_factor = 1,
  headers = gsub("\\{5\\}", "{4}",headers),
  spacer_size = 1, notes = notes, note_width =1.4, 
  divisions_before = c(5,9), output_path = paste0('3) output/',label,'.tex'),
  
  coef_names = gpaste('\\hspace{5 pt}', c('Data', 'STEM', 'R\\&D', 'Engineering')),
  custom_rows =  list(c('Total Compensation', rep('', 12))),
  custom_row_placement = 8,
  caption = 'Firm Employment Characteristics by Age and Data Use Quartile',
  final_commands = "table[9] =gsub('\\\\.\\\\d', '', table[9])")
 
#### EXPORT VARIABLES 
label = '1c_balance_test_exporter_vars'
format_table(
  summary_table_input = import_file('3) output/0_raw_output/7c_balance_table_3.rds'),
  label =  label, rescale_factor = 1,  headers = headers, spacer_size = .5, notes = notes, note_width =1.4, 
  divisions_before = c(6,11), output_path = paste0('3) output/',label,'.tex'),
  custom_row_placement = 20,
  custom_rows =   list(c('\\textbf{Comp. for employees:}', rep('', 12))),
  coef_names = c('currently export', 'total export rev', 'num. export\nmarkets',
               'avg products\nper market', 'avg export market\nstreak age', 'intermarket HHI',
               gpaste('\\multicolumn{1}{r}{', c('currently in mkt','\\hspace{5 pt}with recent mkt exp.', 'with any mkt exp.'), "}")),
  caption = 'Firm Export Characteristics by Age and Data Use Quartile')
}

# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()




