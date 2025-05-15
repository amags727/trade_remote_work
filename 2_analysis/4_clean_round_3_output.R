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

## import helper functions 
source('2) code/0_set_parameter_values.R')

# add some keys for variable comparison 
variable_comp_header = "&\\multicolumn{3}{c}{Log Comp Data}& &\\multicolumn{3}{c}{Share Comp Data}\\\\"
var_comp_dir = paste0(finished_output_dir, '2_var_comp_analysis/'); dir.create(var_comp_dir)
base_env = c(base_env, 'variable_comp_header', 'var_comp_dir')

# 1 balance tests  --------------------------------------------------------
block_num = 1
base = import_file(paste0(raw_output_dir,'block_',block_num,'.rds'))
for(name in names(base)){assign(name, base[[name]])}
headers = gpaste("&",gpaste('\\multicolumn{5}{c}{',c('All Firms', 'Young Firms (age $<$ 5)', 'Mature Firms (age $\\ge$ 5)'), "}",
                            collapse_str = "& &"),'\\\\')

notes = paste0("Quartile Ranks are at the four digit NACE code $\\times$ year level.",
               "For age specific restuls, quartile ranking is within age bracket. ",
               "Standard Deviation in parentheses.")

## 1a balance sheet vars 
label = '1a_balance_test_balance_sheet_vars'
format_table(
  summary_table_input = balance_table_1,
  label =  label, rescale_factor = 1,
  headers = gsub('\\{4\\}', '{5}',age_header),
  spacer_size = 1,
  notes = notes, note_width =1.4, 
  divisions_before = c(6,11), 
  output_path = paste0(finished_output_dir,label,'.tex'),
  coef_names =c('total rev', 'capital', 'intangible\nassets', 'total\nemployment', 'firm age','worker\nprestige'),
  caption = 'Firm Balance Sheet Characteristics by Age and Data Use Quartile', 
  make_tex = F)

## other compensation types 
label = '1b_balance_test_linkedin_vars'
format_table(
  summary_table_input = balance_table_2[1:8, -c(6,11,16)],
  label =  label,
  rescale_factor = 1,
  headers = age_header,
  spacer_size = 1, notes = notes, note_width =1.4, 
  divisions_before = c(5,9), 
  output_path = paste0(finished_output_dir,label,'.tex'),
  coef_names = gpaste('\\hspace{5 pt}', c('Data', 'STEM', 'R\\&D', 'Engineering')),
  custom_rows =  list(c('Total Compensation', rep('', 12))),
  custom_row_placement = 8,
  caption = 'Firm Employment Characteristics by Age and Data Use Quartile',
  final_commands = "table[9] =gsub('\\\\.\\\\d', '', table[9])", 
  make_tex = F)

## Export behavior 
label = '1c_balance_test_exporter_vars'
format_table(
  summary_table_input = balance_table_3,
  label =  label, rescale_factor = 1,  headers = age_header, spacer_size = .5, notes = notes, note_width =1.4, 
  divisions_before = c(6,11), output_path = paste0(finished_output_dir,label,'.tex'),
  custom_row_placement = 20,
  custom_rows =   list(c('\\textbf{Comp. for employees:}', rep('', 12))),
  coef_names = c('currently export', 'total export rev', 'num. export\nmarkets',
                 'avg products\nper market', 'avg export market\nstreak age', 'intermarket HHI',
                 gpaste('\\multicolumn{1}{r}{', c('currently in mkt','\\hspace{5 pt}with recent mkt exp.', 'with any mkt exp.'), "}")),
  caption = 'Firm Export Characteristics by Age and Data Use Quartile', 
  make_tex = F)

rm(list= setdiff(ls(), base_env)); gc()
# 2a firm_yr variable comp  -------------------------------------------------------------------------
base = import_file(paste0(raw_output_dir,'block_2a_firm_yr.rds'))
for(name in names(base)){assign(name, base[[name]])}

sub_blocks = unique(variation_output$block)
sub_block_indeces = lapply(sub_blocks, function(i) which(variation_output$block == i) )
base_coef_block = c('value', 'log comp r\\&d', 'log age', 'avg worker\nprestige',
                    append(rep(c('pct rank', 'sd from mean'),2),'value', after = 2))
                 
sub_block_coef = list(base_coef_block,
                 append(base_coef_block, 'log dom. revenue', after = 1),
                 append(base_coef_block, 'log dom. revenue', after = 1),
                 append(base_coef_block, 'log dom. revenue', after = 1)[-4]) # remove log age
sub_block_coef_order = lapply(sub_block_coef, function(i){end = length(unique(i)); c(1,(end-1):end, 2:(end-2))})
for (i in 1:length(sub_blocks)){
format_table(model_inputs = model_output[sub_block_indeces[[i]]],
             label = sub_blocks[i],
             coef_names = sub_block_coef[[i]],
             coef_order = sub_block_coef_order[[i]],
             headers = variable_comp_header,
             divisions_before = 4,
             notes = ' Robust standard errors clustered at the firm level. All regressions include industry and year FE.',
             output_path = paste0(var_comp_dir, sub_blocks[i], '.tex'),
             cox = ifelse(i == 4, T,F), 
             make_tex = F)
}

rm(list= setdiff(ls(), base_env)); gc()
# 2b firm ctry year variable comp ---------------------------------------------------------------------
base = import_file(paste0(raw_output_dir,'block_2b_firm_ctry_yr.rds'))
for(name in names(base)){assign(name, base[[name]])}

sub_blocks = unique(variation_output$block)
sub_block_indeces = lapply(sub_blocks, function(i) which(variation_output$block == i) )
coef_block = c('value','log age', 'log dom turnover', 'log comp r\\&d', 'avg worker\nprestige',
               append(rep(c('pct rank', 'sd from mean'),2),'value', after = 2))
end = length(unique(coef_block)); coef_order = c(1,(end-1):end, 2:(end-2))
for (i in 1:length(sub_blocks)){
  format_table(model_inputs = model_output[sub_block_indeces[[i]]],
               label = sub_blocks[i],
               coef_names = coef_block,
               coef_order = coef_order,
               headers = variable_comp_header,
               divisions_before = 4,
               notes = ' Robust standard errors clustered at the firm level. All regressions include industry, country, and year FE.',
               output_path = paste0(var_comp_dir, sub_blocks[i], '.tex'),
               cox = ifelse(i > 4, T,F), 
               make_tex = F)
}
# 2c firm variance results  -----------------------------------------------
base = import_file(paste0(raw_output_dir,'block_2c_firm_variance.rds'))
for(name in names(base)){assign(name, base[[name]])}
sub_blocks = unique(variation_output$block)
sub_block_indeces = lapply(sub_blocks, function(i) which(variation_output$block == i) )
base_coef_block = c('value','log dom. revenue',  'log comp r\\&d', 'avg worker\nprestige',
                    'log age', 'log years observed', 
                    append(rep(c('pct rank', 'sd from mean'),2),'value', after = 2))
sub_block_coef = list(base_coef_block,
                      append(base_coef_block,'log export revenue', after = 2), 
                      append(base_coef_block,'log export revenue', after = 2))

sub_block_coef_order = lapply(sub_block_coef, function(i){end = length(unique(i)); c(1,(end-1):end, 2:(end-2))})
for (i in 1:length(sub_blocks)){
  format_table(model_inputs = model_output[sub_block_indeces[[i]]],
               label = sub_blocks[i],
               coef_names = sub_block_coef[[i]],
               coef_order = sub_block_coef_order[[i]],
               headers = variable_comp_header,
               divisions_before = 4,
               notes = ' Robust standard errors clustered at the firm level. All regressions include industry and initial-year FE.',
               output_path = paste0(var_comp_dir, sub_blocks[i], '.tex'), 
               make_tex = F)
}



# 2d firm ctry variance variable comp  ------------------------------------
base = import_file(paste0(raw_output_dir,'block_2d_firm_ctry_variance.rds'))
for(name in names(base)){assign(name, base[[name]])}
sub_blocks = unique(variation_output$block)
sub_block_indeces = lapply(sub_blocks, function(i) which(variation_output$block == i) )
coef_block = c('value','log export rev customs', 'log dom. revenue',  'log age',  'log comp r\\&d', 'avg worker\nprestige', 'log streak duration',
               append(rep(c('pct rank', 'sd from mean'),2),'value', after = 2))

end = length(unique(coef_block)); coef_order = c(1,(end-1):end, 2:(end-2))
for (i in 1:length(sub_blocks)){
  format_table(model_inputs = model_output[sub_block_indeces[[i]]],
               label = sub_blocks[i],
               coef_names = coef_block,
               coef_order = coef_order,
               headers = variable_comp_header,
               divisions_before = 4,
               notes = ' Robust standard errors clustered at the firm level. All regressions include industry, country, and streak start year FE.',
               output_path = paste0(var_comp_dir, sub_blocks[i], '.tex'),
               cox = ifelse(i > 4, T,F), 
               make_tex = F)
}





