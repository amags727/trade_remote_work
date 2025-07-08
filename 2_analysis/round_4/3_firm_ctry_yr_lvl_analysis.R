# setup -------------------------------------------------------------------
rm(list = ls()); gc();

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data",
    
    ## update as makes sense for CASD / your own use 
    T ~ "C:/Users/Public/Documents/Big data Project")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')



# set parameter values  ---------------------------------------------------
base_controls =  paste("", 'log_comp_total', 'log_comp_total_lag1', 'log_dom_turnover', 'log_dom_turnover_sq',
                       'avg_prestige_total', 'share_empl_college', 'log_age', sep = " + ")

coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age', 
               gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'share industry exporting'))


# 3a export rev  --------------------------------------------------------
firm_yr_ctry = import_file(firm_yr_ctry_path)


variations = expand(
  c(F,T), # extensive_margin
  c('','[currently_export_customs_any_ctry == T]', '[currently_export_customs == T]'), # exporter restriction 
  c(F,T), # interaction
  names =c('extensive_margin', 'export_restriction','interaction')) %>% rowwise() %>% mutate(
    ind_var = gpaste('log_comp_data', c('', '_lag1'), ifelse(interaction,'*nace_share_export_customs_any_ctry', '' ), collapse_str = "+"),
    ind_var = ifelse(extensive_margin, gsub('log_comp_data', 'use_data', ind_var), ind_var),
    command = reg_command(dataset = paste0('firm_yr_ctry', export_restriction),
                          dep_var = 'log_export_rev_customs',
                          ind_var = ind_var,
                          controls = base_controls, 
                          fe = "| firmid_num + year + ctry_num",
                          cluster = 'firmid_num'
    ))
model_output =evaluate_variations(variations)[['model_output']]
write_rds(raw_output_dir, '3a_ctry_lvl_export_rev.RDS')

## output results 
if(dummy_version) model_output = rep(model_output[1:2],6)
coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
               gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age', 
               gpaste(c("", '\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), 'share industry exporting'))
for (i in 1:2){
  if(i == 1){
    label = "3a.1_ctry_lvl_export_rev";
    temp_model_output = model_output[1:6];
    temp_coef_names = coef_names
  }
  if(i == 2){
    label = "3a.2_ctry_lvl_export_rev_extensive_margin"; 
    temp_model_output = model_output[7:12]
    temp_coef_names = gsub('log payroll data', 'use data',coef_names)
  }
  format_table(temp_model_output, label = label,
               coef_names = temp_coef_names,
               coef_order = c(1, 11,2,12, 3:10),
               headers = make_headers(2,  c('Unconditional', rep('Exporting Firms',2))),
               divisions_before = c(3,5),
               rescale_factor = 1,
               custom_rows = list(""),
               custom_row_placement = 16,
               final_commands = paste0("table = gsub('lx', 'x',table);",
                                       "table = append(table, make_headers(2, c('', '(overall)', '(to country)')), after = 7)"),
               notes = "Robust Standard Errors clustered at the firm level. All Regressio ns include firm, year, and country FE.",
               note_width = .8,
               dummy_version = dummy_version,
               output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
} 

# 3b currently exporting/ streak death / revenue variance  -------------------------------------------------
variations = expand(
  c(F,T), # extensive_margin
  c('currently_export_customs', 'is_streak_death', 'log_export_rev_customs_cond_detrended_var'), # dep_var 
  c('', '[currently_export_customs_any_ctry == T]'), # exporter restriction 
  c(F,T), # interaction
  names =c('extensive_margin','dep_var', 'export_restriction','interaction')) %>% 
  filter(export_restriction == '' | dep_var == 'currently_export_customs') %>% rowwise() %>%
  mutate(
    ind_var = gpaste('log_comp_data', c('', '_lag1'), ifelse(interaction,'*nace_share_export_customs_any_ctry', '' ), collapse_str = "+"),
    ind_var = ifelse(extensive_margin, gsub('log_comp_data', 'use_data', ind_var), ind_var),
    controls = ifelse(grepl('currently', dep_var), base_controls, paste0(base_controls, "+ log_years_since_streak_start")),
    command = reg_command(dataset = paste0('firm_yr_ctry', export_restriction),
                          dep_var = 'currently_export_customs',
                          ind_var = ind_var,
                          controls = controls, 
                          fe = "| firmid_num + year + ctry_num",
                          cluster = 'firmid_num',
                          family = ifelse(grepl('var',dep_var), 'feols', 'binomial')
    ))

model_output =evaluate_variations(variations)[['model_output']]
write_rds(raw_output_dir, '3b_ctry_lvl_supplementary_regs.RDS')

## output results 
if(dummy_version){model_output[c(3:4,11:12)] = model_output[c(1:2, 9:10)]}
for (i in 1:2){
  if (i == 1){
label = "3b.1_ctry_lvl_supp_regressions"; 
temp_model_output = model_output[1:8]
temp_coef_names = c(coef_names,'log year of\nexport streak')}
  if (i == 2){
    label = "3b.2_ctry_lvl_supp_regressions_extensive_margin"; 
    temp_model_output = model_output[9:16]
    temp_coef_names = c(coef_names,'log year of\nexport streak')
  }

header_1 =  make_headers(2,  c('P(currently export)','P(end export streak)', 'detrended export var.' )) %>% sub("2", "4", .)
header_2 = make_headers(2, c('all firms', 'exporters')) %>% gsub('&&', '&',.)
format_table(temp_model_output, label = label, caption = 'Country Level Results: Impact of Data Use on Export Performance',
             coef_names = temp_coef_names,
             coef_order = c(1,11,2,12,10,3:9,13),
             headers = paste0(header_1, header_2),
             divisions_before = c(5,7),
             rescale_factor = 1,
             custom_rows = list(""),
             custom_row_placement = 18,
             final_commands = "table = gsub('lx', 'x',table); table = table[-9]",
             notes = "Robust standard errors clustered at the firm level. All Regressions include firm, year, and country FE.",
             note_width = 1,
             dummy_version = dummy_version,
             output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
}

# 3c  gravity ------------------------------------------
## run regressions 
grav_types<-c("extended_", "", "either_")

for(grav_type in grav_types){
  
  dep_var<-'log_total_export_rev_customs'
  base_command = reg_command('firm_yr_ctry',
                             dep_var = dep_var,
                             ind_var = paste('log_comp_data', 'log_comp_data_lag1', 'log_comp_total', 'log_comp_total_lag1',
                                             'log_dom_turnover', 'log_dom_turnover_sq', 'avg_prestige_total', 'share_empl_college',
                                             'log_age', sep = " + "),
                             fe = "| firmid_num + year", 
                             cluster = 'firmid_num') 
  interactions = gpaste(grav_type, 'grav_',c('region', 'language', 'border'))
  variations = data.frame(command =  c(base_command, 
                                       unlist(lapply(interactions, function(inter) gsub("customs~", paste0('customs~log_comp_data*', inter, ' +log_comp_data_lag1*', inter, ' + '),base_command)))))
  
  model_output = evaluate_variations(variations)[['model_output']]
  
  ## output results 
  coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
                 gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age',
                 c(gpaste(c('', '\\hspace{5 pt}x ',  '\\hspace{5 pt}lx '), gsub("_", "", grav_type), ' gravity ', c("region"))),
                 c(gpaste(c('', '\\hspace{5 pt}x ',  '\\hspace{5 pt}lx '), gsub("_", "", grav_type), ' gravity ', c("language"))),
                 c(gpaste(c('', '\\hspace{5 pt}x ',  '\\hspace{5 pt}lx '), gsub("_", "", grav_type), ' gravity ', c("border"))))
  
  label = paste0("3c_", grav_type, "gravity")
  format_table(model_output, label = label,
               coef_names = coef_names,
               coef_order = c(1,11,14,17,2,12,15,18,10,13,16,3:9),
               headers = "&\\multicolumn{4}{c}{Total Export Rev. (Customs)}\\\\",
               # divisions_before = 4,
               rescale_factor = 0.8,
               custom_rows = list(""),
               custom_row_placement = 18,
               final_commands = "table = gsub('lx', 'x',table)",
               notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm and year FE.",
               note_width = 1,
               output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
}







