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

firm_yr_ctry = import_file(firm_yr_ctry_path)

firm_yr_ctry[, `:=`(grav_dist=asinh(grav_dist),
                    either_grav_dist=asinh(either_grav_dist))]


# 3a export rev  --------------------------------------------------------

variations = expand(
  c(F,T), # extensive_margin
  c('', '[currently_export_customs_any_ctry == T]', '[currently_export_customs == T]'), # exporter restriction 
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

gravity_vars<-c("grav_region", "grav_border", "grav_language", "grav_dist", 
                "either_grav_region", "either_grav_border", "either_grav_language", "either_grav_dist", 
                "ctry_pop", "nace_ctry_pop")

gravity_vars<-c("grav_dist","either_grav_dist")


for(grav_var in gravity_vars){
  variations = expand(
    c(F,T), # extensive_margin
    c(''), # exporter restriction 
    c(F,T), # interaction
    names =c('extensive_margin', 'export_restriction','interaction')) %>% rowwise() %>% mutate(
      ind_var = gpaste('log_comp_data', c('', '_lag1'), ifelse(interaction,paste0('*', grav_var), '' ), collapse_str = "+"),
      ind_var = ifelse(extensive_margin, gsub('log_comp_data', 'use_data', ind_var), ind_var),
      command = reg_command(dataset = paste0('firm_yr_ctry', export_restriction),
                            dep_var = 'log_export_rev_customs',
                            ind_var = ind_var,
                            controls = base_controls, 
                            fe = "| firmid_num + year + ctry_num",
                            cluster = 'firmid_num'
      ))
  model_output =evaluate_variations(variations)[['model_output']]
  write_rds(raw_output_dir, paste0('3c_', grav_var, '_gravity.RDS'))
  
  ## output results 
  if(dummy_version) model_output = rep(model_output[1:2],6)
  coef_names = c(gpaste(c('', 'lagged '),'log payroll ', c('data', 'total'), order = 3:1),
                 gpaste('log dom. revenue', c('', ' sq')), 'empl. prestige', 'share empl. college grad', 'log firm age', 
                 gpaste(c('\\hspace{5 pt}x ', '\\hspace{5 pt}lx '), gsub("_", " ", gsub("grav", "gravity", gsub("dist", "distance", gsub("ctry", "country", gsub("pop", "population", grav_var)))))),
                 gpaste(c('', 'lagged '), 'use data'),
                 gpaste(c('\\hspace{5 pt}mx ', '\\hspace{5 pt}nx '), gsub("_", " ", gsub("grav", "gravity", gsub("dist", "distance", gsub("ctry", "country", gsub("pop", "population", grav_var)))))))
  coef_order = c(1, 10, 2, 11,  12, 14, 13, 15, 3:9)
  
  if(grav_var!="grav_region"){
    coef_names<-append(coef_names, gsub("_", " ", gsub("grav", "gravity", grav_var)), after=9)
    coef_order = c(1, 11, 2, 12,  13, 15, 14, 16, 3:9)
  }
  
  label = paste0('3c_', grav_var, '_gravity')
  temp_model_output = model_output;
  temp_coef_names = coef_names
  
  
  format_table(temp_model_output, label = label,
               coef_names = temp_coef_names,
               coef_order = coef_order,
               headers = make_headers(2,  c(rep('Export Rev. (Customs)',2))),
               divisions_before = c(3),
               rescale_factor = 1,
               custom_rows = list(""),
               custom_row_placement = 17,
               final_commands = paste0("table = gsub('lx|mx|nx', 'x',table);"),
               notes = "Robust Standard Errors clustered at the firm level. All Regressions include firm, year, and country FE.",
               note_width = .8,
               dummy_version = dummy_version,
               output_path =  paste0(finished_output_dir, label, '.tex'), make_tex = F )
  
}


 






