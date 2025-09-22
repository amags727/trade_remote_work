# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}; gc();
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




# 1 Generate graph of fit ---------------------------------------------------
library(ggpmisc)
data = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet") %>%
  .[first_or_no_forecast == T]
fit_graph = ggplot(
  data, aes(x = asinh(parent_empl_total), y = log_compustat_emp)) +
  geom_point(alpha = 0.1) + stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label.., sep = "~~~")),parse = TRUE) +
  theme_minimal() + labs(title = "Revelio x Compustat Employee Match", x = "log revelio employees", y = "log compustat employees")

ggsave(paste0(us_output_path, '1_employee_fit_graph.png'), width = 6.5, height = 4.25)

# 2 Balance Tests -------------------------------------------------------------------------
finance_vars = c('parent_comp_total', gpaste('share_parent_comp_', c('data','rnd', 'stem')),
                 'parent_share_empl_college', gpaste('compustat_', c('rev', 'capital', 'inventory_to_rev_ratio')),
                 'revel_age', 'abs_firm_forecast_error', 'pct_abs_firm_forecast_error')

var_names = c('payroll total', gpaste('share payroll ', c( 'data', 'rnd', 'stem')), 'pct college degree', 
              'revenue', 'capital', 'inventory to rev. ratio', 'firm age', 'abs. forecast error', 'pct forecast error')
  
financial_data = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet") %>%
  .[first_or_no_forecast == T & !is.na(naics_2d) & !is.na(comp_data)] %>%
  .[parent_comp_data == 0, `:=`(comp_data_quartile = 0, share_comp_data_quartile = 0)] %>% 
  .[parent_comp_data !=0, `:=`(comp_data_quartile = ntile(parent_comp_data, 4),
                               share_comp_data_quartile = ntile(share_parent_comp_data, 4)), 
    by = .(naics_2d, fiscal_year)] %>% .[!is.na(share_comp_data_quartile)] %>% 
  mutate(across(con_fil(finance_vars, 'share', 'age','pct', 'ratio', inc =F), ~.*1e-6))

balance_table = reshape_to_summary(financial_data,finance_vars,'share_comp_data_quartile')


label = '2_balance_tests'
format_table(summary_table_input = balance_table,
             label = label,
             headers = "&&& \\multicolumn{4}{c}{Data Payroll Share Quartile} \\\\",
             coef_names = var_names, 
             divisions_before = 2,
             custom_rows = list('\\textbf{Linkedin Data}','\\textbf{Balance Sheet Data}', '\\textbf{Guidance Data}'),
             custom_row_placement = c(8, 19,28),
             spacer_size = 1,
             notes = 'Quartiles at the  year-level',
             note_width = .8,
             rescale_factor =  1,
             output_path = paste0(us_output_path, label, '.tex'),make_tex = F)

# 3 event study version of data adoption -----------------------------------------------------------------------
combined_dta = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet") %>% 
  .[usfirm == 1 & first_forecast ==T]

base_controls = paste(c("", 'log_parent_comp_total', 'log_forecast_horizon','parent_share_empl_college',
                      'parent_avg_prestige', 'log_revel_age', 'abs_analyst_log_forecast_error'),  collapse = " + ")

additional_controls = c("","log_compustat_rev", 'log_comp_stem + log_comp_non_data_rnd') %>% c(., paste(.,collapse = "+"))
base_command = reg_command('combined_dta[in_event_study_data == T]', 
                           'abs_firm_log_forecast_error', 'sunab(extensive_hire_cohort_data,event_study_yr_3y_trim_data)', base_controls,
                           fe = "| ibes_ticker + fiscal_year", cluster = 'ibes_ticker') 

iplot(eval(parse(text= base_command)))


# 4 generate graphs for forecasting ability -----------------------------------------------------------------------
obtain_residuals = function(dta, model, var_name){
  non_dropped_obs = setdiff(1:nrow(dta),-1*model$obs_selection$obsRemoved)
  dta = dta[non_dropped_obs, (var_name) := model$residuals] 
}
obtain_quartiles = function(dta, var_name, yr_to_yr = F){
  if(yr_to_yr){
    dta[!is.na(var_name),paste0(var_name, "_quartile_yr_to_yr") := ntile(get(var_name),4), by = fiscal_year]
  }else{
    dta[!is.na(var_name),paste0(var_name, "_quartile") := ntile(get(var_name),4)]
  }
}
graph_quartiles = function(dta, dep_var, ind_var){
  dta[!is.na(get(ind_var)), .(dep_var= NA_mean(get(dep_var))), by = c(ind_var, 'fiscal_year')] %>% 
    ggplot(., aes(x = fiscal_year, y = dep_var, color = factor(.[[ind_var]]))) + geom_line() + theme_minimal() + 
    labs(color = 'quartile',  x= element_blank(), y = dep_var) + theme(legend.position = 'bottom')
}

dta = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet") %>%
  .[!is.na(abs_firm_forecast_error) & usfirm == 1]
model_dta = dta[first_forecast == T]

model1 = feols(model_dta, log_parent_comp_data~ log_parent_comp_total + parent_share_empl_college + parent_avg_prestige + log_revel_age + log_parent_comp_stem + asinh(realized_value) | fiscal_year)
for( i in 1:1) model_dta =  obtain_residuals(model_dta, get(paste0('model',i)), paste0('resid_data', i)) 
for( i in 1:1) model_dta = obtain_quartiles(model_dta,paste0('resid_data',i))
model_dta = model_dta[,share_comp_data_quartile := ntile(share_parent_comp_data,4), by = .(naics_2d, fiscal_year)] %>% 
  select('ibes_ticker', 'fiscal_year', con_fil(.,'resid' ,'quartile'))
dta = merge(dta, model_dta, by =c('ibes_ticker', 'fiscal_year')) 


residual_graph = graph_quartiles(dta[first_forecast == T & !forecast_out_of_range], 'log_abs_firm_forecast_error', 'resid_data1_quartile') + 
  labs(color = 'residual data \nspend quartile',
       y = 'log abs. forecast error', 
       title = 'Impact of Data on Forecast Errors') 

ggsave(paste0(us_output_path, '4_residual_graph.png'),residual_graph, width = 6, height = 4.25)


# forecast error regression results  --------------------------------------------
combined_dta = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet") %>% 
  .[usfirm == 1 & first_forecast == T] %>%
  .[,initial_over_opt := log_fy_init_analyst_forecast > asinh(realized_value)] %>% 
  .[, leave_out_industry_mean_forecast_error_pct := 
      ifelse(.N >2, (NA_sum(abs_firm_log_forecast_error) - abs_firm_log_forecast_error)/(.N-1), NA), by = .(industry_group, fiscal_year)]  
  
parent_controls <- paste("", "log_parent_comp_total", "parent_share_empl_college", "parent_avg_prestige",
                          "log_forecast_horizon", "log_revel_age", "log_abs_analyst_forecast_error", sep = " + ")
additional_controls = c('', 'log_fy_init_analyst_forecast')
base_prt_command = reg_command('combined_dta','log_abs_firm_forecast_error', 'log_parent_comp_data +',parent_controls,
                               fe ="| industry_group + fiscal_year", cluster = 'ibes_ticker')

variations = expand(
  c('abs', 'pct'), c('industry', 'firm'), additional_controls,
  names = c( 'dep_var', 'fe', 'add_control')) %>%
  .[,command := base_prt_command] %>% 
  .[dep_var == 'pct', command := gsub("log_abs_(firm|analyst)", "abs_\\1_log", command)] %>%
  .[fe == 'firm', command := gsub('\\| industry_group', '| ibes_ticker',command)] %>% 
  .[add_control != '',command := mapply(function(c,command) gsub('\\|', paste0("+",c, " |"), command), add_control, command)]
  
model_output = evaluate_variations(variations)$model_output   

### RUN INITIAL REGRESSIONS 
model_output = evaluate_variations(variations)$model_output   
table1_regs = variations[control_for_analyst == T & dep_var =='log_abs_firm_forecast_error'][['index']]
table2_regs =  variations[control_for_analyst == F & dep_var =='log_abs_firm_forecast_error'][['index']]
table5_regs = variations[ add_control == "" ][['index']]

for (i in 1:4){
  cust_row_place = ifelse(i %in% c(1,3), 29, 27)
  label = paste0('9a',i, "_forecast_error")
  regs = get(paste0('table',i, '_regs'))
  format_table(model_output[regs],label = label, caption = 'Impact of Data Spending on Next Period Forecast Errors',
               custom_rows = list(c('Industry / Firm Fe',c(rep('industry',4), rep('firm', 3)))),
               custom_row_placement = cust_row_place, make_tex = F, make_pdf = T,
               divisions_before = 5,
               output_path = paste0(de_dummy(finished_output_dir), label, '.pdf'),
               rescale_factor = 1
  )
}

for (i in 5:6){
  label = paste0('9a',i, "_forecast_error")
  regs = get(paste0('table',i, '_regs'))
  format_table(model_output[regs],label = label, caption = 'Impact of Data Spending on Next Period Forecast Errors',
               headers = make_headers(4,c('log abs. fcst error', 'log sq. fcst error', 'abs log fcst error')),
               custom_rows = list(c('Industry / Firm Fe', rep(rep(c('industry', 'firm'), each = 2),3))),
               custom_row_placement = 27, make_tex = F, make_pdf = T,
               divisions_before = c(5,9),
               output_path = paste0(de_dummy(finished_output_dir), label, '.pdf'),
               rescale_factor = 1)
}

### RUN SPLINE REGRESSIONS 
forecast_error_spline_wrapper = function(dta,ind_var, running_var, fe){
bottom_top = quantile(dta[[running_var]], c(.01,.99), na.rm = T)
dta = dta[get(running_var) > bottom_top[1] & get(running_var) < bottom_top[2]]
command =  gsub('comp_data',paste0("comp_data*bs(",running_var,", df = 5)"), base_prt_command) %>%
           gsub('combined_dta', 'dta',.)
if(ind_var == 'pct')command = gsub("log_abs_(firm|analyst)", "abs_\\1_log", command)

if (fe == 'firm') command = gsub('\\| industry_group', '| ibes_ticker',command)
spline_reg = eval(parse(text = command))
output_graph = spline_analysis(dta, spline_reg, 'log_parent_comp_data', running_var)
}
h=forecast_error_spline_wrapper(combined_dta,'pct', 'leave_out_industry_mean_forecast_error_pct', 'firm' )


stem_spline_graph = forecast_error_spline_wrapper(combined_dta, 'log_parent_comp_stem', 'industry')
stem_spline_graph_firm_fe = forecast_error_spline_wrapper(combined_dta, 'log_parent_comp_stem', 'firm')

rnd_spline_graph = forecast_error_spline_wrapper(combined_dta, 'log_compustat_rnd', 'industry')
rnd_spline_graph_firm_fe = forecast_error_spline_wrapper(combined_dta, 'log_compustat_rnd', 'firm')


# impact on revenue  -------------------------
model_dta = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet") %>% .[first_or_no_forecast == T & !is.na(gvkey)] %>% 
  .[,obs_count := .N, by = gvkey] %>% .[,revel_age_sq := revel_age^2] %>% 
  .[obs_count > 5, detrended_var := abs(sub_regression(log_compustat_rev, log_revel_age, residuals = T)), by = gvkey]

parent_controls = paste('log_parent_comp_total', 'parent_share_empl_college', 'parent_avg_prestige',
                         'log_revel_age', 'log_compustat_capital', sep = " + ")  
base_command = reg_command('model_dta','log_compustat_rev', 'log_parent_comp_data +',parent_controls,
                           fe ="| naics + fiscal_year", cluster = 'gvkey')
additional_controls = c("",paste0("+", c('log_comp_stem', 'log_compustat_rnd')%>% c(., paste(.,collapse = "+"))))

variations = expand(c('industry', 'firm'), additional_controls, names = c('fe', 'add_control')) %>% 
 .[, command := mapply(function(c) gsub('\\| naics',paste0(c, "| naics") ,base_command), add_control)] %>% 
 .[fe == 'firm', command :=gsub('naics', 'gvkey', command) ]

model_output = evaluate_variations(variations)[['model_output']]


rev_spline_wrapper = function(dta,running_var, fe){
  bottom_top = quantile(dta[[running_var]], c(.01,.99), na.rm = T)
  dta = dta[get(running_var) > bottom_top[1] & get(running_var) < bottom_top[2]]
  
  command =  gsub('comp_data',paste0("comp_data*bs(",running_var,", df = 5)"), base_command) %>%
    gsub('model_dta', 'dta',.)
  
  if (fe == 'firm') command = gsub('\\| naics', '| gvkey',command)
  spline_reg = eval(parse(text = command))
  output_graph = spline_analysis(dta, spline_reg, 'log_parent_comp_data', running_var)
}

age_spline_industry = rev_spline_wrapper(model_dta, 'log_revel_age', 'industry') + scale_x_continuous(labels = function(x) round(exp(x), 0)) + labs(x = 'firm age')
age_spline_firm = rev_spline_wrapper(model_dta, 'log_revel_age', 'firm') +   scale_x_continuous(labels = function(x) round(exp(x), 0)) + labs(x = 'firm age')









