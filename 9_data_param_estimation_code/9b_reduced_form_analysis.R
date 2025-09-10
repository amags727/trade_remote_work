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


wrds <- dbConnect(RPostgres::Postgres(), host = "wrds-pgdata.wharton.upenn.edu",port = 9737,
                  dbname = "wrds", user = "am0195", password = "BodyBody123!", sslmode = "require")
wrds_query= function(query_string){dbGetQuery(wrds, query_string) %>% data.table()}

# generate graphs for forecasting ability -----------------------------------------------------------------------
quartile_graph = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet") %>%
  .[!is.na(data_spend_quartile_IBES_sample), .(log_abs_firm_forecast_error= NA_mean(log_abs_firm_forecast_error),
                                               log_abs_analyst_forecast_error = NA_mean(log_abs_analyst_forecast_error)), by = .(data_spend_quartile_IBES_sample, fiscal_year)] %>% 
  ggplot(., aes(x= fiscal_year, y = log_abs_firm_forecast_error, color = as.factor(data_spend_quartile_IBES_sample))) + geom_line() +
  labs(color = 'data spend \nquartile', y = 'log abs. forecast error', x = '')

ggsave(paste0(de_dummy(finished_output_dir), '9c_forecast_error_time_trend.png'),quartile_graph,  width = 5, height = 5)


# generate regression results  --------------------------------------------
combined_dta = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet")
parent_controls = paste("",'log_parent_comp_total', 'log_forecast_horizon','parent_share_empl_college',
                        'parent_avg_prestige','log_abs_analyst_forecast_error', 'log_age',  sep = " + ")  

base_prt_command = reg_command('combined_dta','log_abs_firm_forecast_error', 'log_parent_comp_data',parent_controls,
                               fe ="| industry_group + fiscal_year", cluster = 'ibes_ticker + fiscal_year')

variations = data.table(restrict = NA,interaction = NA,controls = 'full', cluster = 'firm x year',fe = 'industry',command = base_prt_command) %>% 
  bind_rows(mutate(.,fe = 'firm', command = gsub('\\| industry_group', '| ibes_ticker',command))) %>% 
  bind_rows(mutate(.,controls = 'drop analyst', command = gsub("\\+ log_abs_analyst_forecast_error", "", command))) %>% 
  bind_rows(mutate(.[1:2],cluster = 'firm', command = gsub('~ibes_ticker \\+ fiscal_year', '~ibes_ticker', command))) %>% 
  bind_rows(mutate(.[5:6],restrict = 'first_forecast', command = gsub('combined_dta', 'combined_dta[first_forecast == T]', command))) %>% 
  bind_rows(mutate(.[1:2],interaction = 'log_age', command = gsub('_data', '_data*log_age',command))) %>% 
  bind_rows(mutate(.[1:2], interaction = 'log_compustat_rev_init', command = gsub('_data', '_data*log_compustat_rev_init', command)))

model_output = evaluate_variations(variations)$model_output
base_coef_names = c('log payroll data', 'log payroll total', 'log forecast horizon',
                    'share empl. college grad', 'empl. prestige',
                    'log abs. analyst\nforecast error', 'log age')

label = '9a_forecast_error' 
format_table(model_output[1:8],label = label, caption = 'Impact of Data Spending on Next Period Forecast Errors',
             coef_names = base_coef_names, 
             custom_rows = list(c('Industry / Firm Fe', rep(c('industry', 'firm'), 4)),
                                c('First Forecast Only', rep('X', 6), rep('\\checkmark', 2) ),
                                c('SE cluster', rep(c('firm x year', 'firm'), each = 4))),
             custom_row_placement = 23:26, make_tex = F, make_pdf = T,
             output_path = paste0(de_dummy(finished_output_dir), label, '.pdf'),
             rescale_factor = 1
)

label = '9b_forecast_error_interactions'
interact_coef_names = c(base_coef_names, 'log payroll data x log age', 'log init sales', 'log payroll data x log init sales')
format_table(model_output[c(1:2,9:12)], label = label, caption = 'Impact of Data Spending on Next Period Forecast Errors',
             coef_order = c(1,7:10,2:6),
             coef_names = interact_coef_names, 
             output_path = paste0(de_dummy(finished_output_dir), label, '.pdf'),
             notes = 'Two-Way Robust standard errors clustered at the fiscal year x firm level.',
             note_width = 1,
             custom_rows = list("",c('Industry / Firm Fe', rep(c('industry', 'firm'), 3))),
             custom_row_placement = c(18,30),
             rescale_factor = 1, make_pdf = T, make_tex = F)


# generate results about productivity dispersion  -------------------------
model_dta = import_file("1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet") %>% .[first_or_no_forecast == T] %>% 
  .[, age_sq := age^2] %>% .[,obs_count := .N, by = PERMNO] %>% 
  .[obs_count > 5, detrended_var := sub_regression(log_compustat_rev, age, log_compustat_emp, log_compustat_capital, residuals = T)^2, by = PERMNO] %>% 
  select(detrended_var, everything()) %>% 
  .[, pct_firm_forecast_error := abs_firm_forecast_error / realized_value]

feols(model_dta, detrended_var ~ log_parent_comp_data + log_parent_comp_total + 
        parent_share_empl_college  + parent_avg_prestige + log_age + log_compustat_capital | industry_group + fiscal_year, cluster = ~ ibes_ticker)


feols(model_dta, log_compustat_rev ~ log_parent_comp_data + log_parent_comp_total + 
        parent_share_empl_college  + parent_avg_prestige + log_age + log_compustat_capital| industry_group + fiscal_year, cluster = ~ ibes_ticker)

feols(model_dta, log_compustat_rev ~ log_parent_comp_data + log_parent_comp_total + 
        parent_share_empl_college  + parent_avg_prestige + log_age + log_compustat_capital + pct_firm_forecast_error| industry_group + fiscal_year, cluster = ~ ibes_ticker)


feols(model_dta, detrended_var ~ log_parent_comp_data + log_parent_comp_total + 
        parent_share_empl_college  + parent_avg_prestige + log_age +log_compustat_capital | ibes_ticker + fiscal_year, cluster = ~ ibes_ticker)

feols(model_dta, log_compustat_rev ~ log_parent_comp_data + log_parent_comp_total + 
        parent_share_empl_college  + parent_avg_prestige + log_age + log_compustat_capital  | ibes_ticker + fiscal_year, cluster = ~ ibes_ticker)

feols(model_dta, detrended_var ~ factor(data_spend_quartile_IBES_sample) + log_parent_comp_total + 
        parent_share_empl_college  + parent_avg_prestige + log_age + log_compustat_capital | industry_group + fiscal_year, cluster = ~ ibes_ticker)


