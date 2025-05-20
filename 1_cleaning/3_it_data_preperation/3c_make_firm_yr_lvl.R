
# import datasets/ set parameter values  --------------------------------------------------------
bs_br = import_file('1) data/3_bs_br_data.csv',char_vars =  c('firmid')) %>%
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T)
if(dummy_version){ bs_br[,NACE_BR := first(NACE_BR), by = firmid]}
  
age_data = import_file(file.path(inputs_dir, '16a_firm_lvl_birth_data.parquet'),
                       col_select = c('birth_year', 'first_export_year', 'firmid', 'last_observed'))  
ctry_lvl_age_data = import_file(file.path(inputs_dir, '16b_firm_ctry_lvl_birth_data.parquet'))
linkedin_ctry_lvl = import_file(linkedin_ctry_lvl_path)
linkedin_firm_lvl = import_file(linkedin_basic_path)
export_data =  import_file('1) data/9_customs_cleaned.csv',
  col_select =  c('firmid', 'ctry', 'year', 'exim', 'deflated_value', 'products', 'streak_id'), char_vars = 'firmid') %>% 
  .[exim == 2 & ctry != 'FR' & year %in% year_range] %>% .[,exim := NULL] %>%
  distinct(firmid,year,ctry, .keep_all = T)
 


linkedin_matched_firms = import_file(linkedin_match_path)[['firmid']]
temp_1_export_vars = c('total_export_rev_customs', 'num_product_x_ctry_markets', 'intermarket_hhi', 'avg_streak_age', 'num_export_countries')
temp_2_export_vars =  gpaste(c('empl', 'comp'),"_", c('now', 'l5', 'ever'))
export_vars_to_zero = c('total_export_rev_customs', 'num_product_x_ctry_markets', "num_export_countries")
compensation_type_vars = gpaste('comp_', c('french', 'engineer','data', 'rnd','stem', 'french_data'))
vars_to_log = c('age',  'turnover','dom_turnover', 'total_export_rev_customs', 'empl', 'capital',
                gpaste(c('comp_data', 'comp_rnd', 'comp_total','comp_abroad'),c('', '_lag1')),
                gpaste('export_mkt_avg_rev_wgted_comp_', c('now', 'l5', 'ever')))

# for relative values 
d_vars = c("comp_data", "share_comp_data")
divisions_list = list(list('nace', c('NACE_BR', 'year')),list('nace_exporter', c('NACE_BR', 'currently_export', 'year'))) 
# generate firm-yr level datasets describing export activity ----------------------------------------------------------------------
temp_1 = export_data %>%
  merge(ctry_lvl_age_data, all.x = T) %>% 
  .[,`:=`(num_export_countries = 1, avg_streak_age = year - streak_start)] %>% 
  .[, intermarket_hhi := (deflated_value / NA_sum(deflated_value))^2, by = .(firmid, year)]  %>%
    rename(num_product_x_ctry_markets = products, total_export_rev_customs = deflated_value) %>%
  .[, lapply(.SD, NA_sum), .SDcols = temp_1_export_vars, by = .(firmid, year)]  %>% 
  .[, `:=`(avg_streak_age = avg_streak_age / num_export_countries,
           avg_products_per_ctry = num_product_x_ctry_markets / num_export_countries)]


temp_2 = export_data %>% 
  merge(linkedin_ctry_lvl, all.x = T, by = c('firmid', 'ctry', 'year')) %>% 
  .[firmid %in% linkedin_matched_firms] %>% 
  .[, (temp_2_export_vars) := lapply(.SD,function(x) replace_na(x,0)), .SDcols = temp_2_export_vars] %>% 
  .[,(paste0('rev_wgted_', temp_2_export_vars)) := lapply(.SD, function(x)
    x * deflated_value / NA_sum(deflated_value)), .SDcols = temp_2_export_vars, by = .(firmid, year)] %>%
  .[, lapply(.SD, NA_mean), .SDcols = gpaste(c('', "rev_wgted_"), temp_2_export_vars), by = .(firmid, year)] %>% 
  rename_with(.cols = -c(firmid,year), ~ paste0('export_mkt_avg_',.))

# merge datasets together  ------------------------------------------------

output = bs_br %>% 
  merge(linkedin_firm_lvl, all.x = T, by = c('firmid', 'year')) %>%
  merge(age_data, all.x = T, by = 'firmid') %>% 
  .[, age := year - birth_year] %>% 
  merge(temp_1, by = c('firmid', 'year'), all.x = T) %>%
  merge(temp_2, by = c('firmid', 'year'), all.x = T) %>% 
  .[, (export_vars_to_zero) := lapply(.SD,function(x) replace_na(x,0)), .SDcols = export_vars_to_zero] %>%
  .[, `:=`(
    young = year - birth_year <= 5,
    adolescent = year - birth_year <= 10,
    is_first_export_year = year == first_export_year,
    currently_export = total_export_rev_customs > 0)]  %>% 
  .[,min_age := NA_min(age), by = firmid] %>% 
  .[,young_at_start := min_age <= 5] %>% 

  ## add variance / churn metrics 
  variance_metrics(time_id = 'year', group_id = 'NACE_BR', ind_id = 'firmid',
                 int_id = 'dom_turnover',  birth_id = 'birth_year', logged_version = T, prefix = 'nace_',
                 full_dataset = T) %>% 
  calc_churn_rates(., 'NACE_BR', 'birth_year', 'last_observed', 'year', 'nace') %>% 
  ##log variables
  .[,paste0('log_',vars_to_log) := lapply(.SD,asinh), .SDcols = vars_to_log]


  # generate comparison vars 
  for (i in 1:length(divisions_list)){inner = divisions_list[[i]][[1]]; outer = ''; group = divisions_list[[i]][[2]]
  for (j in 1:2){if (j ==2){outer = "_age"; group = c(group,'young')}
   output =output %>% 
      .[, (gpaste(d_vars,"_", inner,'_quartile', outer)) := lapply(d_vars, function(x) as.factor(ntile(get(x), 4))), by = group] %>% 
      .[, (gpaste(d_vars,'_',inner,'_pct_rank',outer)) := lapply(d_vars, function(x) percent_rank(get(x))), by = group] %>%
      .[, (gpaste(d_vars,'_',inner,'_sd_from_mean',outer)) := lapply(d_vars, function(x)(get(x)- NA_mean(get(x)))/ NA_sd(get(x))), by = group]
  }}

  # add year variables for event study 
  vars_to_yr = c('comp_data', 'log_comp_data')
  for (yr in year_range) output[, (gpaste(vars_to_yr, "_y", yr)) := lapply(vars_to_yr, function(x) ifelse(year == yr, get(x), 0))]


write_parquet(output,file.path(inputs_dir, '16c_firm_yr_lvl.parquet'))

# clear ------------------------------
rm(list= setdiff(ls(), base_env)); gc()

  