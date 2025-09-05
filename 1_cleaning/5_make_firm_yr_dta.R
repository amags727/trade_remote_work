# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))};gc()

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

source('2) code/0_set_parameter_values.R')


# import component datasets --------------------------------------------------------
### ADD IN THE AGE DATA 
age_vars =  c('birth_year', gpaste('first_export_year_', c('customs', 'BS')),'last_observed', 'firmid_num')
age_data = import_file(firm_lvl_birth_path, col_select = age_vars)
firm_lvl_streak_info = import_file(firm_lvl_export_birth_path)

### ADD IN THE LINKEDIN DATA 
NACE_2d_info = if(!dummy_version){import_file("1) Data/0_misc_data/0a_nace_2d_industry_categories.csv")}else{data.table(NACE_2d = 0:5, industry_category = letters[1:6])}
linkedin_firm_yr = import_file(linkedin_firm_yr_path) %>% 
  .[, NACE_2d := as.integer(substr(as.character(str_pad(NACE_BR, 4, side="left", pad="0")), 1, 2))] %>% 
  merge(NACE_2d_info, all.x = T)

### ADD IN THE BUSINESS SURVEY DATA
bs_vars = c('firmid_num', 'year','dom_turnover', 'empl', 'capital', 'intangible_fixed_assets','for_turnover', 'labor_cost','turnover', 'NACE_BR')
bs_data = import_file(raw_bs_br_path,col_select = bs_vars) %>% 
  rename(total_export_rev_BS = for_turnover) %>% 
  .[, nace_share_dom := dom_turnover / NA_sum(dom_turnover), by = .(NACE_BR, year)] %>% 
  .[, nace_leader_dom := dom_turnover > quantile(dom_turnover, 0.99, na.rm = TRUE), by = .(NACE_BR, year)] %>% 
  .[, nace_HHI_dom := NA_sum(nace_share_dom^2), by = .(NACE_BR, year)]  %>% .[,NACE_BR := NULL] %>% 
  .[year %in% year_range & firmid_num %in% linkedin_firm_yr$firmid_num] %>% 
  .[, empl_bin := case_when(is.na(empl) ~NA_character_, empl < 50 ~ 'small', empl < 200 ~'medium', T ~ 'large')] 


### ADD IN THE CUSTOMS DATA 
export_vars = c('firmid_num', 'year', 'exim', 'value','products')
export_data = import_file(raw_customs_firm_lvl_path, col_select = export_vars) %>%
  .[exim == 2 & year %in% year_range] %>% .[,exim := NULL] %>%
  .[,.(num_mkts = .N, total_export_rev_customs = sum(value, na.rm = T), products_per_ctry = NA_mean(products)), by = .(firmid_num, year)]

export_product_counts = import_file(raw_customs_product_lvl_path, col_select = c('year', 'exim', 'CN8plus', 'firmid_num')) %>%
  .[exim == 2 & year %in% year_range & firmid_num %in% linkedin_firm_yr$firmid_num] %>% 
  distinct(year, firmid_num, CN8plus) %>% 
  .[, .(num_unique_export_products = .N), by = .(firmid_num, year)]

network_closeness = import_file(raw_customs_firm_lvl_path, col_select = c(export_vars,'ctry_num')) %>%
  .[exim == 2 & year %in% year_range] %>% 
  rbind(bs_data[!is.na(dom_turnover), c('firmid_num', 'year', 'dom_turnover')] %>% rename(value = dom_turnover) %>% .[,ctry_num := 0], fill = T) %>% 
  .[, currently_exporting := any(ctry_num !=0), by = .(firmid_num, year)] %>% .[currently_exporting == T] %>% .[,currently_exporting := NULL] %>%
  merge(.[,c('ctry_num', 'firmid_num', 'year', 'value')] %>% rename(d_ctry_num =  ctry_num, d_value = value), by = c('firmid_num', 'year'), allow.cartesian = T) %>%
  rename(o_ctry_num = ctry_num) %>% .[,value := value * d_value] %>% 
  .[,`:=`(not_france = o_ctry_num != 0 & d_ctry_num != 0, not_self = o_ctry_num  != d_ctry_num)] %>% 
  merge(import_file(similiarity_dir, '/outputs/overall_distance_data.csv'), by = con_fil(.,'ctry')) %>% 
  .[!is.na(distance_km)] %>% 
  .[,.(network_closeness_inc_france_inc_self = weighted.mean(distance_km, value),
       network_closeness_excl_france_inc_self = weighted.mean(distance_km[not_france], value[not_france]),
       network_closeness_inc_france_excl_self = weighted.mean(distance_km[not_self], value[not_self]),
       network_closeness_excl_france_excl_self = weighted.mean(distance_km[not_self & not_france], value[not_self & not_france])), by = .(firmid_num, year)] %>% 
  .[, con_fil(., 'network') := lapply(.SD, asinh), .SDcols = con_fil(., 'network')]

# prepare output  ---------------------------------------------------------
suffixes = c('customs', 'BS')
vars_to_cond = c('num_unique_export_products','num_mkts', 'products_per_ctry', gpaste('total_export_rev_' ,suffixes))
vars_to_log = c('age', 'dom_turnover','num_mkts','empl',  gpaste("total_export_rev_",suffixes, c('', '_cond')),
                gpaste(c('export_streak_age', 'years_since_first_export_year'), "_",suffixes))


output = merge(bs_data, linkedin_firm_yr, by = c('firmid_num', 'year')) %>% 
  ## merge in process/age data
  merge(age_data, by = 'firmid_num', all.x = T) %>% 
  .[, age := year - birth_year] %>% remove_if_NA('age') %>% 
  .[, (paste0("years_since_first_export_year_", suffixes)) := lapply(suffixes, function(x) ifelse(year <get(paste0('first_export_year_', x)), NA, year -get(paste0('first_export_year_', x))))] %>% 
  .[, (paste0("is_first_export_year_", suffixes)) := lapply(suffixes, function(x) year == get(paste0('first_export_year_', x)))] %>% 
  .[, `:=`(age_bracket = case_when(age <= 5 ~ "<= 5", age <= 10 ~ "6-10", age <= 20 ~ "11-20", T ~'20+'), young = age <=5)]  %>% 
  
  
  ## merge in / process export data 
  merge(export_data, all.x = T,  by = c('firmid_num', 'year')) %>% 
  merge(export_product_counts, all.x =T , by = c('firmid_num', 'year')) %>% 
  merge(network_closeness,all.x = T, by = c('firmid_num', 'year')) %>% 
  .[, (paste0(vars_to_cond, '_cond')) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols =vars_to_cond] %>%
  .[, (vars_to_cond) := lapply(.SD, function(x) replace_na(x, 0)), .SDcols =vars_to_cond] %>% 
  .[, (paste0('currently_export_', c('customs', 'BS'))) := lapply(c('customs', 'BS'), function(x) get(paste0('total_export_rev_',x))>0)] %>% 
 
 
  ## merge in info on export streaks 
  merge(firm_lvl_streak_info, all.x = T, by= c('firmid_num', 'year')) %>% 
  
  ## make log variables 
  .[,paste0('log_', vars_to_log) := lapply(vars_to_log, function(x) asinh(get(x)))] %>% 
  .[,log_dom_turnover_sq := log_dom_turnover^2] %>% 

  ## make vars based on leads 
  unbalanced_lag(.,'firmid_num', 'year', c('currently_export_BS', 'currently_export_customs'), -1, expand = T, expand_value = F, death_var = 'last_observed') %>% 
  .[currently_export_BS == T, stop_exporting_BS := !currently_export_BS_lead1] %>% 
  .[currently_export_customs == T, stop_exporting_customs := !currently_export_customs_lead1] %>% 
  
  ## add industry variables 
  .[, gpaste('nace_share_export_', suffixes):= lapply(suffixes, function(x) NA_mean(get(paste0('currently_export_', x)))), by = c('NACE_BR', 'year')]

  ## add detrended variance values 
  if (dummy_version){ ## the regressions will fail if we use dummy data 
   detrended_vars = gpaste('log_total_export_rev_',suffixes, c('', '_cond'),'_detrended_var')
   output[, (detrended_vars) := runif(.N)]
  }else{
  for( suffix in suffixes){
    command_1 = gpaste('feols(output, log_total_export_rev_',suffix, '~log_age| firmid_num + year)')
    command_2 = gsub('log_age', gpaste('log_export_streak_age_', suffix), command_1)
    models = list(eval(parse(text = command_1)), eval(parse(text = command_2)))
    for (i in 1:2){
      var_name = gpaste('log_total_export_rev_',suffix, ifelse(i==1, '', '_cond'), "_detrended_var")
      non_dropped_obs = setdiff(1:nrow(output),-1*models[[i]]$obs_selection$obsRemoved)
      output[non_dropped_obs, (var_name) := models[[i]]$residuals^2]
      }
  }
  }

  ## add size adjusted comp data (metric for how unusual data comp is for a firm of that size)
  model = feols(data = output, log_comp_data ~ asinh(turnover) + I(asinh(turnover)^2))
  non_dropped_obs =  setdiff(1:nrow(output),-1*model$obs_selection$obsRemoved)
  output[non_dropped_obs, size_adjusted_comp_data := model$residuals] 
  
  ## add in lags for overall exporter behavior 
  output = output %>% unbalanced_lag(., 'firmid_num', 'year',con_fil(., 'share_export'), 1) %>% 
  
  # Create capital intensity as capital to total revenue
  .[, capital_intensity:=ifelse(turnover==0, NA, log(capital/(turnover)))] %>% 
  

  ## gen brackets based on time since starting to export / time in the current export streak 
  mutate(across(gpaste(c('export_streak_age_','years_since_first_export_year_'), suffixes), ~ case_when(
    is.na(.) ~ NA_character_, . < 2 ~ '0-1', . < 5 ~ '2-4', T ~ '5+'), .names = '{col}_bracket')) %>% 
  
  ### generate domestic size quartile 
  .[, dom_turnover_quartile := ntile(dom_turnover, 4), by = .(year)]

  ## generate variables based on the initial condition of the firm
  init_vars = c('for_to_dom_rev_ratio_BS', 'for_to_dom_rev_ratio_customs', 'num_mkts_bracket',
                'log_num_mkts', 'log_dom_turnover', 'dom_turnover_quartile','log_empl', 'empl_bin', 'nace_share_dom')
  setorder(output,firmid_num, year)
  output = output[dom_turnover != 0, `:=`(for_to_dom_rev_ratio_BS =  total_export_rev_BS/ dom_turnover, 
                                 for_to_dom_rev_ratio_customs = total_export_rev_customs / dom_turnover)] %>% 
  .[, num_mkts_bracket := lapply(.SD, function(x) case_when(is.na(x) ~ NA_character_, x == 0 ~ '0', x < 5 ~ '1-5', T ~ '5+')), .SDcols = 'num_mkts'] %>% 
  .[, paste0(init_vars, '_init') := lapply(.SD, function(x) na.omit(x)[1]), by = firmid_num, .SDcols = init_vars] 

## EXPORT FILE   
write_parquet(output,firm_yr_path)
  
  
rm(list= setdiff(ls(), c(base_env))); gc()
# generate unmatched input for summary stats ------------------------------
#### 
#import data
####
linkedin_firms =import_file(linkedin_firm_yr_path, col_select = 'firmid_num') %>% pull('firmid_num') %>% unique()

bs_vars = c('firmid_num', 'year', 'dom_turnover', 'empl', 'capital', 'intangible_fixed_assets','for_turnover')
bs_data = import_file(raw_bs_br_path,col_select = bs_vars) %>% 
  rename(total_export_rev_BS = for_turnover) %>%
  .[year %in% year_range &! firmid_num %in% linkedin_firms] 

export_vars = c('firmid_num', 'year', 'exim', 'value','products')
export_data = import_file(raw_customs_firm_lvl_path, col_select = export_vars) %>%
  .[exim == 2 & year %in% year_range &! firmid_num %in% linkedin_firms] %>% .[,exim := NULL] %>%
  .[,.(num_mkts = .N, total_export_rev_customs = sum(value, na.rm = T), products_per_ctry = NA_mean(products)), by = .(firmid_num, year)]

age_data = import_file(firm_lvl_birth_path, col_select = c('firmid_num', 'birth_year'))
firm_yr_linkedin = import_file(firm_yr_path) %>% .[,`:=`(comp_data = sinh(log_comp_data), comp_total= sinh(log_comp_total)) ]

#### 
# merge and clean
####
suffixes = c('customs', 'BS')
vars_to_cond = c('num_mkts', 'products_per_ctry', gpaste('total_export_rev_' ,suffixes))

output = bs_data %>% 
  ## add export data / vars 
  merge(export_data, all.x = T, by = c('firmid_num', 'year')) %>% 
  .[, (paste0(vars_to_cond, '_cond')) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols =vars_to_cond] %>%
  .[, (vars_to_cond) := lapply(.SD, function(x) replace_na(x, 0)), .SDcols =vars_to_cond] %>%
  .[, (paste0('currently_export_', c('customs', 'BS'))) := lapply(c('customs', 'BS'), function(x) get(paste0('total_export_rev_',x))>0)] %>% 
  
  ## add in age data / vars 
  merge(age_data, all.x = T, by = 'firmid_num') %>% 
  .[,age := year - birth_year] 

## combine with the linkedin firms 
output = rbindlist(list(output,firm_yr_linkedin), use.names = T, fill = T)[, in_linkedin := !is.na(comp_data)]

## export 
write_parquet(output, firm_yr_summary_stats_path)
# cleanup -----------------------------------------------------------------
rm(list= setdiff(ls(), c(base_env))); gc()









