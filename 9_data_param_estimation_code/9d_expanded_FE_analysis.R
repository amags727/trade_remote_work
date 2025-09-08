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
# Import financial data -----------------------------------------------------------------
importing_raw_financials = F
if (importing_raw_financials){

#### FORECAST DATA 
#https://wrds-www.wharton.upenn.edu/pages/get-data/lseg-ibes/ibes-guidance/detail-history/detail/
IBES_forecast_data =  wrds_query("SELECT ticker AS ibes_ticker, measure, curr, units, anndats AS fcst_ann_date, prd_yr, prd_mon, val_1, val_2, 
                                 mean_at_date AS analyst_forecast_mean FROM ibes.det_guidance 
                                 WHERE pdicity = 'ANN' AND usfirm = 1 AND val_1 IS NOT NULL AND curr = 'USD'") %>% 
  .[measure %in% c("EPS","SAL")] %>%
  
  ## clean up how the forecast is reported 
  .[,forecast := apply(.SD, 1, mean, na.rm = TRUE), .SDcols = c('val_1','val_2')] %>% 
  .[!is.na(val_2), `:=`(forecast_lb = val_1, forecast_ub = val_2, forecast_range = val_2 - val_1)] %>% 
  .[forecast_range < 0, paste0('forecast', c('', '_lb', '_ub', '_range')):= NA] %>% 
  .[forecast_range == 0 ,  paste0('forecast', c('_lb', '_ub', '_range')):= NA] %>% 
  .[,c('val_1', 'val_2') := NULL] %>%
  
  ## remove forecasts that appear after the fact and those that are over a year away 
  .[, end_of_period := ymd(paste(prd_yr, prd_mon, 1, sep = "-")) %m+% months(1) - days(1)] %>% 
  .[fcst_ann_date <= end_of_period & end_of_period - fcst_ann_date < 365] %>% .[,end_of_period := NULL] 
  
  ## mark the first / last forecast 
  setorder(IBES_forecast_data, ibes_ticker, measure, prd_yr, fcst_ann_date)
  IBES_forecast_data[, `:=`(first_forecast = .I == .I[1], last_forecast = .I == .I[.N]), by = c('ibes_ticker', 'measure', 'prd_yr')]

###### ACTUAL VALUES OF THE FORECASTED VARIABLES 
#https://wrds-www.wharton.upenn.edu/pages/get-data/lseg-ibes/ibes-academic/detail-history/actuals/  
IBES_actual = import_file("1) data/11_parameter_calibration/raw/IBES_actuals_raw.csv") %>% rename_with(~tolower(.)) %>% 
  .[, pends := as.Date(pends)] %>% .[, `:=`(prd_yr = year(pends), prd_mon = month(pends))]  %>%
  rename(ibes_ticker= ticker, realized_value = value) %>% 
  distinct(., ibes_ticker, prd_yr, prd_mon, measure, .keep_all = T) %>% 
  select('ibes_ticker', 'cusip','pends', 'prd_yr', 'prd_mon', 'measure', 'realized_value') 

### Import DATASETS NEEDED FOR MATCHING
isin_matching =  wrds_query("SELECT item6003 AS firm_name, item6038 AS ibes_ticker, item6008 AS isin, item6011 AS industry_group,
                            item18272 AS birth_date, item18273 AS incorporation_date FROM trws.wrds_ws_company WHERE item6038 is not NULL") %>% 
  distinct(ibes_ticker, .keep_all = T)
rcid_matching =  wrds_query("SELECT rcid,child_rcid, ultimate_parent_rcid, year_founded, isin, cusip  FROM revelio.company_mapping WHERE isin is not NULL or cusip is not NULL") %>% .[,cusip8 := substr(cusip, 1, 8)]
  

IBES_combined = merge(IBES_forecast_data, IBES_actual,  by = c('ibes_ticker', 'prd_yr', 'prd_mon', 'measure')) %>%
   merge(isin_matching, all.x = T, by ='ibes_ticker') %>% 
  .[, forecast_horizon := as.numeric(pends - fcst_ann_date)] %>% 
  .[, fiscal_year := ifelse(prd_mon >5, prd_yr, prd_yr - 1)] %>%
  .[, birth_year := ifelse(!is.na(birth_date), year(birth_date), year(incorporation_date))] %>% 
  .[, age := fiscal_year - birth_year] %>% 
  .[, pstart := pends %m-% years(1) + days(1)] %>% 
  
  ### MERGE IN THE REVELIO DATA 
  merge(rcid_matching[!is.na(isin),c('isin', 'rcid')], by = 'isin', all.x = T) %>% 
  merge(rcid_matching[,c('rcid', 'cusip8')] %>% rename(rcid_cusip = rcid, cusip = cusip8), by= 'cusip', all.x = T) %>% as.data.table() %>% 
  .[is.na(rcid), rcid := rcid_cusip] %>% .[,rcid_cusip := NULL]  %>%
  merge(rcid_matching[,c('rcid','child_rcid', 'ultimate_parent_rcid')], all.x = T, by = 'rcid') 

  duplicated_rcids = distinct(IBES_combined, ibes_ticker, rcid) %>% drop_na() %>% .[,.(count = .N), by = rcid] %>% .[count >1] %>% pull(rcid) 
  IBES_combined[rcid %in% duplicated_rcids, rcid := NA] %>% .[,ultimate_parent :=  ultimate_parent_rcid == rcid] %>% 
  select(ibes_ticker,cusip,rcid,ultimate_parent, fcst_ann_date, prd_yr,pstart, pends, measure, forecast, analyst_forecast_mean, realized_value, everything())
write_parquet(IBES_combined, "1) data/11_parameter_calibration/raw/IBES_combined_raw.parquet")
}






# import role data --------------------------------------------------------
importing_basic_role = F
if (importing_basic_role){ 
IBES_combined = import_file("1) data/11_parameter_calibration/raw/IBES_combined_raw.parquet")

### GENERATE THE ROLE DATA 
rcid_list = sample(setdiff(unique(IBES_combined$rcid), NA))
rcid_list = split(rcid_list, cut(seq_along(rcid_list), 500, labels = FALSE))
for (i in rev(1:length(rcid_list))){
  if (!file.exists(paste0('1) data/11_parameter_calibration/raw/temp/role_',i,'.csv'))){
    fwrite(data.frame(h = 1), paste0('1) data/11_parameter_calibration/raw/temp/role_',i,'.csv'))
    print(i)
    rcid_segment = rcid_list[[i]]
    in_clause <- paste(sprintf("'%s'", gsub("'", "''", rcid_segment)), collapse = ",")
    temp = wrds_query(sprintf("
  SELECT ip.rcid, ip.startdate, ip.enddate, ip.role_k1500, ip.total_compensation, ip.weight, 
  iu.prestige, iu.highest_degree 
  FROM revelio.individual_positions ip 
  LEFT JOIN revelio.individual_user iu ON ip.user_id = iu.user_id
  WHERE ip.total_compensation IS NOT NULL AND ip.startdate IS NOT NULL AND ip.rcid IN (%s)", in_clause)) %>% 
      .[, `:=`(college = highest_degree %in% c('Master', 'Bachelor', 'MBA', 'Doctor'),
               college_total = !is.na(highest_degree))] %>% 
      .[!is.na(weight) & !is.na(total_compensation)] %>% 
      .[, comp := total_compensation * weight] %>% 
      merge(import_file('1) data/7_revelio_data/b_processed_data/linkedin/revelio_role_dict.csv'), by = 'role_k1500')
    
    output_table = rbindlist(lapply(2000:2024, function(yr){
      sub_temp = lapply(c(0,1), function(lag){
        sub_temp = IBES_combined[rcid %in% rcid_segment & fiscal_year == yr] %>% 
          distinct(rcid, fiscal_year, .keep_all = T) %>% 
          .[,c('rcid', 'fiscal_year', 'pstart', 'pends')] %>%
          .[, `:=`(pstart= pstart %m-% years(lag), pends= pends %m-% years(lag))] %>% 
          merge(temp, by = 'rcid') %>% 
          .[startdate <= pends & (enddate >= pstart | is.na(enddate))]  %>% 
          .[, .(comp_total = sum(comp), empl_total = sum(weight),
                comp_data = sum(comp*data), empl_data = sum(weight*data),
                avg_prestige = NA_mean(prestige),
                share_empl_college = NA_sum(weight*college)/ NA_sum(weight*college_total)), by = .(rcid, fiscal_year)] 
        
        if (lag == 1) sub_temp = sub_temp %>% rename_with(.cols = setdiff(names(sub_temp), c('rcid','fiscal_year')), ~paste0(.,'_lag1'))
        return(sub_temp)
      })
      sub_temp = merge(sub_temp[[1]], sub_temp[[2]], by = c('rcid', 'fiscal_year'), all = T)
    }))
    fwrite(output_table, paste0('1) data/11_parameter_calibration/raw/temp/role_',i,'.csv'))
  }
}


file_list = list.files(path = '1) data/11_parameter_calibration/raw/temp', full.names = T)
role_dta = rbindlist(lapply(file_list,import_file), use.names = T, fill = T)
write_parquet(role_dta,'1) data/11_parameter_calibration/raw/9d_version_firm_role_dta.parquet') 
}

# import role data of parent company --------------------------------------

importing_parent_role = F
if(importing_parent_role){
  
  IBES_combined = import_file("1) data/11_parameter_calibration/raw/IBES_combined_raw.parquet")
  
  ### GENERATE THE ROLE DATA 
  rcid_list = setdiff(unique(IBES_combined[ultimate_parent_rcid == rcid][['rcid']]), NA)
  
  for (i in (1:whole_way)){
    if (!file.exists(paste0('1) data/11_parameter_calibration/raw/parent_temp/parent_role_',i,'.csv'))){
      print(paste0(round(i/length(rcid_list)*100,2),'%'))
      parent_list = rcid_list[i]
      in_clause <- paste(sprintf("'%s'", gsub("'", "''", parent_list)), collapse = ",")
      
      temp =  wrds_query(sprintf("WITH target_rcids AS (
      SELECT DISTINCT cm.rcid, cm.ultimate_parent_rcid
      FROM revelio.company_mapping cm
      WHERE cm.ultimate_parent_rcid IN (%s)
    )
    SELECT
      t.ultimate_parent_rcid  AS parent_rcid, ip.rcid, ip.startdate, ip.enddate, ip.role_k1500,
      ip.total_compensation, ip.weight, iu.prestige, iu.highest_degree 
      FROM target_rcids t
      JOIN revelio.individual_positions ip ON ip.rcid = t.rcid
    LEFT JOIN revelio.individual_user iu ON iu.user_id = ip.user_id
    WHERE ip.total_compensation IS NOT NULL AND ip.startdate IS NOT NULL
  ", in_clause)) %>% select(-rcid) %>% rename(rcid = parent_rcid) %>% 
        .[, `:=`(college = highest_degree %in% c('Master', 'Bachelor', 'MBA', 'Doctor'),
                 college_total = !is.na(highest_degree))] %>% 
        .[!is.na(weight) & !is.na(total_compensation)] %>% 
        .[, comp := total_compensation * weight] %>% 
        merge(import_file('1) data/7_revelio_data/b_processed_data/linkedin/revelio_role_dict.csv'), by = 'role_k1500')
      
      output_table = rbindlist(lapply(2000:2024, function(yr){
        sub_temp = lapply(c(0,1), function(lag){
          sub_temp = IBES_combined[rcid == rcid_list[i] & fiscal_year == yr] %>% 
            distinct(rcid, fiscal_year, .keep_all = T) %>% 
            .[,c('rcid', 'fiscal_year', 'pstart', 'pends')] %>%
            .[, `:=`(pstart= pstart %m-% years(lag), pends= pends %m-% years(lag))] %>% 
            merge(temp, by = 'rcid') %>% 
            .[startdate <= pends & (enddate >= pstart | is.na(enddate))]  %>% 
            .[, .(comp_total = sum(comp), empl_total = sum(weight),
                  comp_data = sum(comp*data), empl_data = sum(weight*data),
                  avg_prestige = NA_mean(prestige),
                  share_empl_college = NA_sum(weight*college)/ NA_sum(weight*college_total)), by = .(rcid, fiscal_year)] 
          
          if (lag == 1) sub_temp = sub_temp %>% rename_with(.cols = setdiff(names(sub_temp), c('rcid','fiscal_year')), ~paste0(.,'_lag1'))
          return(sub_temp)
        })
        sub_temp = merge(sub_temp[[1]], sub_temp[[2]], by = c('rcid', 'fiscal_year'), all = T)
      })) %>% 
        rename_with(.cols = con_fil(., 'rcid', 'fiscal_year', inc = F), ~paste0('parent_', .))
      fwrite(output_table, paste0('1) data/11_parameter_calibration/raw/parent_temp/parent_role_',i,'.csv'))
    }
  }
  
  file_list = list.files(path = '1) data/11_parameter_calibration/raw/parent_temp', full.names = T)
  parent_role_dta = rbindlist(lapply(file_list,import_file), use.names = T, fill = T)
  write_parquet(parent_role_dta,'1) data/11_parameter_calibration/raw/9d_version_parent_firm_role_dta.parquet') 
}


# import role data of subsidiaries  ---------------------------------------


importing_subsidiary_role = F
if(importing_subsidiary_role){}
IBES_combined = import_file("1) data/11_parameter_calibration/raw/IBES_combined_raw.parquet")
rcid_matching =  wrds_query("SELECT rcid,child_rcid, ultimate_parent_rcid, year_founded, isin, cusip  FROM revelio.company_mapping WHERE isin is not NULL or cusip is not NULL") %>% .[,cusip8 := substr(cusip, 1, 8)]

### OBTAIN THE INFORMATION ON EACH FIRM'S SUBSIDIARIES 
remaining_subsids = Inf; i = 1
all_subsids = IBES_combined %>% distinct(rcid, .keep_all =T) %>%
  .[ultimate_parent_rcid != rcid] %>% 
  select(rcid, child_rcid) %>% rename(child_rcid1 = child_rcid) %>%
  .[child_rcid1 == rcid, child_rcid1 := NA]
while (remaining_subsids > 0){
  ci1 = paste0('child_rcid', i+1); ci = paste0('child_rcid', i)
  command = paste0("all_subsids = merge(all_subsids,rcid_matching %>% rename(",ci1 ,"=child_rcid,",ci, '= rcid) %>% ',
                   ".[,c('", ci1, "', '", ci, "')], by = '",ci,"', all.x = T) %>% .[",ci1, "==", ci,", ", ci1, ":= NA]")
  eval(parse(text = command))
  remaining_subsids = nrow(all_subsids[!is.na(get(ci1))]) 
  i = i +1
}
all_subsids = all_subsids %>% mutate(child_rcid0 = rcid) %>% 
  pivot_longer( cols= -'rcid', values_to = 'child_rcid') %>%
  filter(!is.na(child_rcid)) %>% select(rcid,child_rcid) %>% as.data.table()

rcid_list = sample(unique(all_subsids$rcid))
rcid_list = split(rcid_list, cut(seq_along(rcid_list), 500, labels =F))
for (i in (1:length(rcid_list))){
  out_name = paste0('1) data/11_parameter_calibration/raw/subsid_temp/subsid_role_',i,'.csv')
  if (!file.exists(out_name)){
    fwrite(data.table(h = ''), out_name)
    print(paste0(round(i/length(rcid_list)*100,2),'%'))
    
    rcid_segment = all_subsids[rcid %in%  rcid_list[[i]]][['child_rcid']]
    in_clause <- paste(sprintf("'%s'", gsub("'", "''", rcid_segment)), collapse = ",")
    temp = wrds_query(sprintf("
  SELECT ip.rcid as child_rcid, ip.startdate, ip.enddate, ip.role_k1500, ip.total_compensation, ip.weight, 
  iu.prestige, iu.highest_degree 
  FROM revelio.individual_positions ip 
  LEFT JOIN revelio.individual_user iu ON ip.user_id = iu.user_id
  WHERE ip.total_compensation IS NOT NULL AND ip.startdate IS NOT NULL AND ip.rcid IN (%s)", in_clause)) %>% 
      .[, `:=`(college = highest_degree %in% c('Master', 'Bachelor', 'MBA', 'Doctor'),
               college_total = !is.na(highest_degree))] %>% 
      .[!is.na(weight) & !is.na(total_compensation)] %>% 
      .[, comp := total_compensation * weight] %>% 
      merge(import_file('1) data/7_revelio_data/b_processed_data/linkedin/revelio_role_dict.csv'), by = 'role_k1500') %>% 
      merge( all_subsids, by = 'child_rcid', allow.cartesian = T) 
    
    output_table = rbindlist(lapply(2000:2024, function(yr){
      sub_temp = lapply(c(0,1), function(lag){
        sub_temp = IBES_combined[rcid %in% rcid_segment & fiscal_year == yr] %>% 
          distinct(rcid, fiscal_year, .keep_all = T) %>% 
          .[,c('rcid', 'fiscal_year', 'pstart', 'pends')] %>%
          .[, `:=`(pstart= pstart %m-% years(lag), pends= pends %m-% years(lag))] %>% 
          merge(temp, by = 'rcid') %>% 
          .[startdate <= pends & (enddate >= pstart | is.na(enddate))]  %>% 
          .[, .(comp_total = sum(comp), empl_total = sum(weight),
                comp_data = sum(comp*data), empl_data = sum(weight*data),
                avg_prestige = NA_mean(prestige),
                share_empl_college = NA_sum(weight*college)/ NA_sum(weight*college_total)), by = .(rcid, fiscal_year)] 
        
        if (lag == 1) sub_temp = sub_temp %>% rename_with(.cols = setdiff(names(sub_temp), c('rcid','fiscal_year')), ~paste0(.,'_lag1'))
        return(sub_temp)
      })
      sub_temp = merge(sub_temp[[1]], sub_temp[[2]], by = c('rcid', 'fiscal_year'), all = T)
    })) %>% 
      rename_with(.cols = con_fil(., 'rcid', 'fiscal_year', inc = F), ~paste0('parent_', .))
    fwrite(output_table, out_name)
  }
}
    
file_list = list.files(path = '1) data/11_parameter_calibration/raw/subsid_temp', full.names = T)
subsid_role_dta = rbindlist(lapply(file_list,import_file), use.names = T, fill = T)
write_parquet(subsid_role_dta,'1) data/11_parameter_calibration/raw/9d_version_subsid_firm_role_dta.parquet') 

    
    


# combine and finish cleaning -----------------------------------------------------------------------
vars_to_deflate = c("analyst_forecast_mean","forecast", "forecast_lb", "forecast_ub", "forecast_range", "realized_value")
vars_to_log = c(gpaste(c('', 'parent_'),'comp_', c('data', 'total'), c('', '_lag1')),
                gpaste('abs_',c('analyst', 'firm'), '_forecast_error'), 'forecast_horizon', 'age')

### DEFLATOR DATA 
getSymbols("GDPDEF", src = "FRED")
deflator_dta = as.data.table(GDPDEF) %>% rename_with(~c('date', 'gdp_def')) %>% 
  complete(date = seq(min(date), max(date), by = "day"))  %>% 
  fill(gdp_def, .direction = "down") %>% as.data.table()
base_deflator_val = deflator_dta[date == as.Date('2025-01-01')][['gdp_def']]
deflator_dta[, gdp_def := base_deflator_val/ gdp_def]


### parent role data
parent_role_dta = rbindlist(list(import_file('1) data/11_parameter_calibration/raw/9d_version_parent_firm_role_dta.parquet'),
                                 import_file('1) data/11_parameter_calibration/raw/9d_version_subsid_firm_role_dta.parquet')))

IBES_combined = import_file("1) data/11_parameter_calibration/raw/IBES_combined_raw.parquet") %>% 
  
  ## DEFLATE NOMINAL VALUES 
  merge(deflator_dta, by.x = 'pends', by.y = 'date', all.x = T) %>% 
  .[!is.na(rcid) & measure == 'SAL'] %>%
  .[, (vars_to_deflate) := lapply(.SD, function(x) x*gdp_def), .SDcols = vars_to_deflate] %>% 
  
  ## Generate the forecast errors 
  .[, `:=`(analyst_forecast_error = realized_value - analyst_forecast_mean,
           firm_forecast_error = realized_value - forecast)] %>% 
  mutate(across(con_fil(.,'forecast_error'), ~abs(.), .names = 'abs_{col}'))  %>% 


  ## merge in the role data 
  merge(import_file('1) data/11_parameter_calibration/raw/9d_version_firm_role_dta.parquet'), all.x = T, by = c('rcid','fiscal_year')) %>% 
  merge(parent_role_dta, all.x = T,  by = c('rcid','fiscal_year')) %>% 
  mutate(across(con_fil(., 'comp'), ~.*1-6)) %>% # put compensation vars in millions 

  
  ## log necessary values 
  mutate(across(all_of(vars_to_log), ~asinh(.), .names ='log_{col}')) %>% 
  .[, firm_fiscal_year := .GRP, by =.(ibes_ticker,fiscal_year)] %>% 
  setorder(., fiscal_year) %>% 
  .[fiscal_year %in% year_range, log_realized_value_init := asinh(realized_value[1]), by = .(ibes_ticker, measure)] %>% 

  ## add an interaction for its performance in the prior fiscal year 
  merge(.[,.(prediction_success_lag1 = asinh(NA_mean(abs_firm_forecast_error)),
             relative_prediction_success_lag1 = NA_mean(log_abs_analyst_forecast_error - log_abs_firm_forecast_error )),
                      by =.(ibes_ticker, fiscal_year, measure)] %>% .[,fiscal_year := fiscal_year + 1], all.x = T) %>% 
  
  ### restrict to sample range  
  .[measure == 'SAL' & fiscal_year %in% year_range & ultimate_parent == T] %>%
  .[,young := age <= 5]

fwrite(IBES_combined, "1) data/11_parameter_calibration/clean/9d_IBES_combined_cleaned.parquet")


# conduct analysis  -------------------------------------------------------
IBES_combined = import_file(IBES_combined)
parent_controls = paste("",'log_parent_comp_total', 'log_forecast_horizon','parent_share_empl_college',
                        'parent_avg_prestige','log_abs_analyst_forecast_error', 'log_age',  sep = " + ")  

base_prt_command = reg_command('IBES_combined','log_abs_firm_forecast_error', 'log_parent_comp_data',parent_controls,
                               fe ="| industry_group + fiscal_year", cluster = 'ibes_ticker + fiscal_year')

variations = data.table(restrict = NA,interaction = NA,controls = 'full', cluster = 'firm x year',fe = 'industry',command = base_prt_command) %>% 
  bind_rows(mutate(.,fe = 'firm', command = gsub('\\| industry_group', '| ibes_ticker',command))) %>% 
  bind_rows(mutate(.,controls = 'drop analyst', command = gsub("\\+ log_abs_analyst_forecast_error", "", command))) %>% 
  bind_rows(mutate(.[1:2],cluster = 'firm', command = gsub('~ibes_ticker \\+ fiscal_year', '~ibes_ticker', command))) %>% 
  bind_rows(mutate(.[5:6],restrict = 'first_forecast', command = gsub('_combined', '_combined[first_forecast == T]', command))) %>% 
  bind_rows(mutate(.[1:2],interaction = 'log_age', command = gsub('_data', '_data*log_age',command))) %>% 
  bind_rows(mutate(.[1:2], interaction = 'relative_prediction_success_lag1', command = gsub('_data','_data*relative_prediction_success_lag1', command))) %>% 
  bind_rows(mutate(.[1:2], interaction = 'log_realized_value_init', command = gsub('_data', '_data*log_realized_value_init', command)))

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
format_table(model_output[c(1:2,9:10,13:14)], label = label, caption = 'Impact of Data Spending on Next Period Forecast Errors',
             coef_order = c(1,7:10,2:6),
             coef_names = interact_coef_names, 
             output_path = paste0(de_dummy(finished_output_dir), label, '.pdf'),
             notes = 'Two-Way Robust standard errors clustered at the fiscal year x firm level.',
             note_width = 1,
             custom_rows = list("",c('Industry / Firm Fe', rep(c('industry', 'firm'), 3))),
             custom_row_placement = c(18,30),
             rescale_factor = 1, make_pdf = T, make_tex = F)
             



