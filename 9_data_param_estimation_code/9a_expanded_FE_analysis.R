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

# import IBES data --------------------------------------------------------
importing_IBES = F
if (importing_IBES){
#### FORECAST DATA 
#https://wrds-www.wharton.upenn.edu/pages/get-data/lseg-ibes/ibes-guidance/detail-history/detail/
IBES_forecast_data =  wrds_query("SELECT ticker AS ibes_ticker, measure, curr, units, anndats AS fcst_ann_date, prd_yr, prd_mon, val_1, val_2, 
                                 mean_at_date AS analyst_forecast_mean FROM ibes.det_guidance 
                                 WHERE pdicity = 'ANN' AND usfirm = 1 AND val_1 IS NOT NULL AND curr = 'USD' AND measure = 'SAL'") %>% 
  
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
IBES_actual = import_file("1) data/11_parameter_calibration/raw/1_IBES_actuals_raw.csv") %>% rename_with(~tolower(.)) %>% 
  .[, pends := as.Date(pends)] %>% .[, `:=`(prd_yr = year(pends), prd_mon = month(pends))]  %>%
  rename(ibes_ticker= ticker, realized_value = value) %>% 
  distinct(., ibes_ticker, prd_yr, prd_mon, measure, .keep_all = T) %>% 
  select('ibes_ticker','pends', 'prd_yr', 'prd_mon', 'measure', 'realized_value') 

IBES_initial_combined = merge(IBES_forecast_data, IBES_actual,  by = c('ibes_ticker', 'prd_yr', 'prd_mon', 'measure')) %>% 
  .[, forecast_horizon := as.numeric(pends - fcst_ann_date)] %>% 
  .[, fiscal_year := ifelse(prd_mon >5, prd_yr, prd_yr - 1)] %>%
  .[!is.na(ibes_ticker)] %>% 
  .[, pstart := pends %m-% years(1) + days(1)] %>% 
  select(ibes_ticker, fiscal_year,pstart, pends, measure, curr, units, con_fil(., 'forecast'), realized_value)

  write_parquet(IBES_initial_combined, "1) data/11_parameter_calibration/raw/2_IBES_forecast_plus_actuals_plus_birth_raw.parquet")
}

# Import compustat data --------------------------------------------------------
importing_compustat = F
if (importing_compustat){
  
  ##### https://wrds-www.wharton.upenn.edu/pages/get-data/linking-suite-wrds/ibes-crsp-link/
  ##### https://wrds-www.wharton.upenn.edu/pages/get-data/center-research-security-prices-crsp/annual-update/crspcompustat-merged/fundamentals-annual/
  compustat_dta = merge(
    
    ### crsp to ibes data 
    import_file('1) data/11_parameter_calibration/raw/3_crsp_ibes_crosswalk.csv') %>%
      rename(ibes_ticker = TICKER) %>% distinct(ibes_ticker, PERMNO) %>% remove_if_NA('PERMNO', 'ibes_ticker'),
    
    ### crsp x compustat data 
    import_file('1) data/11_parameter_calibration/raw/4_compustat_data_raw.csv') %>% 
      rename(compustat_capital = ppent, compustat_rev = sale, compustat_investment = capx,  fiscal_yr_end_month = fyr,
             compustat_emp = emp, compustat_wages = xlr, PERMNO = LPERMNO, fiscal_year = fyear) %>% 
      remove_if_NA('PERMNO') %>% 
      .[, naics := sprintf("%06d", as.integer(naics))] %>% .[, naics_2d := substr(naics,1,2)] %>% 
      .[, fiscal_yr_enddate := ymd(paste(fiscal_year, fiscal_yr_end_month, 1, sep = "-")) %m+% months(1) - days(1)] %>% 
      .[, fiscal_yr_startdate := fiscal_yr_enddate %m-% years(1) + days(1)] %>% 
      
      ### discard obs with missing or neg rev, wages, or emp
      .[!(is.na(compustat_emp) | compustat_emp <=0) ] %>% 
      .[!(is.na(compustat_rev) | compustat_rev <=0) ] %>% 
      .[!(is.na(compustat_capital) | compustat_capital <=0)] %>%
      
      ## select interest vars  
      select(PERMNO, cusip, naics, fiscal_year,fiscal_yr_startdate, fiscal_yr_enddate, compustat_rev,compustat_capital, compustat_investment, compustat_emp, compustat_wages, ipodate) %>% 
      distinct(PERMNO, fiscal_year, .keep_all = T),
    by = 'PERMNO') %>% .[,finan_or_util := substr(naics, 1, 2) %in% c("52", "22")] %>% 
    .[, count := .N, by = .(PERMNO, fiscal_year)] %>% .[count ==1] %>% .[,count := NULL] 
  
  write_parquet(compustat_dta, '1) data/11_parameter_calibration/raw/5_compustat_data_initial_processed.parquet')
}

# combine and clean financial data -----------------------------------------------------------------
combining_financials = F
if (combining_financials){
IBES = import_file("1) data/11_parameter_calibration/raw/2_IBES_forecast_plus_actuals_plus_birth_raw.parquet")
compustat = import_file('1) data/11_parameter_calibration/raw/5_compustat_data_initial_processed.parquet')
vars_to_deflate = c("analyst_forecast_mean","forecast", "forecast_lb", "forecast_ub", "forecast_range", "realized_value",
                    "compustat_rev", "compustat_capital", "compustat_investment", "compustat_wages")

### DEFLATOR DATA 
getSymbols("GDPDEF", src = "FRED")
deflator_dta = as.data.table(GDPDEF) %>% rename_with(~c('date', 'gdp_def')) %>% 
  complete(date = seq(min(date), max(date), by = "day"))  %>% 
  fill(gdp_def, .direction = "down") %>% as.data.table()
base_deflator_val = deflator_dta[date == as.Date('2025-01-01')][['gdp_def']]
deflator_dta[, gdp_def := base_deflator_val/ gdp_def]

### MERGE AND CLEAN 
financial_dta = merge(compustat,IBES, all = T,  by = c('ibes_ticker', 'fiscal_year')) %>% 
  .[is.na(fiscal_yr_enddate),  fiscal_yr_enddate := pends] %>% 
  .[is.na(fiscal_yr_startdate), fiscal_yr_startdate := pstart] %>% 
  .[!finan_or_util == T] %>% 
  .[, first_or_no_forecast := is.na(forecast) | first_forecast == T] %>% 
  merge(deflator_dta, by.x = 'fiscal_yr_enddate', by.y = 'date', all.x = T) %>% 
  .[, (vars_to_deflate) := lapply(.SD, function(x) x*gdp_def), .SDcols = vars_to_deflate] %>% 
  .[, (vars_to_deflate) := lapply(.SD, function(x) x*1e6), .SDcols = vars_to_deflate] %>% 
  
  ## Generate the forecast errors 
  .[, `:=`(analyst_forecast_error = realized_value - analyst_forecast_mean,
           firm_forecast_error = realized_value - forecast)] %>% 
  mutate(across(con_fil(.,'forecast_error'), ~abs(.), .names = 'abs_{col}')) 

  ### generate misallocation metrics 
  alpha = 2/3
  financial_dta = financial_dta[, `:=`(
    tfpr = compustat_rev / (compustat_emp^alpha* compustat_capital^(1-alpha)),
    mrpl = compustat_rev /compustat_emp,
    mrpk = compustat_rev /compustat_capital)]
  
  ### # IBES birth (and industry group) match + age vars 
  ibes_birth_ages = wrds_query("SELECT item6003 AS firm_name, item6038 AS ibes_ticker, item6008 AS isin, item6011 AS industry_group,
                            item18272 AS birth_date, item18273 AS incorporation_date FROM trws.wrds_ws_company WHERE item6038 is not NULL") %>% 
    distinct(ibes_ticker, .keep_all = T)
  financial_dta = merge(financial_dta, ibes_birth_ages, all.x = T, by = 'ibes_ticker') %>% 
    .[,birth_year := year(case_when(!is.na(birth_date) ~ birth_date,
                                    !is.na(incorporation_date) ~ incorporation_date,
                                    !is.na(ipodate) ~ ipodate))] %>% 
    .[fiscal_year >= birth_year,age := fiscal_year - birth_year] %>% 
    .[,young := age <= 5]

### ADD THE RCID DATA 
rcid_matching =  wrds_query("SELECT rcid,child_rcid, ultimate_parent_rcid, year_founded, isin, cusip  FROM revelio.company_mapping WHERE isin is not NULL or cusip is not NULL")
financial_dta = financial_dta %>% merge(rcid_matching[!is.na(cusip),c('cusip', 'rcid')] %>% distinct(cusip, .keep_all = T), by = 'cusip', all.x = T) %>% 
  merge(rcid_matching[!is.na(isin),c('rcid', 'isin')] %>% distinct(isin, .keep_all = T) %>% rename(rcid_isin = rcid), by= 'isin', all.x = T) %>% 
  .[is.na(rcid), rcid := rcid_isin] %>% .[,rcid_isin := NULL]  %>%
  merge(rcid_matching[!is.na(rcid),c('rcid','child_rcid', 'ultimate_parent_rcid')], all.x = T, by = 'rcid')

  duplicated_rcids = distinct(financial_dta,PERMNO, rcid) %>% drop_na() %>% .[,.(count = .N), by = rcid] %>% .[count >1] %>% pull(rcid) 
  financial_dta = financial_dta[rcid %in% duplicated_rcids, rcid := NA] %>% .[, ultimate_parent :=  ultimate_parent_rcid == rcid] %>% 
  .[,is_ultimate_parent := rcid == ultimate_parent_rcid & !is.na(rcid)]
write_parquet(financial_dta, "1) data/11_parameter_calibration/clean/1_cleaned_financial_dta.parquet")
}  


# import role data of parent company --------------------------------------
importing_parent_role = F
if(importing_parent_role){
  
  financial_dta = import_file("1) data/11_parameter_calibration/clean/1_cleaned_financial_dta.parquet")
  
  ### GENERATE THE ROLE DATA 
  rcid_list = setdiff(unique(financial_dta[is_ultimate_parent == T][['rcid']]), NA)
  
  for (i in (1:length(rcid_list))){
    output_file = paste0('1) data/11_parameter_calibration/raw/parent_temp/parent_role_',i,'.csv')
    if (!file.exists(output_file)){
      fwrite(data.frame(h = 'h'), output_file)
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
    WHERE ip.total_compensation IS NOT NULL AND ip.startdate IS NOT NULL", in_clause)) %>%
        select(-rcid) %>% rename(rcid = parent_rcid) %>% 
        .[, `:=`(college = highest_degree %in% c('Master', 'Bachelor', 'MBA', 'Doctor'),
                 college_total = !is.na(highest_degree))] %>% 
        .[!is.na(weight) & !is.na(total_compensation)] %>% 
        .[, comp := total_compensation * weight] %>% 
        merge(import_file('1) data/7_revelio_data/b_processed_data/linkedin/revelio_role_dict.csv'), by = 'role_k1500')
      
      output_table = rbindlist(lapply(2000:2024, function(yr){
        sub_temp = lapply(c(0,1), function(lag){
          sub_temp = financial_dta[rcid == rcid_list[i] & fiscal_year == yr] %>% 
            distinct(rcid, fiscal_year, .keep_all = T) %>% 
            .[,c('rcid', 'fiscal_year', 'fiscal_yr_startdate', 'fiscal_yr_enddate')] %>%
            .[, `:=`(fiscal_yr_startdate= fiscal_yr_startdate %m-% years(lag), fiscal_yr_enddate= fiscal_yr_enddate %m-% years(lag))] %>% 
            merge(temp, by = 'rcid') %>% 
            .[startdate <= fiscal_yr_enddate & (enddate >= fiscal_yr_startdate | is.na(enddate))]  %>% 
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
  write_parquet(parent_role_dta,'1) data/11_parameter_calibration/raw/6_parent_firm_role_dta.parquet') 
}

# import role data of subsidiaries  ---------------------------------------
importing_subsidiary_role = T
if(importing_subsidiary_role){
  financial_dta = import_file("1) data/11_parameter_calibration/clean/1_cleaned_financial_dta.parquet")
  rcid_matching =  wrds_query("SELECT rcid,child_rcid, ultimate_parent_rcid, year_founded, isin, cusip  FROM revelio.company_mapping WHERE isin is not NULL or cusip is not NULL") 
  
  ### OBTAIN THE INFORMATION ON EACH FIRM'S SUBSIDIARIES 
  remaining_subsids = Inf; i = 1
  all_subsids =financial_dta %>% distinct(rcid, .keep_all =T) %>%
    .[!is_ultimate_parent ==T] %>% 
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
 
  ### FOR EACH RCID OBTAIN THE INFO FROM ALL SUBSIDS 
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
        merge(all_subsids[rcid %in%  rcid_list[[i]]], by = 'child_rcid', allow.cartesian = T) 
      
      output_table = rbindlist(lapply(2000:2024, function(yr){
        sub_temp = lapply(c(0,1), function(lag){
          sub_temp =financial_dta[rcid %in% rcid_segment & fiscal_year == yr] %>% 
            distinct(rcid, fiscal_year, .keep_all = T) %>% 
            .[,c('rcid', 'fiscal_year', 'fiscal_yr_startdate', 'fiscal_yr_enddate')] %>%
            .[, `:=`(fiscal_yr_startdate= fiscal_yr_startdate %m-% years(lag), fiscal_yr_enddate= fiscal_yr_enddate %m-% years(lag))] %>% 
            merge(temp, by = 'rcid') %>% 
            .[startdate <= fiscal_yr_enddate & (enddate >= fiscal_yr_startdate | is.na(enddate))]  %>% 
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
  write_parquet(subsid_role_dta,'1) data/11_parameter_calibration/raw/7_subsid_firm_role_dta.parquet') 
}

# generate combined dataset -------------------------------------------------------------------------
combining_dta = F
if(combining_dta){
role_dta = rbindlist(list(import_file('1) data/11_parameter_calibration/raw/6_parent_firm_role_dta.parquet')))
financial_dta = import_file("1) data/11_parameter_calibration/clean/1_cleaned_financial_dta.parquet") %>% .[,idx := .I]

vars_to_log = c(gpaste(c('', 'parent_'),'comp_', c('data', 'total'), c('', '_lag1')),
                gpaste('abs_',c('analyst', 'firm'), '_forecast_error'), 'forecast_horizon', 'age')

combined_dta = merge(financial_dta, role_dta, all.x = T, by = c('rcid', 'fiscal_year')) %>% 
  
  ### generate an initial value of revenue 
  setorder(., fiscal_year) %>% 
  .[fiscal_year %in% year_range & !is.na(compustat_rev), compustat_rev_init := compustat_rev[1], by= .(PERMNO)] %>%
  
  ## log necessary values 
  mutate(across(con_fil(con_fil(.,  'comp', 'forecast','age'), 'first', 'last', inc = F), ~asinh(.), .names = 'log_{col}')) %>% 
    
  ### restrict to sample range  
  .[(measure == 'SAL' | is.na(measure)) & fiscal_year %in% year_range  & is_ultimate_parent == T] 

### generate a metric for how unusual data spend is 
model_dta =  combined_dta %>% .[first_or_no_forecast == T]
model = feols(model_dta, log_parent_comp_data~ log_parent_comp_total + parent_share_empl_college + asinh(compustat_capital) | naics + fiscal_year)
non_dropped_obs = setdiff(1:nrow(model_dta),-1*model$obs_selection$obsRemoved)
model_dta = model_dta[non_dropped_obs, relative_data_spend := model$residuals]

model = feols(model_dta, log_parent_comp_data~ log_parent_comp_total + parent_share_empl_college + asinh(compustat_capital) | industry_group + fiscal_year)
non_dropped_obs = setdiff(1:nrow(model_dta),-1*model$obs_selection$obsRemoved)
model_dta = model_dta[non_dropped_obs, relative_data_spend_IBES_sample := model$residuals] %>% 
  .[,data_spend_quartile := ntile(relative_data_spend, 4)] %>% 
  .[,data_spend_quartile_yr_to_yr:=  ntile(relative_data_spend, 4), fiscal_year] %>% 
  .[!is.na(relative_data_spend_IBES_sample), data_spend_quartile_IBES_sample := ntile(relative_data_spend_IBES_sample,4)] %>% 
  .[!is.na(relative_data_spend_IBES_sample), data_spend_quartile_IBES_sample_yr_to_yr := ntile(relative_data_spend_IBES_sample,4), fiscal_year] %>% 
  unbalanced_lag(., 'PERMNO', 'fiscal_year',con_fil(.,'quartile'),1) %>% select('PERMNO', 'fiscal_year', con_fil(.,'quartile', 'relative'))
combined_dta = merge(combined_dta, model_dta, by = c('PERMNO', 'fiscal_year'), all.x = T)
write_parquet(combined_dta, "1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet")
}




