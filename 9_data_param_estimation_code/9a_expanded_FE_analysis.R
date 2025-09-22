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
importing_IBES = T
if (importing_IBES){
  
  # generate exchange rate data 
  getSymbols("DEXUSEU", src="FRED"); 
  euro_usd_dta = as.data.table(DEXUSEU) %>%  rename(date = index, euro_usd = DEXUSEU) %>% 
    complete(date = seq(min(date), max(date), by = "day"))  %>% 
    fill(euro_usd, .direction = "down")
  
  #### FORECAST DATA 
  #https://wrds-www.wharton.upenn.edu/pages/get-data/lseg-ibes/ibes-guidance/detail-history/detail/
  french_ibes = unique(wrds_query("select ticker as ibes_ticker from ibes.id_guidance where countrycd = 'EF' or ex_countrycd = 'EF'")) %>% .[,french_ibes := T]
  IBES_forecast_data =  wrds_query("SELECT ticker AS ibes_ticker,usfirm, measure, curr, units, anndats AS fcst_ann_date, prd_yr, prd_mon, val_1, val_2, 
                                 mean_at_date AS analyst_forecast_mean FROM ibes.det_guidance 
                                 WHERE pdicity = 'ANN' AND val_1 IS NOT NULL AND measure = 'SAL' AND curr IN ('USD', 'EUR')")  %>% 
    merge(french_ibes, all.x = T) %>% .[is.na(french_ibes), french_ibes := F] %>%
    #.[(usfirm == 1 & curr == "USD") | (french_ibes == T & !is.na(curr))] %>% 
    
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
    merge(euro_usd_dta, by.x = 'pends', by.y = 'date', all.x = T) %>% 
    .[, forecast_horizon := as.numeric(pends - fcst_ann_date)] %>% 
    .[, fiscal_year := ifelse(prd_mon >5, prd_yr, prd_yr - 1)] %>%
    .[!is.na(ibes_ticker)] %>% 
    .[, pstart := pends %m-% years(1) + days(1)] %>% 
    select(ibes_ticker, fiscal_year,pstart, pends, measure, curr, units, con_fil(., 'forecast'), realized_value,usfirm, french_ibes, euro_usd) %>% 
    
    ## handle weird duplicates due to firms changing their reporting timing 
    merge(distinct(., fiscal_year, ibes_ticker, .keep_all = T) %>% .[, c('fiscal_year', 'ibes_ticker', 'pends')], by = c('fiscal_year', 'ibes_ticker', 'pends'))
 
  ### Deflate and rescale values 
  vars_to_deflate = c("analyst_forecast_mean","forecast", "forecast_lb", "forecast_ub", "forecast_range", "realized_value") 
  IBES_initial_combined = IBES_initial_combined[curr == "EUR", (vars_to_deflate) := lapply(.SD, function(x) x*euro_usd), .SDcols = vars_to_deflate]
  IBES_initial_combined = deflate_values_us(IBES_initial_combined, vars_to_deflate, 'pends') %>% 
    .[, (vars_to_deflate) := lapply(.SD, function(x) x*1e6), .SDcols = vars_to_deflate]  %>% 
    
  ## Generate the forecast errors 
  .[, `:=`(abs_firm_log_forecast_error = abs(asinh(realized_value) - abs(asinh(forecast))),
           abs_analyst_log_forecast_error = abs(asinh(realized_value) - abs(asinh(analyst_forecast_mean))),
           analyst_forecast_error = realized_value - analyst_forecast_mean,
           firm_forecast_error = realized_value - forecast)] %>% 
  mutate(across(con_fil(con_fil(.,'forecast_error'),'abs', inc = F), ~abs(.), .names = 'abs_{col}')) %>% 
  mutate(across(con_fil(con_fil(.,'abs'),'log', inc = F), ~./ realized_value, .names = 'pct_{col}')) 
  
  write_parquet(IBES_initial_combined, "1) data/11_parameter_calibration/raw/2_IBES_forecast_plus_actuals_plus_birth_raw.parquet")
}
# Import compustat data --------------------------------------------------------
importing_compustat = F
if (importing_compustat){


##### https://wrds-www.wharton.upenn.edu/pages/get-data/linking-suite-wrds/ibes-crsp-link/
ibes_crsp_link = import_file('1) data/11_parameter_calibration/raw/3_crsp_ibes_crosswalk.csv') %>%
  rename(ibes_ticker = TICKER, permno = PERMNO) %>% remove_if_NA('permno', 'ibes_ticker') %>% select(-SCORE, NCUSIP) %>% 
  .[is.na(edate), edate := as.IDate("9999-12-31")] %>% .[,`:=`(sdate = as.IDate(sdate), edate = as.IDate(edate))] %>% 
  setkey(., permno, sdate, edate)

##### Import compustat x crsp data https://wrds-www.wharton.upenn.edu/users/saved-query/6357767/
compustat_dta =  import_file('1) data/11_parameter_calibration/raw/4_compustat_data_raw.csv') %>% rename(
  compustat_capital = ppent,
  compustat_rev = sale,
  compustat_intangibles_gdwl = gdwl,
  compustat_intangibles_other = intano,
  compustat_total_assets = at,
  compustat_intangible_assets = intan,
  compustat_rnd = xrd,
  compustat_inventories = invt,
  compustat_investment = capx,  
  fiscal_yr_end_month = fyr,
  compustat_emp = emp, 
  compustat_wages = xlr,
  permno = LPERMNO,
  fiscal_year = fyear,
  gvkey = GVKEY) %>% 
  .[,naics := {d <- str_replace_all(as.character(naics), "[^0-9]", ""); d[nchar(d) == 0] <- NA_character_; str_pad(d, 6, pad = "0")}] %>% 
  .[, naics_2d := substr(naics,1,2)] %>% 
  .[, fiscal_yr_enddate := ymd(paste(fiscal_year, fiscal_yr_end_month, 1, sep = "-")) %m+% months(1) - days(1)] %>% 
  .[, fiscal_yr_startdate := fiscal_yr_enddate %m-% years(1) + days(1)] %>% 
  
  ### discard obs with missing or neg emp, rev, or capital
  .[!(is.na(compustat_emp) | compustat_emp <=0) ] %>% 
  .[!(is.na(compustat_rev) | compustat_rev <=0) ] %>% 
  .[!(is.na(compustat_capital) | compustat_capital <0)] %>%
  
  ## select interest vars  
  select(permno,gvkey, cusip, naics, naics_2d, fiscal_year,fiscal_yr_startdate, fiscal_yr_enddate, fiscal_yr_end_month, 
         con_fil(., 'compustat'), ipodate,datadate) %>% 
  distinct(permno, fiscal_year, .keep_all = T) 

  ## deflate nominal values and convert from millions to actual 
  vars_to_deflate =  setdiff(con_fil(compustat_dta, 'compustat'), "compustat_emp")
  compustat_dta = deflate_values_us(compustat_dta, vars_to_deflate, 'fiscal_yr_enddate') %>% 
  .[, (vars_to_deflate) := lapply(.SD, function(x) x*1e6), .SDcols = vars_to_deflate] %>% 
  
  ## generate the rolling variance of log sales 
  .[, log_compustat_rev := asinh(compustat_rev)] %>% 
  unbalanced_lag(., 'permno', 'fiscal_year', 'log_compustat_rev', 1:5) %>% 
  .[, log_compustat_rev_var_prior5 := apply(.SD, 1, var, na.rm = TRUE), .SDcols = paste0('log_compustat_rev_lag', 1:5)] %>% 
  .[, log_compustat_rev_var_prior5_strict := apply(.SD, 1, var), .SDcols = paste0('log_compustat_rev_lag', 1:5)] %>% 
  .[, log_compustat_rev_var_prior3 := apply(.SD, 1, var, na.rm = TRUE), .SDcols = paste0('log_compustat_rev_lag', 1:3)] %>% 
  .[, log_compustat_rev_var_prior_3_strict := apply(.SD, 1, var), .SDcols = paste0('log_compustat_rev_lag', 1:3)] %>% 
  select(-all_of(con_fil(con_fil(., 'log_compustat_rev'),'prior', inc = F))) %>% 
  
  ## generate additional variables for analysis 
    .[, compustat_rnd_to_rev_ratio := compustat_rnd / compustat_rev] %>% 
    .[is.na(compustat_intangibles_other) & !is.na(compustat_intangible_assets) & !is.na( compustat_intangibles_gdwl), compustat_intangibles_other := compustat_intangible_assets - compustat_intangibles_gdwl] %>%
    .[, weird_intangibles := compustat_intangible_assets > compustat_total_assets | compustat_total_assets <=0] %>% 
    .[weird_intangibles== F, `:=`(compustat_intangible_asset_fraction = compustat_intangible_assets/ compustat_total_assets,
                               compustat_other_intangible_asset_fraction = compustat_intangibles_other / compustat_total_assets,
                               compustat_goodwill_asset_fraction = compustat_intangibles_gdwl / compustat_total_assets)] %>% 
    .[, weird_intangibles := NULL] %>% 
    .[, compustat_inventory_to_rev_ratio := compustat_inventories/ compustat_rev] %>% 
    .[,finan_or_util := naics_2d %in% c("52", "22")] #flag finance and utilities for removal 

##### IMPORT THE SEGMENT DATA (https://wrds-www.wharton.upenn.edu/users/saved-query/6357705/)
seg_dta = fread("1) data/11_parameter_calibration/raw/8_compustat_segment_dta.csv") %>% 
  setnames(., tolower(names(.))) %>% .[ stype %in% c('OPSEG', 'GEOSEG')] %>% 
  .[, datadate := as.IDate(datadate)] %>% 
  setkey(., gvkey, datadate)

seg_dta = compustat_dta[!is.na(fiscal_yr_end_month) & !is.na(datadate),.(gvkey, datadate, fiscal_yr_end_month) ] %>%
  setkey(., gvkey, datadate) %>% 
  .[seg_dta, roll = T] %>% 
  .[,`:=`(cal_month = month(datadate), cal_year = year(datadate)) ] %>% 
  .[, fiscal_year := ifelse(cal_month >= fiscal_yr_end_month, cal_year, cal_year + 1)] %>% 
  .[order(gvkey, fiscal_year, datadate, srcdate)]  %>% 
  .[, latest_filing := (srcdate == srcdate[.N] & datadate == datadate[.N]), by = .(gvkey,stype, fiscal_year)] %>% 
  .[latest_filing == T]  %>% .[!is.na(fiscal_year)]

seg_export_dta = seg_dta[stype == 'GEOSEG'] %>%
  .[, .(compustat_dom_rev = NA_sum(sales*(geotp == 2)),
       compustat_export1 = NA_sum(salexg),
       compustat_export2 = NA_sum(sales*(geotp == 3))), by = .(gvkey, datadate, fiscal_year)] %>%
  .[, compustat_export_rev := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('compustat_export', 1:2)] %>% 
  deflate_values_us(., con_fil(.,'rev'), 'datadate') %>% 
  .[, c(paste0('compustat_export', 1:2), 'datadate') := NULL] %>% 
  .[, con_fil(.,'rev') := lapply(.SD, function(x) x*1e6), .SDcols = con_fil(.,'rev')] 

seg_opseg_dta = seg_dta[stype == "OPSEG", .(compustat_num_segments = uniqueN(sid)), by = .(gvkey,fiscal_year)]
compustat_combined = compustat_dta %>% 
  merge(seg_opseg_dta, all.x = T, by = c('gvkey', 'fiscal_year'))  %>% 
  merge(seg_export_dta, all.x = T, by = c('gvkey', 'fiscal_year'))

#### Merge to IBES 
ibes_crsp_link = ibes_crsp_link[compustat_combined, on = .(permno, sdate <= datadate, edate >= datadate),nomatch = 0L] %>% 
  .[, .(permno, fiscal_year, ibes_ticker)]
compustat_combined = merge(compustat_combined,ibes_crsp_link, by = c('permno','fiscal_year'), all.x = T) 

### keep only unique permno x year obs as well as unique gvkey x year observations
compustat_combined =  compustat_combined[, count := .N, by = .(permno, fiscal_year)] %>% .[count ==1] %>% .[,count := NULL] 
compustat_combined =  compustat_combined[, count := .N, by = .(gvkey, fiscal_year)] %>% .[count ==1] %>% .[,count := NULL] 
compustat_combined = compustat_combined[,  count := ifelse(is.na(ibes_ticker),1, .N), by = .(ibes_ticker, fiscal_year)] %>% .[count == 1] %>% .[,count := NULL]

## export  
write_parquet(compustat_combined, '1) data/11_parameter_calibration/raw/5_compustat_data_initial_processed.parquet')
}

# import LSEG MA data -------------------------------------------------------------
importing_MA_data = F
if (importing_MA_data){
#https://wrds-www.wharton.upenn.edu/pages/get-data/lseg/sdc/mergers-acquisitions/mergers-and-acquisitions/?saved_query=6367077
MA_data = wrds_query(
"SELECT DISTINCT ON (master_deal_no) -- one value per deal 
master_cusip as target_cusip, dateann as deal_announce_date, 
dateeff as deal_effective_date, UPPER(TRIM(acusip)) as cusip_acquirer, deal_value, master_deal_no as deal_num 
FROM tr_sdc_ma.wrds_ma_details 
WHERE dateann BETWEEN '2000-01-01'::date AND '2025-09-12'::date 
AND  status = 'Completed' 
AND (pctown IS NULL OR pctown > 50) -- they need to end up as majority owner 
AND (form IS NULL OR form in ('Merger', 'Acquisition', 'Acq. Maj. Int.')) 
ORDER BY master_deal_no, dateann DESC -- ensures the latest data only 
") %>% .[is.na(deal_effective_date), deal_effective_date := deal_announce_date] %>% 
  deflate_values_us(., 'deal_value', 'deal_effective_date') %>% 
  .[,deal_value := deal_value*1e6] %>% 
  .[, deal_effective_date := as.IDate(deal_effective_date)] %>% .[,deal_date := deal_effective_date]

MA_data = import_file('1) data/11_parameter_calibration/raw/3_crsp_ibes_crosswalk.csv') %>%
  rename_with(~tolower(.)) %>%
  remove_if_NA('permno') %>% 
  .[,cusip_acquirer := substr(ncusip, 1,6)] %>% .[,c('permno', 'cusip_acquirer', 'sdate', 'edate')] %>% 
  .[MA_data, on = .(cusip_acquirer, sdate <= deal_effective_date, edate >= deal_effective_date),
            allow.cartesian = T, nomatch = 0L] %>%
  .[order(deal_num, sdate, edate)] %>%
  .[, .SD[.N], by = deal_num] %>% 
  .[, c('permno', 'deal_date', 'deal_value')]

MA_data = import_file('1) data/11_parameter_calibration/raw/5_compustat_data_initial_processed.parquet') %>% 
  .[, c('permno','gvkey', 'fiscal_year', 'fiscal_yr_startdate', 'fiscal_yr_enddate')] %>% 
  .[!is.na(permno)] %>% 
  .[MA_data, on = .(permno, fiscal_yr_startdate <= deal_date, fiscal_yr_enddate >= deal_date),
    nomatch = 0L] %>% 
  .[!is.na(gvkey), .(fy_MA_deal_value = NA_sum(deal_value), fy_MA_deals = .N), by = .(fiscal_year, gvkey)]

write_parquet(MA_data, '1) data/11_parameter_calibration/raw/9_LSEG_merger_dta.parquet')
}

# combine and clean financial data -----------------------------------------------------------------
combining_financials = T
if (combining_financials){
IBES = import_file("1) data/11_parameter_calibration/raw/2_IBES_forecast_plus_actuals_plus_birth_raw.parquet")
compustat = import_file('1) data/11_parameter_calibration/raw/5_compustat_data_initial_processed.parquet')
ma_dta = import_file('1) data/11_parameter_calibration/raw/9_LSEG_merger_dta.parquet')

### MERGE AND CLEAN 
financial_dta = merge(compustat,IBES, all = T,  by = c('ibes_ticker', 'fiscal_year')) %>% 
  .[is.na(fiscal_yr_enddate),  fiscal_yr_enddate := pends] %>% 
  .[is.na(fiscal_yr_startdate), fiscal_yr_startdate := pstart] %>% 
  .[, c('pstart', 'pends') := NULL ] %>% 
  .[finan_or_util == F | is.na(finan_or_util)] %>% 
  .[,first_or_no_forecast := is.na(first_forecast) | first_forecast == T] %>% 
  
  merge(ma_dta, all.x = T, by = c('fiscal_year', 'gvkey')) %>% 
  .[is.na(fy_MA_deals), fy_MA_deals := 0] %>% 
  .[fy_MA_deals ==0, fy_MA_deal_value := 0] %>% 
  .[,log_fy_MA_deal_value := asinh(fy_MA_deal_value)]
  

  ### # IBES birth (and industry group) match + age vars 
  ibes_birth_ages = wrds_query("SELECT item6003 AS firm_name, item6038 AS ibes_ticker, item6008 AS isin, item6011 AS industry_group, 
                            item18272 AS birth_date, item18273 AS incorporation_date FROM trws.wrds_ws_company WHERE item6038 is not NULL") %>% 
    distinct(ibes_ticker, .keep_all = T) %>% remove_if_NA('ibes_ticker')
  
  financial_dta = merge(financial_dta, ibes_birth_ages, all.x = T, by = 'ibes_ticker') %>% 
    .[,birth_year := year(case_when(!is.na(birth_date) ~ birth_date,
                                    !is.na(incorporation_date) ~ incorporation_date,
                                    !is.na(ipodate) ~ ipodate))] %>% 
    .[fiscal_year >= birth_year,age := fiscal_year - birth_year] 

### ADD THE RCID DATA 
rcid_matching =  wrds_query("SELECT rcid,child_rcid, ultimate_parent_rcid, year_founded, isin, cusip,
                            hq_country as revel_country, year_founded as revel_birth_year 
                            FROM revelio.company_mapping WHERE isin is not NULL or cusip is not NULL")
financial_dta = financial_dta %>% merge(rcid_matching[!is.na(cusip),c('cusip', 'rcid')] %>% distinct(cusip, .keep_all = T), by = 'cusip', all.x = T) %>% 
  merge(rcid_matching[!is.na(isin),c('rcid', 'isin')] %>% distinct(isin, .keep_all = T) %>% rename(rcid_isin = rcid), by= 'isin', all.x = T) %>% 
  .[is.na(rcid), rcid := rcid_isin] %>% .[,rcid_isin := NULL]  %>%
  merge(rcid_matching[!is.na(rcid),c('rcid','child_rcid', 'ultimate_parent_rcid', 'revel_country', 'revel_birth_year')], all.x = T, by = 'rcid')

  duplicated_rcids = distinct(financial_dta,permno, rcid) %>% drop_na() %>% .[,.(count = .N), by = rcid] %>% .[count >1] %>% pull(rcid) 
  financial_dta = financial_dta[rcid %in% duplicated_rcids, rcid := NA] %>% .[, ultimate_parent :=  ultimate_parent_rcid == rcid] %>% 
  .[,is_ultimate_parent := rcid == ultimate_parent_rcid & !is.na(rcid)]
write_parquet(financial_dta, "1) data/11_parameter_calibration/clean/1_cleaned_financial_dta.parquet")
}  


# import role data of rcid only ------------------------------------------------
importing_rcid_role = F
if(importing_rcid_role){

  financial_dta = import_file("1) data/11_parameter_calibration/clean/1_cleaned_financial_dta.parquet")
  
  ### GENERATE THE ROLE DATA 
  rcid_list = setdiff(unique(financial_dta[['rcid']]), NA)
  
  for (c_rcid in rcid_list){
    output_file = paste0('1) data/11_parameter_calibration/raw/temp/role_',c_rcid,'.csv')
    if (!file.exists(output_file)){
      fwrite(data.frame(h = 'h'), output_file)
      print(round(100*which(rcid_list == c_rcid) / length(rcid_list),2))
      parent_list = c_rcid
      in_clause <- paste(sprintf("'%s'", gsub("'", "''", parent_list)), collapse = ",")
      
      temp = wrds_query(sprintf("
      SELECT ip.rcid as rcid, ip.startdate, ip.enddate, ip.role_k1500, ip.total_compensation, ip.weight, 
      iu.prestige, iu.highest_degree 
      FROM revelio.individual_positions ip 
      LEFT JOIN revelio.individual_user iu ON ip.user_id = iu.user_id
      WHERE ip.total_compensation IS NOT NULL
      AND ip.weight IS NOT NULL 
      AND ip.startdate IS NOT NULL 
      AND ip.rcid IN (%s)", in_clause)) %>%
        .[, `:=`(college = highest_degree %in% c('Master', 'Bachelor', 'MBA', 'Doctor'),
                 college_total = !is.na(highest_degree))] %>% 
        .[!is.na(weight) & !is.na(total_compensation)] %>% 
        .[, comp := total_compensation * weight] %>% 
        merge(import_file('1) data/7_revelio_data/b_processed_data/linkedin/revelio_role_dict.csv'), by = 'role_k1500')
      
      output_table = rbindlist(lapply(2000:2024, function(yr){
        sub_temp = lapply(c(0,1), function(lag){
          sub_temp = financial_dta[rcid == c_rcid & fiscal_year == yr] %>% 
            distinct(rcid, fiscal_year, .keep_all = T) %>% 
            .[,c('rcid', 'fiscal_year', 'fiscal_yr_startdate', 'fiscal_yr_enddate')] %>%
            .[, `:=`(fiscal_yr_startdate= fiscal_yr_startdate %m-% years(lag), fiscal_yr_enddate= fiscal_yr_enddate %m-% years(lag))] %>% 
            merge(temp, by = 'rcid') %>% 
            .[startdate <= fiscal_yr_enddate & (enddate >= fiscal_yr_startdate | is.na(enddate))]  %>% 
            .[, .(comp_total = sum(comp), empl_total = sum(weight),
                  comp_data = sum(comp*data), empl_data = sum(weight*data),
                  comp_rnd = sum(comp*rnd), comp_stem = sum(comp*stem), comp_non_data_rnd = sum(comp*non_data_rnd),
                  share_comp_data_analyst = sum(comp*data_analyst)/ sum(comp*data),
                  avg_prestige = NA_mean(prestige),
                  share_empl_college = NA_sum(weight*college)/ NA_sum(weight*college_total)), by = .(rcid, fiscal_year)] 
          if (lag == 1) sub_temp = sub_temp %>% rename_with(.cols = setdiff(names(sub_temp), c('rcid','fiscal_year')), ~paste0(.,'_lag1'))
          return(sub_temp)
        })
        sub_temp = merge(sub_temp[[1]], sub_temp[[2]], by = c('rcid', 'fiscal_year'), all = T)
      })) 
      fwrite(output_table, output_file)
    }
  }
  file_list = list.files(path = '1) data/11_parameter_calibration/raw/temp', full.names = T)
  rcid_role_dta = rbindlist(lapply(file_list,import_file), use.names = T, fill = T)
  write_parquet(rcid_role_dta,'1) data/11_parameter_calibration/raw/10_firm_role_dta.parquet') 
}


# import role data of parent company --------------------------------------
importing_parent_role = T
if(importing_parent_role){
  
  financial_dta = import_file("1) data/11_parameter_calibration/clean/1_cleaned_financial_dta.parquet")

  ### GENERATE THE ROLE DATA 
  rcid_list = setdiff(unique(financial_dta[is_ultimate_parent == T][['rcid']]), NA)
  
  for (c_rcid in rcid_list){
    output_file = paste0('1) data/11_parameter_calibration/raw/parent_temp/parent_role_',c_rcid,'.csv')
    if (!file.exists(output_file)){
      fwrite(data.frame(h = 'h'), output_file)
      print(round(100*which(rcid_list == c_rcid) / length(rcid_list),2))
      parent_list = c_rcid
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
          sub_temp = financial_dta[rcid == c_rcid & fiscal_year == yr] %>% 
            distinct(rcid, fiscal_year, .keep_all = T) %>% 
            .[,c('rcid', 'fiscal_year', 'fiscal_yr_startdate', 'fiscal_yr_enddate')] %>%
            .[, `:=`(fiscal_yr_startdate= fiscal_yr_startdate %m-% years(lag), fiscal_yr_enddate= fiscal_yr_enddate %m-% years(lag))] %>% 
            merge(temp, by = 'rcid') %>% 
            .[startdate <= fiscal_yr_enddate & (enddate >= fiscal_yr_startdate | is.na(enddate))]  %>% 
            .[, .(comp_total = sum(comp), empl_total = sum(weight),
                  comp_data = sum(comp*data), empl_data = sum(weight*data),
                  comp_rnd = sum(comp*rnd), comp_stem = sum(comp*stem), comp_non_data_rnd = sum(comp*non_data_rnd),
                  share_comp_data_analyst = sum(comp*data_analyst)/ sum(comp*data),
                  avg_prestige = NA_mean(prestige),
                  share_empl_college = NA_sum(weight*college)/ NA_sum(weight*college_total)), by = .(rcid, fiscal_year)] 
          if (lag == 1) sub_temp = sub_temp %>% rename_with(.cols = setdiff(names(sub_temp), c('rcid','fiscal_year')), ~paste0(.,'_lag1'))
          return(sub_temp)
        })
        sub_temp = merge(sub_temp[[1]], sub_temp[[2]], by = c('rcid', 'fiscal_year'), all = T)
      })) %>% 
        rename_with(.cols = con_fil(., 'rcid', 'fiscal_year', inc = F), ~paste0('parent_', .))
      fwrite(output_table, output_file)
    }
  }
  
  file_list = list.files(path = '1) data/11_parameter_calibration/raw/parent_temp', full.names = T)
  parent_role_dta = rbindlist(lapply(file_list,import_file), use.names = T, fill = T)
  
  ### add data on the extensive margin of first data hiring events 
  add_extensive_margin = function(dta, stub){
  comp = paste0('parent_comp_',stub); 
  lag_comp = paste0(comp, '_lag1');
  cohort = paste0('extensive_hire_cohort_', stub) 
  ever_use = paste0('ever_use_', stub)
  event_study_yr_5y_trim = paste0('event_study_yr_5y_trim_',stub)
  event_study_yr_3y_trim = paste0('event_study_yr_3y_trim_',stub)
  in_event_study = paste0('in_event_study_', stub)
  dta = setorder(dta,rcid,fiscal_year) %>% 
    .[, (ever_use) := NA_max(get(comp)) >0 ] %>% 
    .[, event := get(lag_comp) == 0 & get(comp) >0] %>% 
    .[, (cohort) := NA_min(fiscal_year[event]), by = rcid] %>% 
    .[!is.na(get(cohort)), event_revert := get(comp) == 0 & fiscal_year > get(cohort)] %>% 
    .[, event_revert_yr := NA_min(fiscal_year[event_revert]), by = rcid] %>% 
    mutate(across(con_fil(.,'cohort'), ~replace_na(as.numeric(.),Inf))) %>% 
    .[, event_study_yr := fiscal_year - get(cohort)] %>% 
    .[fiscal_year >= event_revert_yr,  event_study_yr := NA] %>% 
    .[, paste0('event_study_yr_',stub) := event_study_yr] %>%

    
    ## TRIM 
    .[, (event_study_yr_5y_trim) := fifelse(event_study_yr > 5,  5, fifelse(event_study_yr < -5, -5, event_study_yr))] %>% 
    .[, (event_study_yr_3y_trim) := fifelse(event_study_yr > 3,  3, fifelse(event_study_yr < -3, -3, event_study_yr))] %>% 
    .[, (in_event_study) := get(cohort) %in% us_year_range] %>% 
    .[, c('event_study_yr', 'event') := NULL]
  }
  parent_role_dta = add_extensive_margin(parent_role_dta, 'data') 
  
  ### add data on the extensive margin of all data hiring events 
  # parent_role_dta = parent_role_dta %>% 
  #   .[, extensive_data_hire := parent_comp_data_lag1== 0 & parent_comp_data >0 & fiscal_year %in% us_year_range, by = rcid] %>% 
  #   .[extensive_data_hire == T, `:=`(stacked_event_yr = 0, stacked_event_cohort = fiscal_year)] 
  # 
  # for (i in 1:50){
  #   parent_role_dta = unbalanced_lag(parent_role_dta,'rcid', 'fiscal_year', c('stacked_event_yr', 'stacked_event_cohort'), c(-1,1)) %>% 
  #     .[!is.na(stacked_event_yr_lead1) & parent_comp_data == 0, `:=`(stacked_event_yr = stacked_event_yr_lead1 - 1,
  #                                                                    stacked_event_cohort = stacked_event_cohort_lead1) ] %>% 
  #     .[!is.na(stacked_event_yr_lag1) & parent_comp_data > 0 ,  `:=`(stacked_event_yr = stacked_event_yr_lag1 + 1,
  #                                                                    stacked_event_cohort = stacked_event_cohort_lag1)] %>% 
  #     .[, gpaste('stacked_event_',c('yr_', 'cohort_'), c('lead', 'lag'),1) := NULL]
  #   
  #   if (nrow(parent_role_dta[stacked_event_yr %in% c(i, -i)]) == 0) break
  # }
  # parent_role_dta[, `:=`(stacked_event_yr_3yr =case_when(stacked_event_yr < -3 ~-3, stacked_event_yr >3 ~3, T~stacked_event_yr))] %>%
  #               .[, `:=`(stacked_event_yr_5yr =case_when(stacked_event_yr < -5 ~-5, stacked_event_yr >5 ~5,T~stacked_event_yr))]

  ## vars to prior 
  # vars_to_prior = con_fil(parent_role_dta, 'comp', 'prestige', 'college') %>% con_fil(., 'lag', inc = F)
  # parent_role_dta = unbalanced_lag(parent_role_dta, 'rcid', 'fiscal_year', vars_to_prior, 2:4)
  # for (var in vars_to_prior){
  #   parent_role_dta =  parent_role_dta[, paste0(var,"_prior5") := apply(.SD, 1,NA_mean), .SDcols = c(var, paste0(var, "_lag",1:4))] %>%
  #     .[, paste0(var,"_prior3") := apply(.SD, 1,NA_mean), .SDcols = c(var, paste0(var, "_lag",1:2))] 
  # }
  # parent_role_dta[, con_fil(parent_role_dta, paste0('lag',2:4)):=NULL]
  # 
  
  
 write_parquet(parent_role_dta,'1) data/11_parameter_calibration/raw/6_parent_firm_role_dta.parquet') 
}

# import role data of subsidiaries  ---------------------------------------
importing_subsidiary_role = F
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
combining_dta = T
if(combining_dta){
rcid_role_dta =  import_file('1) data/11_parameter_calibration/raw/10_firm_role_dta.parquet') 
financial_dta = import_file("1) data/11_parameter_calibration/clean/1_cleaned_financial_dta.parquet") 

vars_to_log = c(gpaste(c('', 'parent_'),'comp_', c('data', 'total'), c('', '_lag1')),
                gpaste('abs_',c('analyst', 'firm'), '_forecast_error'), 'forecast_horizon', 'age')

combined_dta = merge(financial_dta, parent_role_dta, all.x = T, by = c('rcid', 'fiscal_year')) %>% 
 # merge(rcid_role_dta, all.x = T, by = c('rcid', 'fiscal_year')) %>%
 
  ### generate an initial value of revenue 
  setorder(., fiscal_year) %>% 
  .[fiscal_year %in% us_year_range & !is.na(compustat_rev), compustat_rev_init := compustat_rev[1], by= .(gvkey)] %>%
  
  ## log necessary values 
  mutate(across(con_fil(con_fil(.,  'comp', 'forecast','age'),'log', 'fraction', 'ratio', 'first', 'last', inc = F), ~asinh(.), .names = 'log_{col}')) %>% 
    
  ### restrict to sample rangs
  .[(measure == 'SAL' | is.na(measure)) & fiscal_year %in% us_year_range] 
    
  #### drop top and bottom 1 percent outliers in terms of parent employee count match
   model_dta = combined_dta[first_or_no_forecast == T]
   model = feols(data = model_dta, asinh(parent_empl_total) ~ log_compustat_emp)
   non_dropped_obs = setdiff(1:nrow(model_dta),-1*model$obs_selection$obsRemoved)
   model_dta = model_dta[non_dropped_obs, emp_residual := model$residuals] 
   bottom_top = quantile(model_dta$emp_residual, c(.01, .99), na.rm = T)
   model_dta = model_dta[emp_residual < bottom_top[1] | emp_residual > bottom_top[2], drop_parent := T] %>% 
     .[, empl_inac_weight := 1/ (1 + emp_residual^2)] 
     median_weight = NA_median(model_dta$empl_inac_weight) 
   model_dta = model_dta[is.na(empl_inac_weight), empl_inac_weight := median_weight] %>% 
     select('gvkey','ibes_ticker','fiscal_year', 'drop_parent', 'empl_inac_weight')
   
   combined_dta = merge(combined_dta, model_dta, all.x = T) %>% 
     .[drop_parent == T,con_fil(con_fil(., 'parent'), 'ultimate', 'drop', inc = F) := NA]  

  
   # ### drop top and bottom 1 percent outliers in terms of rcid employee count 
   # model_dta = combined_dta[first_or_no_forecast == T]
   # model = feols(data = model_dta, asinh(empl_total) ~ log_compustat_emp)
   # non_dropped_obs = setdiff(1:nrow(model_dta),-1*model$obs_selection$obsRemoved)
   # model_dta = model_dta[non_dropped_obs, emp_residual := model$residuals] 
   # bottom_top = quantile(model_dta$emp_residual, c(.01, .99), na.rm = T)
   # model_dta = model_dta[emp_residual < bottom_top[1] | emp_residual > bottom_top[2]][,drop_rcid_emp := T][,c('gvkey','ibes_ticker','comp_total','fiscal_year', 'drop_rcid_emp')]
   # combined_dta = merge(combined_dta, model_dta, all.x = T, by = c('gvkey','ibes_ticker','comp_total','fiscal_year')) %>% 
   #   .[drop_rcid_emp == T, con_fil(con_fil(combined_dta, 'comp_', 'emp_'), 'parent', inc = F) := NA]
   
 
   ### Add last set of variables 
   combined_dta = setorder(combined_dta,ibes_ticker, fiscal_year) %>% 
     .[, log_fy_init_analyst_forecast := asinh(analyst_forecast_mean[1]), by = .(ibes_ticker, fiscal_year)] %>% 
     .[fiscal_year >= revel_birth_year,revel_age := 1+ fiscal_year - as.numeric(revel_birth_year)] %>% 
     .[,log_revel_age := log(revel_age)] %>% 
     mutate(across(gpaste('parent_comp_', c('data', 'stem', 'rnd')), ~./parent_comp_total, .names = "share_{col}")) %>% 
     .[, parent_use_data := parent_comp_data > 0 ]
   
   ### generate windsorized_versions of key variables 
   vars_to_windsorize = con_fil(combined_dta,'log', 'share', 'avg')
   combined_dta[,(paste0("w_",vars_to_windsorize)) := lapply(.SD, function(x) windsorize(x, .01,.99)), by= fiscal_year, .SDcols =vars_to_windsorize]
   
   ## trim observations from the bottom and top of the forecast error distribution 
   combined_dta = combined_dta %>% .[ ,`:=`(lfe = abs_firm_log_forecast_error, fe =abs_firm_forecast_error)]
   combined_dta = combined_dta[, log_forecast_out_of_range :=  lfe < quantile(lfe, .01, na.rm= T) | lfe > quantile(lfe, .99, na.rm = T), by = fiscal_year]  
   combined_dta = combined_dta[, forecast_out_of_range :=   fe < quantile(fe, .01, na.rm= T) | fe > quantile(fe, .99, na.rm = T), by = fiscal_year]  %>% 
     .[,c('fe', 'lfe') := NULL]
   
    
write_parquet(combined_dta, "1) data/11_parameter_calibration/clean/2_cleaned_finance_plus_roles.parquet")
}



