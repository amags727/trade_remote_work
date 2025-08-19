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
# import raw data ---------------------------------------------------------
importing_dta = F
if (importing_dta){
#set_params
year_range = 2000:2024

# Connect to WRDS PostgreSQL
wrds <- dbConnect(
  RPostgres::Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  dbname = "wrds",
  user = "am0195",
  password = "BodyBody123!",
  sslmode = "require"
)

## IMPORT FINANCIAL DATA 
## wrds_wd_funda / wrds_ws_company --> https://wrds-www.wharton.upenn.edu/pages/get-data/lseg/worldscope/fundamentals-annual/
## [LSEG IBES guidance --> https://wrds-www.wharton.upenn.edu/pages/get-data/lseg-ibes/ibes-guidance/detail-history/detail/
firm_financial_dta <- dbGetQuery(wrds, "
WITH first_guidance AS (
  SELECT ticker, prd_yr::int AS fiscal_year, measure, curr, units, anndats, val_1,val_2,
    ROW_NUMBER() 
    OVER ( PARTITION BY ticker, prd_yr, measure ORDER BY anndats ASC, actdats ASC) AS rn
    FROM ibes.det_guidance
    WHERE pdicity = 'ANN' and measure = 'SAL'
    )
SELECT c.item6038 AS ibes_ticker,c.item6008 AS isin, f.year_ AS fiscal_year, c.item6011  AS industry_group,
  c.item6026 AS nation, f.item7240 AS net_sales_usd, g.measure, g.curr AS forecast_curr, g.units AS forecast_units, g.anndats, g.val_1, g.val_2,
  f.item5350 AS fiscal_year_enddate, f.item7034 AS data_update, c.item18272 AS birth_date, c.item18273 AS incorporation_date
  FROM trws.wrds_ws_funda f
  JOIN trws.wrds_ws_company c ON c.item6038 = f.item6038
  JOIN first_guidance g ON g.ticker = f.item6038 AND g.fiscal_year = f.year_ AND g.rn = 1
  WHERE f.item6038 IS NOT NULL AND f.freq = 'A'") %>% as.data.table() %>% 
  .[is.na(fiscal_year_enddate), fiscal_year_enddate := as.Date(paste0(fiscal_year, '-12-31'))] %>% 
  .[, fiscal_year_startdate := fiscal_year_enddate %m-% years(1) + days(1)] %>%  
  .[, birth_year := ifelse(is.na(birth_date), year(birth_date), year(incorporation_date))] %>% 
  .[, age := fiscal_year - birth_year] %>% 
  .[!is.na(isin)] %>% 
  .[order(-val_1), .SD[1], by = .(isin, fiscal_year)] # drop duplicates in isin / fiscal year with priority going to non-blank value 1 (50 obs)
write_parquet(firm_financial_dta, '1) data/11_parameter_calibration/raw/firm_financial_dta.parquet')

## IMPORT THE ROLE DATA 
us_french_financials = import_file('1) data/11_parameter_calibration/raw/firm_financial_dta.parquet',
  col_select = c( 'nation', 'fiscal_year','fiscal_year_startdate','fiscal_year_enddate', 'isin')) %>% 
  .[nation %in%  c('UNITED STATES', 'FRANCE')] 
isins <- unique(na.omit(us_french_financials$isin));
isins_list = split(isins, cut(seq_along(isins), 100, labels = FALSE))
role_dict = import_file('1) data/7_revelio_data/b_processed_data/linkedin/revelio_role_dict.csv')
temp_dir = '1) data/11_parameter_calibration/raw/temp'; dir.create(temp_dir)
for (i in 1:length(isins_list)){
  # generate numeric equivs for isin
  isin_temp = data.table(isin =isins_list[[i]]) %>% .[,isin_num := .I]
  
  # import the role data 
  in_clause <- paste(sprintf("'%s'", gsub("'", "''", isins_list[[i]])), collapse = ",")
  print(i / length(isins_list))
  query = sprintf("
  SELECT cm.isin, ip.rcid, ip.startdate, ip.enddate, ip.role_k1500, ip.total_compensation, ip.weight, 
  iu.prestige, iu.highest_degree
  FROM revelio.company_mapping cm
  JOIN revelio.individual_positions ip ON ip.rcid = cm.rcid
  LEFT JOIN revelio.individual_user iu ON ip.user_id = iu.user_id
  WHERE cm.isin IN (%s)", in_clause)
  temp = dbGetQuery(wrds, query) %>% as.data.table() %>% 
    .[, `:=`(college = highest_degree %in% c('Master', 'Bachelor', 'MBA', 'Doctor'),
            college_total = !is.na(highest_degree))] %>% 
    .[!is.na(weight) & !is.na(total_compensation)] %>% 
    .[, comp := total_compensation * weight] %>% 
    merge(isin_temp) %>%
    merge(role_dict, by = 'role_k1500') 
  
  ## collapse to ISIN-year level
  temp = rbindlist(lapply(2000:2024, function(yr){

    sub_temp = us_french_financials[fiscal_year == yr & isin %in% isins_list[[i]]] %>%
      merge(isin_temp) %>% merge(temp, by = 'isin_num') %>% 
      .[startdate <= fiscal_year_startdate  & (enddate >= fiscal_year_enddate | is.na(enddate))]  %>% 
      .[, .(comp_total = sum(comp), empl_total = sum(weight),
            comp_data = sum(comp*data), empl_data = sum(weight*data),
            avg_prestige = NA_mean(prestige),
            share_empl_college = NA_sum(weight*college)/ NA_sum(weight*college_total)), by = isin_num] %>% 
      .[, fiscal_year := yr] %>% merge(isin_temp) %>% select(-isin_num)
    }))
  
  # export the result
  write_parquet(temp, paste0('1) data/11_parameter_calibration/raw/temp/role_',i,'.parquet'))
}
file_list = list.files(path = '1) data/11_parameter_calibration/raw/temp', full.names = T)
role_dta = rbindlist(lapply(file_list,import_file))
write_parquet(role_dta,'1) data/11_parameter_calibration/raw/firm_role_dta.parquet') 
}

# merge and clean -------------------------------------------------------
vars_to_deflate = c('net_sales_usd', 'val_1', 'val_2')
# generate exchange rate data 
getSymbols("DEXUSEU", src="FRED"); 
euro_usd_dta = as.data.table(DEXUSEU) %>%  rename(date = index, euro_usd = DEXUSEU) %>% 
  complete(date = seq(min(date), max(date), by = "day"))  %>% 
  fill(euro_usd, .direction = "down")

getSymbols("GDPDEF", src = "FRED")
deflator_dta = as.data.table(GDPDEF) %>% rename_with(~c('date', 'gdp_def')) %>% 
  complete(date = seq(min(date), max(date), by = "day"))  %>% 
  fill(gdp_def, .direction = "down") %>% as.data.table()
base_deflator_val = deflator_dta[date == as.Date('2025-01-01')][['gdp_def']]
deflator_dta[, gdp_def := base_deflator_val/ gdp_def]


role_data = import_file('1) data/11_parameter_calibration/raw/firm_role_dta.parquet') 
combined_dta = import_file('1) data/11_parameter_calibration/raw/firm_financial_dta.parquet') %>% 
  .[nation %in% c("UNITED STATES", 'FRANCE') & forecast_curr %in% c('USD', 'EUR')] %>% 
  
  merge(euro_usd_dta, all.x = T, by.x = 'fiscal_year_enddate', by.y = 'date') %>% 
  merge(deflator_dta, all.x = T, by.x = 'fiscal_year_enddate', by.y = 'date') %>% 
  
  ## convert all forecast values to nominal mill usd
  .[forecast_curr == 'USD' & forecast_units == 'billions', `:=`(forecast_units = 'millions', val_1 = val_1*1e3, val_2= val_2*1e3 )] %>% 
  .[forecast_curr == 'EUR' & forecast_units == 'millions', `:=`(forecast_curr = 'USD', val_1 = val_1*euro_usd , val_2 =val_2*euro_usd)] %>%
  .[, net_sales_usd := net_sales_usd *1e-6]  %>%
  
  ## deflate all values 
  .[, (vars_to_deflate) := lapply(.SD, function(x) x*gdp_def), .SDcols =vars_to_deflate] %>% 

  ## generate sales estimates from guidance data 
  rename(sales_actual = net_sales_usd ) %>% 
  .[,sales_forecast := ifelse(is.na(val_2), val_1, .5*(val_1+val_2))] %>% 
  .[!is.na(val_2), `:=`(sales_forecast_lb = val_1, sales_forecast_ub = val_2)] %>% 
  .[,sales_forecast_from_range := !is.na(sales_forecast + val_2)] %>% 
    
  ## Generate forecast errors (for observations where the merge doesn't look reallllly strange)
  .[, estimate_ratio := sales_actual / sales_forecast] %>% 
  .[ estimate_ratio > .1 & estimate_ratio < 10, forecast_error := sales_actual - sales_forecast]  %>% 

  ## merge in role data and clean compensation to match other units 
  merge(role_data,all.y = T,  by = c('isin', 'fiscal_year')) %>% 
  .[,`:=`(comp_total = comp_total*1e-6, comp_data = comp_data*1e-6)]
  







