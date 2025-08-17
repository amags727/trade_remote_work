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
  c.item6026 AS nation, f.item7240 AS net_sales_usd, g.measure, g.curr, g.units, g.anndats, g.val_1, g.val_2
  FROM trws.wrds_ws_funda f
  JOIN trws.wrds_ws_company c ON c.item6038 = f.item6038
  JOIN first_guidance g ON g.ticker = f.item6038 AND g.fiscal_year = f.year_ AND g.rn = 1
  WHERE f.item6038 IS NOT NULL") %>% as.data.table()
write_parquet(firm_financial_dta, '1) data/11_parameter_calibration/raw/firm_financial_dta.parquet')


## IMPORT THE ROLE DATA 
isins <- unique(na.omit(firm_financial_dta$isin));
isins_list = split(isins, cut(seq_along(isins), 100, labels = FALSE))
role_dict = import_file('1) data/7_revelio_data/b_processed_data/linkedin/revelio_role_dict.csv')
temp_dir = '1) data/11_parameter_calibration/raw/temp'; dir.create(temp_dir)
for (i in rev(1:length(isins_list))){
in_clause <- paste(sprintf("'%s'", gsub("'", "''", isins_list[[i]])), collapse = ",")
print(i / length(isins_list))
query = sprintf("
SELECT cm.isin, ip.rcid, ip.startdate, ip.enddate, ip.role_k1500, ip.total_compensation, ip.weight
FROM revelio.company_mapping cm
JOIN revelio.individual_positions ip
ON ip.rcid = cm.rcid
WHERE cm.isin IN (%s)
", in_clause)
temp = dbGetQuery(wrds, query) %>% as.data.table()

## collapse down to the isin-yr level
isin_temp = data.table(isin =unique(temp$isin )) %>% .[,isin_num := .I]
temp = merge(temp,isin_temp) %>% merge(role_dict, by = 'role_k1500') 
temp = rbindlist(lapply(2000:2024, function(yr){
  yr_start = as.Date(paste0(yr, "-01-01"))
  yr_end = as.Date(paste0(yr, "-12-31"))
  sub_temp = temp %>%
    .[!is.na(weight) & !is.na(total_compensation) & startdate <= yr_end & (enddate >= yr_start | is.na(enddate))] %>% 
    .[, comp := total_compensation * weight] %>% 
    .[, .(comp_total = sum(comp), empl_total = sum(weight),
          comp_data = sum(data*comp), empl_data = sum(weight*comp)), by = isin_num] %>% 
    .[, year := yr] %>% merge(isin_temp) %>% select(-isin_num)
}))
write_parquet(temp, paste0('1) data/11_parameter_calibration/raw/temp/role_',i,'.parquet'))
}
file_list = list.files(path = '1) data/11_parameter_calibration/raw/temp', full.names = T)
role_dta = rbindlist(lapply(file_list,import_file))
write_parquet(role_dta,'1) data/11_parameter_calibration/raw/firm_role_dta.parquet') 
}

# merge and clean -------------------------------------------------------


