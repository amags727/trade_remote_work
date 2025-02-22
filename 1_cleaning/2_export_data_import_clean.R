# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../')
source('2) code/00_helper_functions.R'); helper_funcs = ls()[lapply( ls(), function(x) is.function(get(x))) %>% unlist()]
raw_dir = 'C:/Users/Public/1. Microprod/0. Raw data processing/Data/'
exporting = F

# 1) Import Fare-Ficus / OFATS data  -------------------------------------
## setup cluster
deflator = fread('1) data/0_world_bank_france_gdp_deflator_2015_base.csv')
interest_vars = c('ENT_ID','year',"turnover", "for_turnover", 'dom_turnover',
                  'intangible_fixed_assets', 'capital', 'labor_cost')
cl = makeCluster(6); clusterExport(cl, exclude_from(ls(), 'cl'));
clusterEvalQ(cl, {lapply(packages, library, character.only = T)})

# import BS data and adjust for inflation
start = 1994; end = 2020
bs_data =  rbindlist(parLapply(cl,c(start:end),function(yr){
  ## import BS
  path = paste0(raw_dir,"bs/bs",yr,".csv" )
  available_vars = fread(path, nrows = 1) %>% names() %>% intersect(.,interest_vars)
  bs_data_temp <- fread(path, select = available_vars,  colClasses = list(character= 'ENT_ID'))
  bs_data_temp = bs_data_temp %>% select(intersect(interest_vars, colnames(bs_data_temp)))
  
  # adjust variables for inflation
  vars_to_deflate = setdiff(available_vars, c('year', 'ENT_ID')) 
  deflate_value = deflator %>% filter(year == yr) %>% pull(multiply_by)
  bs_data_temp[, (paste0('deflated_', vars_to_deflate)) := lapply(.SD, function(x) x *deflate_value), .SDcols = vars_to_deflate]
  return(bs_data_temp)
}), use.names = T, fill = T)
bs_data = bs_data %>% rename(firmid = ENT_ID)
if(exporting) fwrite(bs_data, '1) data/1_bs_data.csv')



## import the BR data
br_data = rbindlist(parLapply(cl, c(start:end),function(yr){
  ## import BR data
  br_path = paste0(raw_dir,"br/br",yr,".csv" )
  br_data_temp = fread(br_path , select = c('ENT_ID','year','empl', 'NACE_M'),
                       colClasses = list(character= c('ENT_ID', 'NACE_M'))) %>%
    rename(firmid = ENT_ID, NACE_BR = NACE_M)
  br_data_temp = br_data_temp[,.(firmid, year,empl, NACE_BR)]
}), fill = T)
if(exporting) fwrite(br_data, '1) data/2_br_data.csv')
stopCluster(cl)

## generate birth data and combined br / bs data  
if(! 'bs_data' %in% ls()) bs_data = fread('1) data/1_bs_data.csv',  colClasses = list(character= 'firmid'))
if(! 'br_data' %in% ls()) br_data = fread('1) data/2_br_data.csv',  colClasses = list(character= 'firmid'))
bs_br_data = merge(bs_data, br_data)
bs_br_data[, empl_bucket := ifelse(empl < 10, "0-10",
                                   ifelse(empl < 50, '10-50', 
                                          ifelse(empl < 200, '50-200',
                                                 ifelse(empl >=200, '200+', NA))))]
if(exporting) fwrite(bs_br_data, '1) data/3_bs_br_data.csv')

birth_data = bs_br_data[,.(birth_year = NA_min(year)), by = firmid]
birth_data = birth_data[birth_year != 1994] 
if(exporting) fwrite(birth_data,  '1) data/4_birth_data.csv')

## import the OFATS data 
OFATS_output = lapply(2010:2020, function(yr){ 
  fread(paste0(raw_dir,'/ofats/ofats',yr,'.csv'),colClasses = list(character= 'firmid')) %>%
    mutate(year = yr)}) %>% rbindlist() 

if(exporting) fwrite(OFATS_output, '1) data/4_OFATS.csv')


# 2) initial import and cleaning of customs data  ----------------------------
## import the export/exp data,clean product codes, adjust for inflation 
## output the firm-country-year-level data, output country-hs_class-year level data 
## generate each firm's portfolio of countries 

deflator = fread('1) data/0_world_bank_france_gdp_deflator_2015_base.csv')
countries = readRDS('1) data/similarity_matrices/outputs/similiarity_data.rds') %>% pull(ctry)
cn8_harmonization = fread('1) data/CN8_code_harmonization/output/CN8_harmonized_1993to2022.csv')
start = 1993; end = 2021;
cl = makeCluster(6); clusterExport(cl, exclude_from(ls(), 'cl'));
clusterEvalQ(cl, {lapply(packages, library, character.only = T)})


# customs_data_product_level = rbindlist(parLapply(cl ,c(start:end),function(yr){
#   #customs_data = rbindlist(lapply(c(start:end),function(yr){
#   ##import
#   customs_data = fread(paste0(raw_dir,"itgs/itgs",yr,".csv"),colClasses = list(character= 'firmid'))
#   customs_data[, paste0('CN8_',yr) := cn08]
#   customs_data = customs_data[ ctry %in% countries]
#   ## harmonize product code
#   customs_data = merge(customs_data, unique(cn8_harmonization %>% select(paste0('CN8_',yr),CN8plus))) %>% filter(!is.na(CN8plus))
#   customs_data = customs_data[,.(value = sum(ntrade, na.rm = T),
#                                  kg = sum(kg, na.rm = T)), by = .(firmid,ctry,exim,year, CN8plus)]
# 
#   ## adjust for inflation
#   deflate_value = deflator %>% filter(year == yr) %>% pull(multiply_by)
#   customs_data[,deflated_value := value*deflate_value]
#   customs_data[,hs_class := str_sub(CN8plus, 1 ,2)]
# }), fill = T)
# fwrite(customs_data_product_level,'1) data/5_customs_product_level_raw.csv')
# rm(customs_data_product_level)


customs_data = rbindlist(parLapply(cl ,c(start:end),function(yr){
  #customs_data = rbindlist(lapply(c(start:end),function(yr){
  ##import 
  customs_data = fread(paste0(raw_dir,"itgs/itgs",yr,".csv"),colClasses = list(character= 'firmid')) 
  customs_data[, paste0('CN8_',yr) := cn08]
  customs_data = customs_data[ ctry %in% countries]
  
  ## harmonize product code
  customs_data = merge(customs_data, unique(cn8_harmonization %>% select(paste0('CN8_',yr),CN8plus))) %>% filter(!is.na(CN8plus))
  customs_data = customs_data[,.(value = sum(ntrade, na.rm = T)), by = .(firmid,ctry,exim,year, CN8plus)]
  customs_data[,hs_class := str_sub(CN8plus, 1 ,2)]
  customs_data = customs_data[, .(value = sum(value, na.rm = T)), by = .(firmid,ctry,exim,year,hs_class)]
  
  ## adjust for inflation 
  deflate_value = deflator %>% filter(year == yr) %>% pull(multiply_by)
  customs_data[,deflated_value := value*deflate_value]
  
  ## determine the dominant class of the firm 
  extract_values = c('firmid', 'exim', 'hs_class')
  HS_classid = customs_data[, .(value = sum(value, na.rm = T)), by = .(firmid,exim,year,hs_class)]
  HS_classid = HS_classid[HS_classid[, .I[which.max(value)], by = .(firmid,exim,year)]$V1, ..extract_values]
  extract_values = setdiff(extract_values, 'exim')
  customs_data[,hs_class:=NULL]
  customs_data = merge(customs_data,HS_classid[exim == 1, ..extract_values] %>% rename(import_hs_class = hs_class), all.x = T)
  customs_data = merge(customs_data,HS_classid[exim == 2, ..extract_values] %>% rename(export_hs_class = hs_class), all.x = T)
  
  ## collapse to the firm-year-level
  customs_data = customs_data[, .(products = .N,
                                  value = sum(value, na.rm = T),
                                  deflated_value = sum(deflated_value, na.rm = T)),
                              by = .(firmid,ctry,exim,year,import_hs_class, export_hs_class)]
  
  ## generate each firm's portfolio
  setorder(customs_data, ctry)
  customs_data = customs_data[, `:=`(import_portfolio = list(ctry[exim ==1]),
                                     export_portfolio = list(ctry[exim ==2])),
                              by = .(firmid, year)]
}), fill = T) %>% as.data.table()
## generate keys for each portfolio seen in the data 
portfolio_list = customs_data %>% rename(portfolio = import_portfolio) %>% select(portfolio) %>%
  rbind(.,  customs_data %>% rename(portfolio = export_portfolio) %>% select(portfolio)) %>% as.data.table()
portfolio_list = portfolio_list[!duplicated(portfolio)]
portfolio_list$portfolio_key = 1:nrow(portfolio_list)

## make string versions of the portfolio variables so we can merge
portfolio_list[, portfolio_string := sapply(portfolio, function(x) paste(x, collapse = ','))]
customs_data[, `:=`(import_portfolio_string = sapply(import_portfolio, function(x) paste(x, collapse = ',')),
                    export_portfolio_string = sapply(export_portfolio, function(x) paste(x, collapse = ',')))]

## merge together the portfolio strings in the cusrtoms data with the portfolio keys 
customs_data = merge(customs_data, portfolio_list %>% select(portfolio_string, portfolio_key),
                     by.x = 'import_portfolio_string', by.y = 'portfolio_string') %>%
  rename(import_portfolio_key = portfolio_key) %>%
  merge(.,portfolio_list %>% select(portfolio_string, portfolio_key),
        by.x = 'export_portfolio_string', by.y = 'portfolio_string') %>%
  rename(export_portfolio_key = portfolio_key)


# cross pollinate the export and import portfolios for the firm 
customs_data[, `:=`(export_portfolio_key = max(na.rm=T, export_portfolio_key),
                    import_portfolio_key = max(na.rm=T, import_portfolio_key)), by= .(firmid, year)]

#generate lags 
lag_data = customs_data[,.(firmid, year, export_portfolio_key, import_portfolio_key)] %>% unique() %>% arrange(firmid, year)
lag_data = unbalanced_lag(lag_data, 'firmid', 'year', 'export_portfolio_key', 1)
lag_data = unbalanced_lag(lag_data, 'firmid', 'year', 'import_portfolio_key', 1)
lag_data = lag_data[,c('export_portfolio_key', 'import_portfolio_key'):=NULL]
customs_data = merge(customs_data, lag_data, by = c('firmid', 'year'))

customs_data[year != 1993 & is.na(export_portfolio_key_lag1), export_portfolio_key_lag1 := 1]
customs_data[year != 1993 & is.na(import_portfolio_key_lag1), import_portfolio_key_lag1 := 1]

if(exporting){
  fwrite(customs_data %>% select(-c(import_portfolio, export_portfolio,export_portfolio_string,import_portfolio_string)),
       '1) data/6_customs_raw.csv')
  saveRDS(portfolio_list %>% select(-portfolio_string), '1) data/7_portfolio_data_raw.RDS')
}

# 3) assign extended grav based on portfolio ----------------------------
portfolio_data = as.data.table(readRDS('1) data/7_portfolio_data_raw.RDS'))
similiarity_data = as.data.table(readRDS('1) data/similarity_matrices/outputs/similiarity_data.rds'))


cl = makeCluster(6); clusterExport(cl, c('packages', 'portfolio_data', 'similiarity_data'));
clusterEvalQ(cl, {lapply(packages, library, character.only = T)})

parLapply(cl, 1:nrow(similiarity_data),function(i){
  sim_temp = similiarity_data[i]
  temp = portfolio_data
  temp[,`:=`(ctry = sim_temp$ctry, s_language = sim_temp$share_language,
             s_region = sim_temp$share_region, s_border = sim_temp$share_border)]
  temp[, `:=`(present = mapply(function(x,y) as.numeric(x %in% y), ctry, portfolio),
              language = mapply(function(x,y) as.numeric(length(intersect(x,y))>0), s_language, portfolio),
              region = mapply(function(x,y) as.numeric(length(intersect(x,y))>0), s_region, portfolio),
              border = mapply(function(x,y) as.numeric(length(intersect(x,y))>0), s_border, portfolio))]
  temp = temp[,c('portfolio', 's_language', 's_region', 's_border') := NULL]
  if(exporting) fwrite(temp,paste0('1) data/5_portfolio_similiarity_comps/comp_',i,'.csv'))
  return(NULL)
})
stopCluster(cl)

portfolio_data_complete = data.table(portfolio_key = rep(1:nrow(portfolio_data), nrow(similiarity_data)))
portfolio_data_complete[,`:=`(ctry ="hi",language = 0, region = 0, border =0)]


for(i in 1:nrow(similiarity_data)){ 
  print(i/nrow(similiarity_data))
  start = 1 + (i-1)*nrow(portfolio_data); end = i*nrow(portfolio_data)
  portfolio_data_complete[start:end,2:5 := fread(paste0('1) data/5_portfolio_similiarity_comps/comp_',i,'.csv'),
                                                 select = c('ctry', 'language', 'region', 'border'))]
}

if(exporting) fwrite(portfolio_data_complete, '1) data/8_portfolio_data_similiarity_matched.csv')
# 4) clean firm level customs data ----------------------------------------------------------------------
birth_data = fread('1) data/4_birth_data.csv', colClasses = list(character= 'firmid'));
customs_data = fread('1) data/6_customs_raw.csv', colClasses = list(character= 'firmid'))
portfolio_data_complete =fread('1) data/8_portfolio_data_similiarity_matched.csv')

# note whether we observe the birth of the firm 
customs_data = (merge(customs_data, customs_data[exim == 1][,.(first_import_year = min(year)), by = firmid], all.x = T, by = 'firmid') %>%
               merge(.,customs_data[exim == 2][,.(first_export_year = min(year)), by = firmid], all.x =T, by = 'firmid'))[
               ,first_customs_year := min(year), by = firmid]

customs_data = merge(customs_data, birth_data, all.x = T)
customs_data[, birth_observed := ifelse(is.na(birth_year), 0, ifelse(first_customs_year >= birth_year, 1, 0))]
customs_data[, firm_age := year - birth_year] ## please note should not be taken literally unless "birth observed" is True 
customs_data[first_customs_year < birth_year | is.na(birth_year), firm_age := year - first_customs_year]
customs_data$first_customs_year = NULL
##  Extended Gravity Information 
customs_data = customs_data %>% 
  merge(.,portfolio_data_complete %>% rename_with(.cols = -ctry, ~paste0('export_',.)), by = c('export_portfolio_key', 'ctry')) %>%
  merge(.,portfolio_data_complete %>% rename_with(.cols = -ctry, ~paste0('import_',.)), by = c('import_portfolio_key', 'ctry')) %>%
  merge(.,portfolio_data_complete %>% rename_with(.cols = -ctry, ~paste0('export_',.,"_lag1")), by = c('export_portfolio_key_lag1', 'ctry'), all.x = T) %>%
  merge(.,portfolio_data_complete %>% rename_with(.cols = -ctry, ~paste0('import_',.,"_lag1")), by = c('import_portfolio_key_lag1', 'ctry'), all.x = T)
customs_data[, c('import_portfolio_key_lag1' ,'export_portfolio_key_lag1', 'import_portfolio_key', 'export_portfolio_key') := NULL]

## identify streaks in the data
customs_data[, id_val:= .GRP , by = .(firmid, exim, ctry)][, dummy := 1]
customs_data = unbalanced_lag(customs_data,'id_val', 'year', 'dummy', 1)
customs_data[, streak_id := rleid(id_val, dummy_lag1)]
customs_data = unbalanced_lag(customs_data,'id_val', 'year', 'streak_id', -1)
customs_data[!is.na(streak_id_lead1), streak_id := streak_id_lead1]
customs_data[, `:=`(streak_end = max(year), streak_start = min(year)), by = .(streak_id)]
customs_data[, `:=`(streak_birth_observed = as.numeric(streak_start != 1993),
                                  streak_death_observed = as.numeric(streak_end != 2021),
                                  year_in_streak = year + 1 - streak_start,
                                  streak_length = streak_end + 1 - streak_start)]
customs_data[, c('dummy', 'dummy_lag1', 'id_val', 'streak_id_lead1', 'streak_end'):= NULL]
## export
if(exporting) fwrite(customs_data, '1) data/9_customs_cleaned.csv')


# 5) clean product level customs data -----------------------------
customs_data_product_level = fread('1) data/5_customs_product_level_raw.csv',colClasses = list(character= 'firmid'))
firm_birth_observed = fread('1) data/9_customs_cleaned.csv', colClasses = list(character= 'firmid'), select = c('firmid', 'birth_observed')) %>% unique()
customs_data_product_level = customs_data_product_level %>% merge(.,firm_birth_observed)
customs_data_product_level = customs_data_product_level


# generate streak information 
customs_data_product_level[, id_val:= .GRP , by = .(firmid, CN8plus, exim, ctry)][, dummy := 1]
customs_data_product_level = unbalanced_lag(customs_data_product_level,'id_val', 'year', 'dummy', 1)
customs_data_product_level[, streak_id := rleid(id_val, dummy_lag1)]
customs_data_product_level = unbalanced_lag(customs_data_product_level,'id_val', 'year', 'streak_id', -1)
customs_data_product_level[!is.na(streak_id_lead1), streak_id := streak_id_lead1]
customs_data_product_level[, `:=`(streak_end = max(year), streak_start = min(year)), by = .(streak_id)]
customs_data_product_level[, `:=`(streak_birth_observed = as.numeric(streak_start != 1993),
                                  streak_death_observed = as.numeric(streak_end != 2021),
                                  year_in_streak = year + 1 - streak_start,
                                  streak_length = streak_end + 1 - streak_start)]
customs_data_product_level[, c('dummy', 'dummy_lag1', 'id_val', 'streak_id_lead1', 'streak_end'):= NULL]

#export product level information 
if(exporting) fwrite(customs_data_product_level, '1) data/10_customs_product_level_cleaned.csv')

## Now generate the necessary summary stats
customs_data_product_level = fread('1) data/10_customs_product_level_cleaned.csv',colClasses = list(character= 'firmid'))
age_data = fread('1) data/9_customs_cleaned.csv', colClasses = list(character= 'firmid'),
                 select = c('firmid', 'year', 'firm_age')) %>% unique()

year_range = customs_data_product_level %>% pull(year) %>% unique()
countries = customs_data_product_level  %>% pull(ctry) %>% unique() 
cn8_codes = customs_data_product_level %>% pull(CN8plus) %>% unique()

cn8_ctry_expanded = expand.grid(year_range,countries, cn8_codes, 1:2) %>% as.data.table() %>% 
  rename(year = Var1, ctry = Var2, CN8plus = Var3, exim = Var4) 

interest_vars = c('HHI', 'firm_age', 'incumbent_streak_share', 'streak_age', 'num_firms', 'deflated_value')
product_collapsed = merge(customs_data_product_level, age_data, all.x = T)
product_collapsed[, id_val:= .GRP , by = .(CN8plus, exim, ctry, year)]

collapsed_pt_1 =  product_collapsed[, .(HHI =  1e4 *sum(na.rm = T, (deflated_value/sum(deflated_value, na.rm =T))^2),
                             incumbent_streak_share = mean(year_in_streak != 0),
                             num_firms = .N,
                             deflated_value = sum(na.rm = T, deflated_value)), by = id_val]
collapsed_pt_2 = product_collapsed[, .(firm_age = median(firm_age, na.rm = T),
                                       streak_age = median(year_in_streak, na.rm =T)), by = id_val]
product_collapsed = merge(collapsed_pt_1, collapsed_pt_2 , by = 'id_val') %>%
                   merge(., unique(product_collapsed[,.(id_val, CN8plus, exim, ctry, year)])) 

product_collapsed[year == 1993, c('firm_age', 'incumbent_streak_share', 'streak_age'):= NA]
product_collapsed = merge(product_collapsed,cn8_ctry_expanded, all = T, by= c('year', 'ctry', 'CN8plus','exim'))  %>%
                    mutate(across(c(num_firms, deflated_value), ~replace_na(.,0))) %>%
                    group_by(year) %>% mutate(across(interest_vars, ~ntile(.,4), .names = '{col}_quartile_annual')) %>%
                    ungroup() %>% rename_with(~paste0('cn8_ctry_',.), contains(interest_vars))
product_collapsed$id_val = NULL
if(exporting) fwrite(product_collapsed, '1) data/10a_product_lvl_summary_stats.csv')

# 6) generate country and hs_class-country level data ---------------------------------------------
similiarity_data = readRDS('1) data/similarity_matrices/outputs/similiarity_data.rds')
customs_data = fread('1) data/9_customs_cleaned.csv', colClasses = list(character= 'firmid'))[exim == 2 & !is.na(export_hs_class)] ## exports only 
population_gdp = fread('1) data/similarity_matrices/outputs/gdp_population_data.csv') %>% mutate(gdp = gdp_per_capita*population)
landlocked = fread('1) data/similarity_matrices/outputs/landlocked countries.csv')
distance_from_france = fread('1) data/similarity_matrices/outputs/france_distance_data.csv')

##6a) expand out to all possible hs-ctry-year/ ctry-year combinations (not just those seen in the data) 
##    to obtain distributional data about the number of firms and value of exports 
year_range = customs_data %>% pull(year) %>% unique()
countries = customs_data  %>% pull(ctry) %>% unique() 
hs_classes = customs_data %>% pull(export_hs_class) %>% unique()
hs_ctry_expanded = expand.grid(year_range,countries, hs_classes) %>% as.data.table() %>% 
  rename(year = Var1, ctry = Var2, export_hs_class = Var3) 
ctry_expanded = hs_ctry_expanded %>% select(-export_hs_class) %>% unique()


## now generate the summary stats we're interested in 
interest_vars = c('HHI', 'firm_age', 'incumbent_streak_share', 'streak_age', 'num_firms', 'deflated_value')
customs_hs_collapsed = customs_data %>% group_by(ctry,export_hs_class, year) %>% 
  summarize(HHI =  1e4 *NA_sum((deflated_value/NA_sum(deflated_value))^2),
            firm_age = median(firm_age),
            incumbent_streak_share = mean(year_in_streak != 0),
            streak_age = median(year_in_streak),
            num_firms  = n(),
            deflated_value = NA_sum(deflated_value),.groups = 'drop') %>%
  mutate(across(c(firm_age, incumbent_streak_share, streak_age), ~ifelse(year == 1993,NA,.))) %>%
  merge(.,hs_ctry_expanded, all = T) %>% 
  mutate(across(c(num_firms, deflated_value), ~replace_na(.,0))) %>%
  group_by(year) %>% mutate(across(interest_vars, ~ntile(.,4), .names = '{col}_quartile_annual')) %>% 
  ungroup() %>% rename_with(~paste0('hs_ctry_',.), contains(interest_vars))

interest_vars = c(interest_vars, 'population', 'gdp_per_capita', 'gdp', 'distance_to_france')
customs_ctry_collapsed = customs_data %>% group_by(ctry, year) %>% 
  summarize(HHI =  1e4 *NA_sum((deflated_value/NA_sum(deflated_value))^2),
            firm_age = median(firm_age),
            incumbent_streak_share = mean(year_in_streak != 0),
            streak_age = median(year_in_streak),
            num_firms  = n(),
            deflated_value = NA_sum(deflated_value),.groups = 'drop') %>%
  mutate(across(c(firm_age, incumbent_streak_share, streak_age), ~ifelse(year == 1993,NA,.))) %>%
  merge(.,ctry_expanded, all = T) %>% mutate(across(c(num_firms, deflated_value), ~replace_na(.,0))) %>%
  merge(., population_gdp, all.x = T) %>% merge(., distance_from_france, all.x =T) %>%
  merge(.,landlocked, all.x =T) %>% mutate(ctry_landlocked = !is.na(landlocked), landlocked = NULL) %>% 
  merge(., distance_from_france, all.x =T) %>% group_by(year) %>%
  mutate(across(interest_vars, ~ntile(.,4), .names = '{col}_quartile_annual')) %>% ungroup() %>%
  rename_with(~paste0('ctry_',.), contains(interest_vars))

## merge; add in gravity data and export 
country_level_data = merge(customs_hs_collapsed, customs_ctry_collapsed) %>% as.data.table()
french_data = similiarity_data[ctry == 'FR']
french_region = french_data$share_region; french_border = french_data$share_border; french_language = french_data$share_language

country_level_data[,`:=`(grav_region = as.numeric(ctry %in% french_region[[1]]),
                         grav_border = as.numeric(ctry %in% french_border[[1]]),
                         grav_language = as.numeric(ctry %in% french_language[[1]]))]

country_level_data[, grav_region_border_language := paste0(grav_region,",", grav_border,",",grav_language)]
if(exporting) fwrite(country_level_data, '1) data/11_country_level_data.csv')


# 7) Generate summary information about domestic industry -----------------------
bs_br_data =  fread('1) data/3_bs_br_data.csv', select= c('firmid', 'year', 'deflated_dom_turnover', 'NACE_BR','empl'),
                    colClasses = list(character= 'firmid'))

bs_br_data[, birth_year := min(year), by = firmid];
bs_br_data[, `:=`(age = year - birth_year, young = year -birth_year < 3)];
bs_br_data[year< 1997, young:= NA]; bs_br_data[year == 1994, age := NA]
bs_br_data[, birth_year := NULL]

NACE_quartile_vars = paste0('nace_', c('deflated_rev', 'empl',  "num_firms", 'firm_age', 'HHI'))
NACE_quartiles = bs_br_data %>% group_by(year, NACE_BR) %>%
  summarize(nace_firm_age = median(age, na.rm = T), # use median bc we can't observe old firm ages well 
            nace_deflated_rev = sum(deflated_dom_turnover, na.rm = T),
            nace_empl = sum(empl, na.rm= T), 
            nace_HHI =  1e4 *NA_sum((deflated_dom_turnover/NA_sum(deflated_dom_turnover))^2),
            nace_num_firms = n(), .groups = 'drop') %>% as.data.table()

NACE_quartiles = NACE_quartiles[!is.na(NACE_BR)]
NACE_quartiles = NACE_quartiles %>% group_by(year) %>% mutate(across(NACE_quartile_vars, ~ntile(.,4), .names = '{col}_quartile_annual')) %>% ungroup()
names(NACE_quartiles) = gsub('annual_avg', 'avg', names(NACE_quartiles))
if(exporting) fwrite(NACE_quartiles, '1) data/12_domestic_industry_summary_stats.csv')

# 8) Construct revenue growth analysis datasets ---------------------------------------------
## 8a at the firm level 
bs_br_data = fread('1) data/3_bs_br_data.csv', colClasses = list(character= 'firmid'))
customs_data = fread('1) data/9_customs_cleaned.csv', colClasses = list(character= 'firmid'))[exim == 2][,exim := NULL] ## exports only 

##summary stats and lagged versions
domestic_industry_data = fread('1) data/12_domestic_industry_summary_stats.csv')
domestic_industry_data_lag = domestic_industry_data %>% mutate(year = year +1) %>% rename_with(~paste0(.,"_lag"), -c(NACE_BR,year)) %>%
  select(NACE_BR, year, nace_deflated_rev_quartile_annual_lag)

country_data = fread('1) data/11_country_level_data.csv')
country_data_lag = country_data %>% mutate(year = year + 1) %>% rename_with(~paste0(.,"_lag"), -c(ctry, export_hs_class,year)) %>%
  select(ctry, export_hs_class, year, contains(c('deflated_value_quartile', 'gdp', 'num_firms', 'streak_age','population')))

## merge
export_regression_inputs = merge(customs_data,bs_br_data, by = c('firmid', 'year'), all.x = T) %>% 
  merge(., domestic_industry_data, by = c('NACE_BR', 'year'), all.x = T) %>% 
  merge(., domestic_industry_data_lag, by = c('NACE_BR', 'year'), all.x = T) %>% 
  merge(., country_data, by = c('ctry', 'export_hs_class', 'year'), all.x = T) %>%
  merge(., country_data_lag, by = c('ctry', 'export_hs_class', 'year'), all.x = T) %>%
  mutate(export_ratio = ifelse(deflated_dom_turnover== 0, NA, deflated_value/deflated_dom_turnover))
export_regression_inputs[, streak_always_observed := all(!is.na(export_ratio)), by = streak_id]
if(exporting) fwrite(export_regression_inputs, '1) data/13a_firm_level_growth_analysis_inputs.csv')


## 8b at the product level 
customs_data_product_level = fread('1) data/10_customs_product_level_cleaned.csv', colClasses = list(character= 'firmid'))[exim == 2][,exim := NULL]
bs_br_data =  fread( '1) data/3_bs_br_data.csv', colClasses = list(character= 'firmid'))[firmid %in% unique(customs_data_product_level[, firmid])]

## summary stats and lagged versions
product_collapsed = fread( '1) data/10a_product_lvl_summary_stats.csv')[exim ==2 & cn8_ctry_num_firms > 0][,exim:= NULL]
product_collapsed_lag = product_collapsed %>% mutate(year = year +1) %>% rename_with(~paste0(.,"_lag"), -c(ctry,CN8plus,year)) %>%
  select(ctry, CN8plus, year, contains('deflated_value_quartile'))

country_level_data = fread('1) data/11_country_level_data.csv') %>% select(-contains('hs')) %>% unique()
country_level_data_lag = country_level_data %>% mutate(year = year + 1) %>% rename_with(~paste0(.,"_lag"), -c(ctry, year)) %>%
  select(ctry, year, contains('deflated_value_quartile'))


customs_data_product_level = merge(customs_data_product_level, country_level_data, all.x = T, by = c( 'ctry', 'year')) 
customs_data_product_level = merge(customs_data_product_level, country_level_data_lag, all.x = T, by = c( 'ctry', 'year')) 
customs_data_product_level = merge(customs_data_product_level, product_collapsed, all.x = T, by = c( 'ctry', 'CN8plus', 'year'))
customs_data_product_level = merge(customs_data_product_level, product_collapsed_lag, all.x = T, by = c( 'ctry', 'CN8plus', 'year'))
customs_data_product_level = merge(customs_data_product_level,bs_br_data, all.x = T, by = c('firmid', 'year'))
customs_data_product_level[,export_ratio := deflated_value/deflated_dom_turnover][deflated_dom_turnover ==0, export_ratio := NA]
if(exporting) fwrite(customs_data_product_level, '1) data/13b_product_level_growth_analysis_inputs.csv')


