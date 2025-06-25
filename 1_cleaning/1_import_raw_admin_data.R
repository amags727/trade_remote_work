# setup -------------------------------------------------------------------
rm(list = ls()); gc()

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/Big Data Code",
    
    ## update as makes sense for CASD / your own use 
    T ~ "C:/Users/Public/Documents/Big data Project")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')

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
fwrite(bs_data, '1) data/1_bs_data.csv')



## import the BR data
br_data = rbindlist(parLapply(cl, c(start:end),function(yr){
  ## import BR data
  br_path = paste0(raw_dir,"br/br",yr,".csv" )
  br_data_temp = fread(br_path , select = c('ENT_ID','year','empl', 'NACE_M'),
                       colClasses = list(character= c('ENT_ID', 'NACE_M'))) %>%
    rename(firmid = ENT_ID, NACE_BR = NACE_M)
  br_data_temp = br_data_temp[,.(firmid, year,empl, NACE_BR)]
}), fill = T)
fwrite(br_data, '1) data/2_br_data.csv')
stopCluster(cl)

## generate birth data and combined br / bs data  
if(! 'bs_data' %in% ls()) bs_data = fread('1) data/1_bs_data.csv',  colClasses = list(character= 'firmid'))
if(! 'br_data' %in% ls()) br_data = fread('1) data/2_br_data.csv',  colClasses = list(character= 'firmid'))
rm(list= setdiff(ls(), base_env)); gc()
bs_br_data = merge(import_file('1) data/1_bs_data.csv'),import_file('1) data/2_br_data.csv'))
bs_br_data= bs_br_data %>% .[empl > 0 & turnover > 0]
bs_br_firms = bs_br_data %>% distinct(firmid) %>% .[,firmid_num := .I] %>% select(firmid, firmid_num)

bs_br_data[, empl_bucket := ifelse(empl < 10, "1-10",
                                   ifelse(empl < 50, '10-50', 
                                          ifelse(empl < 200, '50-200',
                                                 ifelse(empl >=200, '200+', NA))))]


## make a dictionary of the firms so we don't have to use the firmid 
write_parquet(bs_br_firms,'1) data/0_misc_data/0b_dictionaries/0b1_matched_firm_dict.parquet')

## output the dataset
write_parquet(bs_br_data %>% numeric_firmid(.), raw_bs_br_path)


## import the OFATS data 
OFATS_output = lapply(2010:2020, function(yr){ 
  fread(paste0(raw_dir,'/ofats/ofats',yr,'.csv'),colClasses = list(character= 'firmid')) %>%
    mutate(year = yr)}) %>% rbindlist() 

write_parquet(OFATS_output %>% numeric_firmid(.), raw_ofats_path)
rm(list= setdiff(ls(), base_env)); gc()


# 3) generate customs product level data  -------------------------------------------------
cn8_harmonization = fread('1) data/CN8_code_harmonization/output/CN8_harmonized_1993to2022.csv')
not_current_countries = c("AN", "CS", "EU",'EQ',"QP" ,"QQ", "QR", "QS", "QU", "QV" ,"QW" ,"TP" ,"XA" ,"XC" ,"XE" ,"XF", "XG", "XH" ,"XI" ,"XL" ,"XM" ,"XN" ,"XO" ,"XP" ,"XR", "XS" ,"XU", "XV" ,"XW" ,"XZ", "YU")
start = 1993; end = 2021;
customs_dta_product_lvl = rbindlist(lapply(start:end, function(yr){
c_CN8 = paste0('CN8_',yr); cc_CN8 = c("CN8plus",c_CN8)
file_path = paste0(raw_dir,"itgs/itgs",yr,".csv")
customs_data = import_file(file_path, char_vars = 'firmid') %>% numeric_firmid() %>%  
  rename( value = ntrade, !!c_CN8  := cn08) %>% 
  .[value > 0 & ctry != "" &! ctry %in% c('', 'FR',not_current_countries)] %>% 
  merge(cn8_harmonization %>% distinct(!!!syms(cc_CN8))) %>% 
  .[,.(value = sum(value*1e-3)), by = c('firmid_num', 'year', 'exim', 'ctry', 'CN8plus')] # multiplying by 1e-3 aligns customs data with bs data reported in 1000s
}))  
setorder(customs_dta_product_lvl,ctry, firmid_num, exim, year)

customs_dta_product_lvl = import_file(raw_customs_product_lvl_path) %>%
  merge(import_file('1) data/0_misc_data/0b_dictionaries/0b2_ctry_dict.parquet'), by = 'ctry_num') %>%
  select(-ctry_num) %>% .[!ctry %in% not_current_countries]
  
### generate a country dictionary excluding those codes that do not correspond to current countries:
### these correspond to .9% percent of export observations and .7% of export value in our time period (2008-2021)
customs_dta_product_lvl[,ctry_num := as.numeric(as.factor(ctry))]
ctry_dict = unique(customs_dta_product_lvl[, .(ctry, ctry_num)]) %>% merge(import_file(similiarity_dir, 'inputs/iso_country_name.csv'))
ctry_dict = rbind(data.frame(ctry = 'FR', ctry_num = 0, country_name = 'France'), ctry_dict)
write_parquet(ctry_dict,'1) data/0_misc_data/0b_dictionaries/0b2_ctry_dict.parquet')
write_parquet(customs_dta_product_lvl[,ctry := NULL],raw_customs_product_lvl_path)
rm(list= setdiff(ls(), base_env)); gc() 



# 4) generate customs firm level data ----------------------------------------
customs_dta_firm_lvl = import_file(raw_customs_product_lvl_path) %>% 
  .[,.(products = .N, value = sum(value)), by = c('firmid_num', 'year', 'exim', 'ctry_num')] %>%
  unbalanced_lag(., id_var = c('firmid_num', 'exim', 'ctry_num'), time_var = 'year', value_vars = 'value', lag_amounts = 1) %>% 
  .[,streak_id := cumsum(is.na(value_lag1))] %>% .[,value_lag1 := NULL]

write_parquet(customs_dta_firm_lvl,raw_customs_firm_lvl_path)
rm(list= setdiff(ls(), base_env)); gc()


# 5) Import Prodcom -------------------------------------------------------
harmonized_prodfra = fread('1) data/pc8_harmonization/harmonized codes/prodfra_harmonized_2009to2021.csv')

start = 2009
end = 2021
product_data = rbindlist(lapply(c(start:end),function(yr){
  print(yr)
  
  # import product data 
  temp_vars = c("firmid", "pcc8", "rev" )  
  dta_temp = import_file(paste0(raw_dir,'prodcom/prodcom',yr,'.csv'), col_select = temp_vars, char_vars = c('firmid', 'pcc8')) %>% numeric_firmid()
  
  #keep product values that aren't dropped in harmonization 
  prodfra_var = paste0('prodfra_',yr)
  prodfra_codes =  unique(harmonized_prodfra %>% select(prodfra_var,prodfra_plus) %>% filter(!is.na(prodfra_var))) 
  
  # merge together, aggregate by harmonized product code, find their domninant pcc8
  dta_temp = merge(dta_temp, prodfra_codes,by.x = 'pcc8', by.y = prodfra_var) %>%
    .[,.(rev = NA_sum(rev)), by = .(firmid_num, prodfra_plus)] %>% 
    .[, .SD[which.max(rev)], by = firmid_num] %>% distinct(firmid_num, .keep_all = T) %>% 
    .[,`:=`(year = yr, rev = NULL)]
  
}))

## assume that the dominant product stayed the same in years where we don't observe the firm 
## use the closest year as a proxy 
product_data = expand(start:end, unique(product_data$firmid_num), names = c('year', 'firmid_num')) %>% merge(product_data, all.x= T)
setorder(product_data, firmid_num, year)
for (i in 1:(end-start)){
  print(paste0(i, ": ",sum(is.na(product_data$prodfra_plus))))
  product_data[, prodfra_plus := ifelse(is.na(prodfra_plus), lag(prodfra_plus, i),prodfra_plus), by = firmid_num ]
  product_data[, prodfra_plus := ifelse(is.na(prodfra_plus), lead(prodfra_plus, i),prodfra_plus), by = firmid_num ]
}  
product_data[,prodfra_plus_numeric := as.integer(as.factor(prodfra_plus))]
write_parquet(product_data, '1) data/pc8_harmonization/prodcom_firms_with_dom_product.parquet')



