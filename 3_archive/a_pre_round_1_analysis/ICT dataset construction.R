

rm(list = ls())
# libraries
library(data.table)
library(haven)
library(readxl)
library(openxlsx)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
setwd('C:/Users/NEWPROD_A_MAGNUSO/Documents/Big data Project')
raw_data_dir = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"
finished_data_dir = 'C:/Users/NEWPROD_A_MAGNUSO/Documents/Big data Project/1) Data/'

start = 2012 
end = 2021


var_list = c('firmid','year', 'DATA_use','DATA_src_IOT', 'DATA_src_weareables', 'DATA_src_social_media', 'DATA_src_other', 'DATA_anal_mtd_ml',
             'DATA_anal_mtd_nat_lang', 'DATA_anal_mtd_other', 'DATA_outsource', 'DATA_inhouse', 'DATA_considered', 'DATA_hind_CBR',
             'DATA_hind_HC', 'DATA_hind_avail', 'DATA_hind_INF', 'DATA_hind_legal', 'DATA_hind_priority', 'DATA_hind_quality',
             'DATA_hind_useful', 'DATA_hind_other', 'DATA_sell', 'DATA_purchase', 'DATA_uptake_increase', 'DATA_uptake_same',
             'DATA_uptake_decrease', 'own_website_sales_binary', 'marketplace_sales_binary', 'all_web_sales_binary', 'EDI_sales_binary', 'own_website_share',
             'marketplace_share', 'all_web_share', 'EDI_share', 'all_web_sales_value', 'EDI_sales_value', 'all_web_sales_france_binary',
             'all_web_sales_EU_binary', 'all_web_sales_non_EU_binary', 'num_marketplaces', 'marketplace_commission', 'marketplace_50',
             'AI_use_read_text', 'AI_use_speech_recog', 'AI_use_generative', 'AI_object_ID', 'AI_use_data_anal', 'AI_use_automate_tasks',
             'AI_use_automate_machine', 'AI_use_marketing', 'AI_use_production', 'AI_use_admin', 'AI_use_managment', 'AI_use_logistics',
             'AI_use_secu', 'AI_use_HR', 'AI_acq_internal', 'AI_acq_purchase_mod', 'AI_acq_OS_mod', 'AI_acq_purchase', 'AI_acq_subcontract',
             'AI_use' )



yr = 2018


for (yr in start:end){
path = paste0(raw_data_dir,"ictec/ictec",yr,".csv" )
ict_dt_temp =  as.data.table(fread(path))
for (var in var_list) {
  if(!var %in% names(ict_dt_temp)){
    ict_dt_temp[[var]] = NA
  }
}
ict_dt_temp = ict_dt_temp %>% select(any_of(var_list)) %>% mutate(year = year -1)
  if (yr == 2020){
    ict_dt_temp = ict_dt_temp %>% mutate(
      DATA_inhouse = pmax(!!! select(., starts_with('DATA_src')), na.rm = T), 
      DATA_inhouse = ifelse(is.na(DATA_inhouse),0, DATA_inhouse),
      DATA_outsource = ifelse(is.na(DATA_outsource),0, DATA_outsource),
      DATA_use = as.numeric(DATA_inhouse + DATA_outsource > 0),
      DATA_use_status = ifelse(DATA_use == 1, "C) Using", ifelse(DATA_considered==1, "B) Considered Using", "A) Not Considering")),
      DATA_use_status = ifelse(is.na(DATA_use_status),  "A) Not Considering", DATA_use_status)
      ) 
  }
  if (yr == 2018 | yr == 2016 ){
    ict_dt_temp = ict_dt_temp %>% mutate(
      DATA_inhouse = ifelse(is.na(DATA_inhouse),0, DATA_inhouse),
      DATA_outsource = ifelse(is.na(DATA_outsource),0, DATA_outsource),
      DATA_use = as.numeric(DATA_inhouse + DATA_outsource >0)
    )
  }

## add in the value add data 
    path = paste0(raw_data_dir,"sbs/sbs",yr -1,".csv" )
    bs_dt <- as.data.table(fread(path)) %>% rename( firmid = ENT_ID,  fte = SBS_16140, turnover = SBS_12110, value_add = SBS_12150) %>%
      select(firmid, fte, turnover, value_add) %>% mutate(value_add_quantile = ntile(value_add, 20))
    ict_dt_temp = ict_dt_temp %>% left_join(bs_dt)
    
## add in the BR registry NACE data 
    path = paste0(raw_data_dir,"br/br",yr -1,".csv" )
    br_dt <- as.data.table(fread(path)) %>% rename( firmid = ENT_ID, NACE_BR = NACE_M)   %>%
      select(firmid, NACE_BR)
    ict_dt_temp = ict_dt_temp %>% left_join(br_dt)
    

    
## add in the reallocation data 
    product_reallocation = read.csv('C:/Users/NEWPROD_A_MAGNUSO/Documents/Reallocation_work/2 Data/biennial_product_reallocation.csv') %>% group_by(year)%>%
      mutate(survivor_reallocation_rate = ifelse(survivor,reallocation_rate, NA),  
             pdct_reallocation_quantile = ntile(survivor_reallocation_rate,20),
             survivor_introduced_share = ifelse(survivor, introduced_share, NA),
             pdct_introduction_quantile = ntile(survivor_introduced_share, 20),
             firmid = as.numeric(firmid)) %>% 
      select(firmid, year, survivor_reallocation_rate, pdct_reallocation_quantile, survivor_introduced_share, pdct_introduction_quantile, dominant_nace) %>%
      filter(year == yr -1)
    
    ict_dt_temp = ict_dt_temp %>% left_join(product_reallocation)
  

## combine year datasets 
if (yr == start){
     ict_dt =  ict_dt_temp
    }else{
      ict_dt = rbind( ict_dt,ict_dt_temp, fill = T)
    }
}

write.csv(ict_dt, paste0(finished_data_dir,'ict_dt.csv'),row.names = F)








