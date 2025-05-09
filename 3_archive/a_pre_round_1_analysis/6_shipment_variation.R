# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('00_helper_functions.R'); helper_funcs = ls()[lapply( ls(), function(x) is.function(get(x))) %>% unlist()]
raw_dir = 'C:/Users/Public/1. Microprod/0. Raw data processing/Data/'



# import each year and clean ----------------------------------------------
cn8_harmonization = fread('../1) data/CN8_code_harmonization/output/CN8_harmonized_1993to2022.csv')
deflator = fread('../1) data/0_world_bank_france_gdp_deflator_2015_base.csv')
EU = c('AT', 'BE', 'BG', 'HR', 'CY', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'GR', 'HU', 'IE', 'IT', 'LV', "LT", 'LU', "MT", 'NL', 'PL', 'PT',' RO', 'SK', 'SI', 'ES', 'SE', 'GB')

## import the shipment data 
extra_EU_export_shipments = lapply(1993:2021, function(yr){
  print(yr)
  customs_data = fread(nrows = 1000,paste0(raw_dir,"itgs/itgs",yr,".csv"),colClasses = list(character= 'firmid')) 
  customs_data = customs_data[exim == 2 & ! ctry %in% EU & kg !=0] ## select only exports to outside the EU that have weight attached
  
  
  customs_data[, paste0('CN8_',yr) := cn08]
  customs_data = customs_data %>% rename(value =ntrade) %>% select(-cn08)
  customs_data = merge(customs_data, unique(cn8_harmonization %>% select(paste0('CN8_',yr),CN8plus))) %>% filter(!is.na(CN8plus))
  
  deflate_value = deflator %>% filter(year == yr) %>% pull(multiply_by)
  customs_data[,deflated_value := value*deflate_value]
  customs_data = customs_data[, .(firmid, year, ctry,CN8plus,value,deflated_value, kg)]
}) %>% rbindlist() 


## generate information about export streak
streak_information = unique(extra_EU_export_shipments[, .(year, firmid, ctry, CN8plus)])
setorder(streak_information, firmid,CN8plus, ctry, year)
streak_information[, `:=`(year_diff = c(1, diff(year)), next_year =  shift(year, type = 'lead')), by = .(firmid,CN8plus, ctry)]
streak_information[, streak_id := rleid(firmid,CN8plus, ctry, year_diff!=1)]
streak_information[, streak_id := ifelse(!is.na(next_year) & year == next_year -1, shift(streak_id, type= 'lead'), streak_id)]
streak_information[, `:=`(year_in_streak = seq_len(.N) -1,streak_start = min(year)), by = .(streak_id)]
streak_information[, c('year_diff', 'next_year'):= NULL]
streak_information[, streak_birth_observed := as.numeric(streak_start != 1993)]
streak_information[, streak_length := max(year_in_streak), by = streak_id]
extra_EU_export_shipments = merge(extra_EU_export_shipments, streak_information) 

# standardize the value per kg metrics 
extra_EU_export_shipments[, deflated_value_per_kg := deflated_value / kg]
extra_EU_export_shipments[, `:=`(mean_val = mean(deflated_value_per_kg, na.rm = T),
                                 sd_val = sd(deflated_value_per_kg, na.rm =T)), by =  .(ctry, year,CN8plus)]
extra_EU_export_shipments[, relative_performance :=  deflated_value_per_kg -mean_val]
extra_EU_export_shipments[, normalized_performance := relative_performance / sd_val]
fwrite(extra_EU_export_shipments, 'extra_EU_export_shipments.csv')


# make the graph ----------------------------------------------------------
period_of_interest = 4; time_allowed = 0

output_data = extra_EU_export_shipments[streak_birth_observed == 1 & year_in_streak <= period_of_interest & streak_length >= period_of_interest]
output_data[, .(relative_performance = median(na.rm =T, relative_performance),
               normalized_performance = median(na.rm = T, normalized_performance)), by = year_in_streak]

graph_data = lapply(0:period_of_interest, function(stk_year){
  temp = 
  
  temp = extra_EU_export_shipments[streak_birth_observed == 1 & year_in_streak <= time_allowed & streak_length >= period_of_interest]
  temp = temp[,.(sd_val = sd(deflated_value_per_kg_std,na.rm= T), mean_val = mean(deflated_value_per_kg_std, na.rm =T)), by = .(streak_id)]
  value_coef_var = temp[,.(mean_value_coef_var = mean(sd_val / mean_val, na.rm =T))] %>% pull(mean_value_coef_var)
  output = data.frame(cum_years = time_allowed, coef_var = value_coef_var)
}) %>% rbindlist()

## example 
num_values = 1000; num_iterations = 1000
output = lapply(1:100, function(iteration){
sapply(seq(1,.5, length.out = num_values), function(sd) rnorm(1,0,sd))

  
hi = data.frame(value = 1:num_values ,
                obs = sapply(seq(1,.9, length.out = num_values), function(sd) rnorm(1,0,sd)),
                cum_sd = NA)
for (i in 1:nrow(hi)){hi$cum_sd[i] =   sd(hi$obs[1:i]) }
return(hi)
}) %>% rbindlist() %>% group_by(value) %>% summarize(cum_sd = mean(cum_sd))
ggplot(output, aes(x = value, y= cum_sd)) + geom_line()




