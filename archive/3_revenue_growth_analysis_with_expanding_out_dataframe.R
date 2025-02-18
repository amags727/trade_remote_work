# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','ggplot2', 'patchwork','fixest')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_helper_functions.R")


# 1) Firm-Industry Level Analysis ---------------------------------------------------
graph_inputs = read_excel("3a_growth_delays_inputs_and_analysis.xlsx")
## import data and keep only observations where 
firm_level_input_data = fread('../1) data/13a_firm_level_growth_analysis_inputs.csv')
# firm_level_input_data[, always_observed := all(!is.na(export_ratio)), by = streak_id]
# firm_level_input_data = firm_level_input_data[always_observed==T]


## expand out the data_frame  
period_of_observation = 4; last_year = max(na.rm = T, firm_level_input_data$year)
firm_level_input_data[,last_allowable_streak_year := last_year - streak_start]
streak_vars = c('streak_birth_observed', 'streak_start', 'streak_length', 'last_allowable_streak_year')
interest_vars =  c('export_ratio', 'deflated_value')
streaks = firm_level_input_data %>% pull(streak_id) %>% unique()
expanded = expand.grid(streaks, 1:period_of_observation) %>% as.data.table() %>% rename(streak_id = Var1, year_in_streak = Var2) 

firm_level_input_data = merge(firm_level_input_data, expanded, all = T, by = c('streak_id', 'year_in_streak')) %>% mutate(addition = is.na(streak_start))
firm_level_input_data[addition ==T, (interest_vars) := lapply(.SD, function(x) replace_na(x,0)),.SDcols = interest_vars]
firm_level_input_data[, (streak_vars) := lapply(.SD, function(x) max(na.rm= T, x)), by = streak_id, .SDcols = streak_vars]
firm_level_input_data = firm_level_input_data[year_in_streak <= last_allowable_streak_year]


## Run the Firm Industry Level Analysis 
breakdown_graphs = function(interest_var,starter_var, observation_period, quartile, titl,output, expansion){
  #choose data frame 
  if( str_ends(output, '_firm') | str_ends(output, '_firm_lag')){
  data = firm_level_input_data}else{ data = product_level_input_data}
  
  # prior to 2010 EU and non-EU countries had different reporting requirements 
  data = data[ streak_start >= 2010]
  
  if (starter_var==T | expansion == T){
    clipped = data[streak_start == year]; clipped[,interest := as.factor(get(interest_var))];
    clipped = clipped[,.(interest,streak_id)]; data = merge(data, clipped, by = 'streak_id')
  }else{
    data[,interest := as.factor(get(interest_var))]  
  }
  
  if(expansion== T){
    data = data[year_in_streak <= observation_period]
  }else{
    data = data[streak_length >= observation_period & year_in_streak <= observation_period] 
  }
  survival_stats = data[deflated_value > 0]; 
  survival_stats = survival_stats[!is.na(interest), .(count = .N), by = .(year_in_streak, interest)]
  survival_stats[, share_survive := 100*count / max(count), by = interest]
  survival_plot = ggplot(data = survival_stats, aes(x = year_in_streak +1, y = share_survive, color = interest)) + geom_line() +
                  labs(subtitle = 'Survival Rate of Firms over Time', y= '% Firms Remaining', x = 'year in streak') +
                  theme(legend.position = 'none')
  
  data = data[addition ==F & streak_length >= observation_period,
             .(median_export_ratio = median(export_ratio, na.rm = T),
               mean_export_ratio  =  mean(export_ratio, na.rm =T),
               median_deflated_value = median(deflated_value, na.rm =T),
               mean_deflated_value = mean(deflated_value, na.rm =T)), by =.(interest, year_in_streak)]
  
  
  ## add normalization
  data = merge(data, data %>% filter(year_in_streak == 0) %>% 
         rename_with(~paste0(., "_first") ,-c(interest, year_in_streak)) %>%
         select(-year_in_streak),by = 'interest', all.x = T) %>%
         mutate(median_export_ratio_normalized = ((median_export_ratio / median_export_ratio_first)-1)*100)
  
  median_export_ratio = ggplot(data, aes(x = year_in_streak +1, y = median_export_ratio,
                                         color = as.factor(interest))) + geom_line() + theme(legend.position = 'none') +
    labs(subtitle = 'Export Ratio over Time (condit. on survival)',y = 'Export Rev / Domestic Rev', x = 'year in streak')
  

  median_export_ratio_normalized = ggplot(data, aes(x = year_in_streak +1, y = median_export_ratio_normalized,
                                                    color = as.factor(interest))) + geom_line() +
    labs(subtitle = 'Export Ratio Growth (condit. on survival)',x = "year in streak", y = '% Change from Year 1', color = 'quartile')
  
  graph = survival_plot +  median_export_ratio + median_export_ratio_normalized + plot_annotation(title = paste0('Quartile From: ', titl))
  ggsave(paste0('../3) output/3) revenue growth analysis/',output,'.png'),graph, width= 15, height = 5)
  return(graph)
}
for (i in 1:18){
  starter_var =T; observation_period = 4; quartile = T; expansion = T
  interest_var = graph_inputs$varname[i]; titl= graph_inputs$title[i]; output = graph_inputs$output_name[i]
  print(interest_var)
  breakdown_graphs(interest_var, starter_var, observation_period,quartile, titl, paste0(i,"_",output), expansion)
  
  if (grepl('rev',output)){ 
    print(paste0(interest_var, "_lag"))
    breakdown_graphs(paste0(interest_var, "_lag"), starter_var, observation_period,quartile, titl, paste0(i,"a_",output, "_lag"), expansion)
  }
}


# analyze failure rates ---------------------------------------------------
firm_level_input_data = fread('../1) data/13a_firm_level_growth_analysis_inputs.csv')[birth_observed == 1]
firm_level_input_data[,country_arrival := min(streak_start, na.rm = T), by = .(ctry, firmid)]
firm_level_input_data = firm_level_input_data[country_arrival == streak_start & year_in_streak == 0 & year >= 2010]

interest_vars = c('ctry_streak_age_quartile_annual_lag', 'ctry_num_firms_quartile_annual_lag',
                  'ctry_gdp_quartile_annual_lag', 'ctry_deflated_value_quartile_annual_lag',
                  'ctry_population_quartile_annual_lag')
output = rbindlist(lapply(interest_vars, function(interest_var){
data =firm_level_input_data

data[,quartile_value := get(interest_var)]
data[, .(failure_rate = round(100*mean(streak_length==0),2)), by = quartile_value] %>% mutate(type =str_remove(interest_var, '_quartile_annual'))
})) %>% filter(!is.na(quartile_value)) %>% arrange(type, quartile_value)


ggplot(output, aes(x = type, y = failure_rate, fill = as.factor(quartile_value))) + geom_bar(stat = 'identity', position = 'dodge')

