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



# 0) Delay into exporting -------------------------------------------------
export_data = fread('../1) data/13a_firm_level_growth_analysis_inputs.csv') 
export_data = export_data[birth_year < 2012 & birth_observed ==1][,delay := first_export_year - birth_year]          

delay_into_exporting = export_data %>% select(firmid, birth_year, delay) %>%
                       unique() %>% tabyl(delay) %>% mutate(cum = cumsum(percent), level = 'first entry')


foreign_market_growth = export_data[, .(first_export_year = min(year)), by = .(birth_year, ctry, firmid)] %>% 
                        mutate(delay = first_export_year - birth_year) %>% select(firmid, birth_year, ctry, delay) %>%
                        unique() %>% tabyl(delay) %>% mutate(cum = cumsum(percent), level = 'overall entry')

graph_input = rbind(delay_into_exporting, foreign_market_growth)

entry_graphs= ggplot(graph_input %>% filter(level == 'first entry'), aes(x = delay, y = cum)) + geom_line(color = 'blue') + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + xlim(0, 10) + 
  labs(title = 'Time to First Foreign Market Entry', x = 'years', y = 'share of firms') +
  ggplot(graph_input %>% filter(level == 'overall entry'), aes(x = delay, y = cum)) + geom_line(color = 'red') + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + xlim(0, 10) + 
  labs(title = 'Total Markets Entered', x = 'years', y = 'share of eventual markets') 

ggsave('../3) output/3) revenue growth analysis/00_entry_results.png' ,entry_graphs, width = 8, height =  4)

# 1) model analysis  ---------------------------------------------------------
num_trials = 1e3; num_periods = 1e3; set.seed(1)
hi = as.data.frame(matrix(rnorm(num_trials*num_periods, 0,1.5), nrow =num_periods)) %>% mutate_all(~cumsum(.))

streak_length = lapply(1:num_trials, function(i){
  streak_length =  which(hi[,i] <= -3)[1]
  if (is.na(streak_length)){streak_length = num_periods}  
  return(streak_length)
}) %>% unlist()

combined_data =  hi %>% mutate(period = 1:num_periods) %>% pivot_longer(., cols = -period) %>% mutate(trial = as.numeric( str_remove(name,'V'))) %>%
  merge(., data.frame(streak_length = streak_length, trial = 1:num_trials)) %>% filter(period <= streak_length) %>% as.data.table()

exiters_input =combined_data %>% mutate(streak_bucket =  cut(streak_length, breaks = seq(0,1000, by = 100),
                                                             include.lowest = T, right =F,
                                                             labels =  paste0(seq(0,900, by = 100),'-', seq(100,1000, by = 100)))) %>%
  group_by(streak_bucket, period) %>% summarize(value = mean(value))

exiters_graph = ggplot(exiters_input, aes(x = period, y = value, color = streak_bucket)) + geom_line() +
                labs(color = 'export spell\nlength', x = 'time', title = 'Revenue of Firms by Spell-Length', subtitle = 'Exiters')


exiters_plus_continuers_input = lapply(seq(100,1000, by = 100), function(base_value){
  temp = combined_data[streak_length >= base_value & period <= base_value] %>% 
    group_by(period) %>% summarize(value = mean(value)) %>% mutate(streak_bucket = paste0(base_value,"+"))
}) %>% rbindlist() %>% mutate(streak_bucket = as.factor(streak_bucket))

exiters_plus_continuers_graph = ggplot(exiters_plus_continuers_input, aes(x = period, y = value, color = streak_bucket)) +
                                geom_line() + labs(color = 'export spell\nlength', x = 'time', 
                                                   title = 'Revenue of Firms by Spell-Length', subtitle = 'Exiters + Continuers')

survivor_input = data.frame(period = 1:num_periods, survivor_data = lapply(1:num_periods, function(i){return(sum(streak_length >=i) / num_trials)
}) %>% unlist())
survivor_graph = ggplot(survivor_input, aes(x = period, y= survivor_data)) + geom_line() + scale_y_continuous(limits = c(0,1)) + 
  labs(x = 'time', y= '% firms still active', title = 'Firm Survival Curve')

ggsave('../3) output/3) revenue growth analysis/00_model_predictions.png' ,survivor_graph + exiters_plus_continuers_graph + exiters_graph, width = 12, height = 4 )

# 2) Baseline Analysis ---------------------------------------------------
firm_level_input_data = fread('../1) data/13a_firm_level_growth_analysis_inputs.csv')
data = firm_level_input_data
data = data[ streak_birth_observed==T]

baseline_output = lapply(c('exiter', 'exiter_plus_continuer'),function(input){
  lapply(1:10, function(yr){
    if (input == 'exiter'){
        temp = data[year_in_streak <= yr & streak_length == yr & streak_death_observed == T]} else{
        temp = data[year_in_streak <= yr & streak_length >= yr]}
    
    temp = temp[,.(median_export_ratio = median(export_ratio, na.rm = T),
                   mean_export_ratio  =  mean(export_ratio, na.rm =T),
                   median_deflated_value = median(deflated_value, na.rm =T),
                   mean_deflated_value = mean(deflated_value, na.rm =T)), by =.(year_in_streak)] 
    temp = temp %>% mutate(cohort = yr, type = input)
  }) %>% rbindlist()}) %>% rbindlist()


survival_data = firm_level_input_data[year_in_streak == 1]; total_streaks = nrow(survival_data)
survival_data = lapply(1:10, function(yr){
  data.frame(year = yr, survival_share = nrow(survival_data[streak_length >= yr])/ total_streaks)
}) %>% rbindlist()

survival_graph_data = ggplot(survival_data, aes(x = year, y = 100*survival_share)) + geom_line() +
  labs(x = 'years in market', y= '% firms active', title = 'Firm Survival Curve') +ylim(0,100)

exiter_graph_data = ggplot(data = baseline_output %>% filter(type == 'exiter'), aes(x = year_in_streak, y = median_export_ratio/1000, color = as.factor(cohort+1))) + geom_line() + 
  scale_x_continuous(breaks = seq(1,10)) +labs(color = 'export spell\nlength', x = 'years in market', y= element_blank(), title = 'Revenue of Firms by Spell-Length', subtitle = 'Exiters') +
  scale_y_continuous(limits = c(0,.012))
exiter_continuer_graph_data = ggplot(data = baseline_output %>% filter(type == 'exiter_plus_continuer'), aes(x = year_in_streak, y = median_export_ratio/1000, color = as.factor(cohort+1))) + geom_line() + 
  scale_x_continuous(breaks = seq(1,10)) + labs( x = 'years in market',  y= 'foreign / domestic revenue',
                                                title = 'Revenue of Firms by Spell-Length', subtitle = 'Exiters + Continuers') +
  theme(legend.position = 'none') +  scale_y_continuous(limits = c(0,.012))

ggsave('../3) output/3) revenue growth analysis/01_empirical_results.png' ,survival_graph_data + exiter_graph_data +  exiter_continuer_graph_data , width = 12, height =  4)

# 3) Firm-Industry Level Analysis ---------------------------------------------------
firm_level_input_data = fread('../1) data/13a_firm_level_growth_analysis_inputs.csv')
graph_inputs = read_excel("3a_growth_delays_inputs_and_analysis.xlsx")

## Run the Firm Industry Level Analysis 
breakdown_graphs = function(interest_var, observation_period, quartile, titl,output){
  #choose data frame 
  if( str_ends(output, '_firm') | str_ends(output, '_firm_lag')){
  data = firm_level_input_data}else{ data = product_level_input_data}
  data = data[streak_birth_observed == 1]
 
  
  # assign to the who streak the value from the first year 
  clipped = data[streak_start == year]; clipped[,interest := as.factor(get(interest_var))];
  clipped = clipped[,.(interest,streak_id)]; data = merge(data, clipped, by = 'streak_id')
  
  # restrict focus to firms that we fully observe for the entire period (both domestic and international revs)
  data = data[year_in_streak <= observation_period & !is.na(interest)]
  data[, always_observed := all(!is.na(export_ratio)), by = streak_id]
  data = data[always_observed==T]
  
  # generate stats on survival
  survival_stats = data[deflated_value > 0 & streak_death_observed]; 
  survival_stats = survival_stats[, .(count = .N), by = .(year_in_streak, interest)]
  survival_stats[, share_survive := 100*count / max(count), by = interest]

  # conditional upon survival look at the growth of the median firm 
  data = data[streak_length >= observation_period,
             .(median_export_ratio = median(export_ratio, na.rm = T),
               mean_export_ratio  =  mean(export_ratio, na.rm =T),
               median_deflated_value = median(deflated_value, na.rm =T),
               mean_deflated_value = mean(deflated_value, na.rm =T)), by =.(interest, year_in_streak)]
  
  
  ## add normalization to look at growth 
  data = merge(data, data %>% filter(year_in_streak == 1) %>% 
         rename_with(~paste0(., "_first") ,-c(interest, year_in_streak)) %>%
         select(-year_in_streak),by = 'interest', all.x = T) %>%
         mutate(median_export_ratio_normalized = ((median_export_ratio / median_export_ratio_first)-1)*100)
  
  ## generate graphs
  survival_plot = ggplot(data = survival_stats, aes(x = year_in_streak , y = share_survive, color = interest)) + geom_line() +
                  labs(subtitle = 'Survival Rate of Firms over Time', y= '% Firms Remaining', x = 'year in streak') +
                  theme(legend.position = 'none')
  
  median_export_ratio = ggplot(data, aes(x = year_in_streak, y = median_export_ratio,
                        color = as.factor(interest))) + geom_line() + theme(legend.position = 'none') +
                        labs(subtitle = 'Export Ratio over Time (condit. on survival)',
                        y = 'Export Rev / Domestic Rev', x = 'year in streak')
  
  median_export_ratio_normalized = ggplot(data, aes(x = year_in_streak, y = median_export_ratio_normalized,
                                   color = as.factor(interest))) + geom_line() +
                                   labs(subtitle = 'Export Ratio Growth (condit. on survival)',
                                   x = "year in streak", y = '% Change from Year 1', color = 'quartile')
  graph = survival_plot +  median_export_ratio + median_export_ratio_normalized + plot_annotation(title = paste0('Quartile From: ', titl))
  
  #export
  ggsave(paste0('../3) output/3) revenue growth analysis/',output,'.png'),graph, width= 15, height = 5)
}
for (i in 1:18){
  starter_var =T; observation_period = 4; quartile = T; expansion = T
  interest_var = graph_inputs$varname[i]; titl= graph_inputs$title[i]; output = graph_inputs$output_name[i]
  print(interest_var)
  breakdown_graphs(interest_var, observation_period,quartile, titl, paste0(i,"_",output))
  
  if (grepl('rev',output) | grepl('streak_age',output)){ 
    print(paste0(interest_var, "_lag"))
    breakdown_graphs(paste0(interest_var, "_lag"), observation_period,quartile, titl, paste0(i,"a_",output, "_lag"))
  }
}




