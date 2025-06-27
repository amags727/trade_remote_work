# setup
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','ggplot2', 'patchwork','fixest')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_helper_functions.R")

# delay into exporting ---------------------------------------------------
export_data = fread('../1) data/6_customs_cleaned.csv') %>% 
  filter( birth_year < 2012, exim == 2)              


delay_into_exporting = export_data %>% mutate(delay = first_export_year - birth_year) %>% 
  select(firmid, birth_year, delay) %>% unique() %>% tabyl(delay) %>%
  mutate(cum = cumsum(percent), level = 'first entry')


foreign_market_growth = export_data[, .(first_export_year = min(year)), by = .(birth_year, ctry, firmid)] %>% 
  mutate(delay = first_export_year - birth_year) %>% select(firmid, birth_year, ctry, delay) %>%
  unique() %>% tabyl(delay) %>% mutate(cum = cumsum(percent), level = 'overall entry')

rbind(delay_into_exporting, foreign_market_growth) %>% fwrite(.,'../3) output/delay_output.csv')


# compare first and second markets ----------------------------------------
export_data_og = fread('../1) data/6_customs_cleaned.csv') %>% 
  filter(birth_year < 2015, exim == 2)            

#select on firms that expand into two countries
markets_examined = 3; cutoff = 9
export_data = export_data_og[, if (uniqueN(ctry) >= markets_examined) .SD, by = firmid]

# demarcate the year a country enters into the mark; filter to only get initial streak 
export_data[, entry_year := min(year, na.rm = T), by = .(firmid, ctry)]
export_data = export_data[ entry_year == streak_start]
export_data[, streak_length := max(year_in_streak), by = streak_id]
# number the firms' entrance into new markets 
country_rankings = export_data[year == entry_year, .(ctry, entry_year, rank = frank(entry_year, ties.method = 'first')), by = firmid]
export_data = merge(export_data, country_rankings)

export_data = export_data[rank <= markets_examined, long_enough_streaks := min(streak_length) > cutoff, by = firmid]

graph_inputs_firm = export_data[long_enough_streaks == T & year_in_streak <= (cutoff +1),
                           .(deflated_value_med = median(na.rm =T, deflated_value),
                             deflated_value_mean = mean(na.rm =T, deflated_value)), by = .(rank, year_in_streak)]
ggplot(graph_inputs_firm, aes(x = year_in_streak, y = deflated_value_med, color = as.factor(rank))) + geom_line() 



# compare first and second products ---------------------------------------
product_export_og = fread('../1) data/6a_product_export_streaks.csv') 

# filter out markets where we don't know when the firm initially entered
product_export = product_export_og[,product_entry_year := min(year, na.rm = T), by = .(firmid, ctry,CN8plus)]
product_export = product_export[,country_entry_year := min(year, na.rm = T), by = .(firmid, ctry)]

products_examined = 3; cutoff = 3
product_export = product_export[, if (uniqueN(CN8plus) >= products_examined) .SD, by = .(firmid,ctry)]
product_rankings = product_export[year == product_entry_year,
                                  .(CN8plus, product_entry_year, rank = frank(product_entry_year, ties.method = 'first')),
                                  by = .(firmid,ctry)]
product_export = merge(product_export, product_rankings)
product_export[, streak_length := max(year_in_streak), by = streak_id]
product_export = product_export[ rank <= products_examined, long_enough_streaks := min(streak_length) > cutoff, by = firmid]
graph_inputs = product_export[long_enough_streaks == T & year_in_streak <= (cutoff +1),
                            .(deflated_value_med = median(na.rm =T, deflated_value),
                              deflated_value_mean = mean(na.rm = T, deflated_value)), by = .(rank, year_in_streak)]
ggplot(graph_inputs, aes(x = year_in_streak, y = deflated_value_mean, color = as.factor(rank))) + geom_line() 





# revenue growth and variance; firm-country-level ---------------------------------------------------
revenue_data = fread('../1) data/8_revenue_prediction_inputs.csv') 
revenue_streak = revenue_data %>% mutate(export_ratio = deflated_value / (deflated_dom_turnover*1000)) %>% filter(export_ratio != Inf)
revenue_streak = revenue_streak[, streak_length := NA_max(year_in_streak), by = streak_id]

streak_counts = revenue_streak %>% filter(!is.na(streak_length))  %>% group_by(streak_length) %>%
  summarize(num_streaks_eq_x = n()) %>% arrange(-streak_length) %>%
  mutate(num_streaks_geq_x = cumsum(num_streaks_eq_x),
         share_streaks_eq_x =num_streaks_eq_x/ sum(num_streaks_eq_x),
         share_streaks_geq_x = num_streaks_geq_x /sum(num_streaks_eq_x)) %>%
  rename(streak_restriction = streak_length)

model = feglm(data = revenue_streak, deflated_value ~ 1 | year + ctry + firmid, family = 'poisson')
revenue_streak = revenue_streak %>% mutate(deflated_value_resid = deflated_value - predict(model, newdata = .))
revenue_output = rbindlist(lapply(-1:NA_max(revenue_streak$streak_length), function(i){
  print(i)
  if (i == -1){
    restricted = revenue_streak} else{
  restricted = revenue_streak[streak_length >= i & year_in_streak <= i]
    }
  restricted = restricted %>% group_by(year_in_streak) %>%
    summarize(across(c(value, deflated_value, deflated_value_resid, export_ratio),~NA_median(.), .names = 'med_{col}'),
              across(c(value, deflated_value,deflated_value_resid, export_ratio),~NA_mean(.), .names = 'mean_{col}'),
              across(c(value, deflated_value, deflated_value_resid),~NA_IQR(.), .names = 'IQR_{col}'),
              across(c(value, deflated_value, deflated_value_resid),~NA_sd(.), .names = 'sd_{col}'),
              across(c(value, deflated_value, deflated_value_resid),~NA_coef_var(.), .names = 'coef_var_{col}')) %>%
    mutate(streak_restriction = i)
})) 


revenue_output = merge(revenue_output,streak_counts, by = 'streak_restriction', all.x = T)
write.csv(revenue_output,'../3) output/streak_rev_output_firm_lev.csv', row.names = F)


# revenue growth and variance; firm-country-product-level ---------------------------------------------------
product_export = fread('../1) data/6a_product_export_streaks.csv', colClasses = list(character= 'firmid'))
bs_data = fread( '../1) data/1_bs_data.csv', select = c('firmid', 'year', 'deflated_dom_turnover'))
product_export = merge(product_export,bs_data, all.x= T) %>% mutate(export_ratio = deflated_value / (deflated_dom_turnover*1000))

product_streak_counts = product_export %>% filter(!is.na(streak_length))  %>% group_by(streak_length) %>%
  summarize(num_streaks_eq_x = n()) %>% arrange(-streak_length) %>%
  mutate(num_streaks_geq_x = cumsum(num_streaks_eq_x),
         share_streaks_eq_x =num_streaks_eq_x/ sum(num_streaks_eq_x),
         share_streaks_geq_x = num_streaks_geq_x /sum(num_streaks_eq_x)) %>%
  rename(streak_restriction = streak_length) 

#model = feglm(data = product_export, deflated_value ~ 1 | year + ctry + firmid + hs_class, family = 'poisson')
#product_export = product_export %>% mutate(deflated_value_resid = deflated_value - predict(model, newdata = .))
product_export = product_export %>% mutate(deflated_value_resid = deflated_value)

product_export_streaks = rbindlist(lapply(-1:NA_max(product_export$streak_length), function(i){
  print(i)
  if (i == -1){
    restricted =  product_export} else{
      restricted = product_export[streak_length >= i & year_in_streak <= i]
    }
  restricted = restricted %>% group_by(year_in_streak) %>%
    summarize(across(c(value, deflated_value, deflated_value_resid, export_ratio),~NA_median(.), .names = 'med_{col}'),
              across(c(value, deflated_value, deflated_value_resid, export_ratio),~NA_mean(.), .names = 'mean_{col}'),
              across(c(value, deflated_value, deflated_value_resid, export_ratio),~NA_sd(.), .names = 'sd_{col}'),
              across(c(value, deflated_value, deflated_value_resid, export_ratio),~NA_coef_var(.), .names = 'coef_var_{col}')) %>%
    mutate(streak_restriction = i)
}))
product_output = merge(product_export_streaks,product_streak_counts, by = 'streak_restriction', all.x = T)

write.csv(product_output,'../3) output/streak_rev_output_product_lev.csv', row.names = F)

# make firm level graphs  ------------------------------------------------------------
delay_output = fread('../3) output/delay_output.csv')

entry_graphs= ggplot(data = delay_output %>% filter(level == 'first entry'), aes(x = delay, y = cum)) + geom_line(color = 'blue') + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + xlim(0, 10) + 
  labs(title = 'Time to First Foreign Market Entry', x = 'years', y = 'share of firms') +
  ggplot(data = delay_output %>% filter(level == 'overall entry'), aes(x = delay, y = cum)) + geom_line(color = 'red') + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + xlim(0, 10) + 
  labs(title = 'Total Markets Entered', x = 'years', y = 'share of eventual markets') 

ggsave('../3) output/graphs/delay_output_graph.png', entry_graphs, width = 8, height = 4)


revenue_output = fread('../3) output/streak_rev_output_firm_lev.csv')
ymax_1 = (revenue_output %>% filter(streak_restriction < 11, streak_restriction != -1) %>% pull(med_deflated_value) %>% NA_max(.))/1000
rev = ggplot(revenue_output %>% filter(streak_restriction == -1, year_in_streak <11),
       aes(x = year_in_streak, y = med_deflated_value/ 1000)) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) + ylim(0, ymax_1) +
  labs(title = element_blank(),subtitle = 'Aggregated across export spell durations' , y= 'Market Revenue (thousands)', x = 'Year of Export Spell')

rev_disagg = ggplot(revenue_output %>% filter(streak_restriction < 11,  streak_restriction>=0),
       aes(x = year_in_streak, y = med_deflated_value/1000, color = as.factor(streak_restriction))) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) + ylim(0, ymax_1) +
  labs(title = element_blank(), subtitle = 'Dissagreggated by export spell duration', x = 'Year of Export Spell', y = '') + theme(legend.position = 'none')
composite_rev = rev+ rev_disagg +plot_annotation(title = '       Firm Revenue Growth over Time')
ggsave('../3) output/graphs/firm_rev_growth.png', composite_rev, width = 10, height = 5)


ymax_2 = (revenue_output %>% filter(streak_restriction < 11, streak_restriction != -1) %>% pull(med_export_ratio) %>% NA_max(.))
export_ratio = ggplot(revenue_output %>% filter(streak_restriction == -1, year_in_streak <11),
       aes(x = year_in_streak, y = med_export_ratio)) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, ymax_2)) +
  labs(title = element_blank(), subtitle = 'Aggregated across export spell durations', y = 'exports / dom. rev', x ='Year of Export Spell')

export_ratio_disagg = ggplot(revenue_output %>% filter(streak_restriction < 11,  streak_restriction>=0),
       aes(x = year_in_streak, y = med_export_ratio, color = as.factor(streak_restriction))) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, ymax_2)) +
  labs(title = element_blank(), subtitle = 'Dissagreggated by export spell duration', y = '', x ='Year of Export Spell') +
  theme(legend.position = 'none')
composite_export_ratio = export_ratio+ export_ratio_disagg +plot_annotation(title = '          Export to Domestic Revenue Ratio')
ggsave('../3) output/graphs/firm_export_ratio.png', composite_export_ratio, width = 10, height = 5)


ymax_3 = (revenue_output %>% filter(streak_restriction < 11, streak_restriction != -1) %>% pull(coef_var_deflated_value) %>% NA_max(.))
coef_var = ggplot(revenue_output %>% filter(streak_restriction == -1, year_in_streak <11),
       aes(x = year_in_streak, y = coef_var_deflated_value)) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) +ylim(0,ymax_3) + 
  labs(title = element_blank(), subtitle = 'Aggregated across export spell durations', y = 'Coef. of Variation Export Revenue', x= 'Year of Export Spell')


coef_var_disagg = ggplot(revenue_output %>% filter(streak_restriction < 11,  streak_restriction>=0),
       aes(x = year_in_streak, y = coef_var_deflated_value, color = as.factor(streak_restriction))) +
  geom_line()+scale_x_continuous(breaks = seq(0,10,2)) +ylim(0,ymax_3) + theme(legend.position = 'none') +
  labs(title = element_blank(), subtitle = 'Dissagreggated by export spell duration', y = '', x= 'Year of Export Spell')
composite_coef_var = coef_var + coef_var_disagg +plot_annotation(title = '        Variability of Export Revenue')
ggsave('../3) output/graphs/firm_coef_var.png', composite_coef_var, width = 10, height = 5)




# make product level graphs -----------------------------------------------
revenue_output = fread('../3) output/streak_rev_output_product_lev.csv')


ymax_1 = (revenue_output %>% filter(streak_restriction < 11, streak_restriction != -1) %>% pull(med_deflated_value) %>% NA_max(.))/1000
rev = ggplot(revenue_output %>% filter(streak_restriction == -1, year_in_streak <11),
             aes(x = year_in_streak, y = med_deflated_value/ 1000)) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) + ylim(0, ymax_1) +
  labs(title = element_blank(),subtitle = 'Aggregated across export spell durations' , y= 'Market Revenue (thousands)', x = 'Year of Export Spell')

rev_disagg = ggplot(revenue_output %>% filter(streak_restriction < 11,  streak_restriction>=0),
                    aes(x = year_in_streak, y = med_deflated_value/1000, color = as.factor(streak_restriction))) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) + ylim(0, ymax_1) +
  labs(title = element_blank(), subtitle = 'Dissagreggated by export spell duration', x = 'Year of Export Spell', y = '') + theme(legend.position = 'none')
composite_rev = rev+ rev_disagg +plot_annotation(title = '       Product Revenue Growth over Time')
ggsave('../3) output/graphs/product_rev_growth.png', composite_rev, width = 10, height = 5)


ymax_2 = (revenue_output %>% filter(streak_restriction < 11, streak_restriction != -1) %>% pull(med_export_ratio) %>% NA_max(.))
export_ratio = ggplot(revenue_output %>% filter(streak_restriction == -1, year_in_streak <11),
                      aes(x = year_in_streak, y = med_export_ratio)) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, ymax_2)) +
  labs(title = element_blank(), subtitle = 'Aggregated across export spell durations', y = 'exports / dom. rev', x ='Year of Export Spell')

export_ratio_disagg = ggplot(revenue_output %>% filter(streak_restriction < 11,  streak_restriction>=0),
                             aes(x = year_in_streak, y = med_export_ratio, color = as.factor(streak_restriction))) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, ymax_2)) +
  labs(title = element_blank(), subtitle = 'Dissagreggated by export spell duration', y = '', x ='Year of Export Spell') +
  theme(legend.position = 'none')
composite_export_ratio = export_ratio+ export_ratio_disagg +plot_annotation(title = '           Product Exports to Domestic Revenue Ratio')
ggsave('../3) output/graphs/product_export_ratio.png', composite_export_ratio, width = 10, height = 5)



ymax_3 = (revenue_output %>% filter(streak_restriction < 11, streak_restriction != -1) %>% pull(coef_var_deflated_value) %>% NA_max(.))
coef_var = ggplot(revenue_output %>% filter(streak_restriction == -1, year_in_streak <11),
                  aes(x = year_in_streak, y = coef_var_deflated_value)) + geom_line()+scale_x_continuous(breaks = seq(0,10,2)) +ylim(0,ymax_3) + 
  labs(title = element_blank(), subtitle = 'Aggregated across export spell durations', y = 'Coef. of Variation Export Revenue', x= 'Year of Export Spell')


coef_var_disagg = ggplot(revenue_output %>% filter(streak_restriction < 11,  streak_restriction>=0),
                         aes(x = year_in_streak, y = coef_var_deflated_value, color = as.factor(streak_restriction))) +
  geom_line()+scale_x_continuous(breaks = seq(0,10,2)) +ylim(0,ymax_3) + theme(legend.position = 'none') +
  labs(title = element_blank(), subtitle = 'Dissagreggated by export spell duration', y = '', x= 'Year of Export Spell')
composite_coef_var = coef_var + coef_var_disagg +plot_annotation(title = '        Variability of Product Export Revenue')
ggsave('../3) output/graphs/product_coef_var.png', composite_coef_var, width = 10, height = 5)



