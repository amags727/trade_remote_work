# setup -------------------------------------------------------------------
rm(list = ls()); gc();

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




# make extended version of data for variance purposes ------------------------------------------
making_extended = T; start_year = 2000;
extended_firm_yr_path =  '1) data/14_extended_firm_yr_variance_dta.parquet'
if (dummy_version) extended_firm_yr_path = gsub('1) data', '1a) dummy data', extended_firm_yr_path)
if (making_extended  == T){

main_sample_firms = import_file(firm_yr_path, col_select = 'firmid_num') %>% unique() %>% pull(firmid_num)
bs_data = import_file(raw_bs_br_path,col_select = c('firmid_num', 'year','for_turnover', 'turnover')) %>% 
  .[, log_total_export_rev_BS := asinh(for_turnover)] %>% .[year >= start_year]

export_data = import_file(raw_customs_firm_lvl_path, col_select = c('firmid_num', 'year', 'exim', 'value')) %>%
  .[exim == 2 & year >= start_year] %>% .[,exim := NULL] %>%
  .[,.(log_total_export_rev_customs = asinh(sum(value, na.rm = T))), by = .(firmid_num, year)] %>%
  .[year!=2021]

extended_data = merge(bs_data, export_data, all = T, by = c('firmid_num', 'year')) %>% 
  .[,log_total_export_rev_customs := replace_na(log_total_export_rev_customs, 0)]  %>% 
  
  # add age data 
  merge(import_file(firm_lvl_birth_path, col_select = c('firmid_num', 'birth_year')), all.x=T, by = 'firmid_num') %>%
  .[,age := year - birth_year] %>% 
  merge(import_file(firm_lvl_export_birth_path), all.x =T, by = c('firmid_num', 'year')) %>% 
  .[, `:=`(log_age = asinh(age), log_export_streak_age_BS = asinh(export_streak_age_BS),
           log_export_streak_age_customs = asinh(export_streak_age_customs))] %>%
    
  # mark whether firm in main sample 
  .[, main_sample_firm := firmid_num %in% main_sample_firms]

## add detrended variance values 
suffixes = c('BS', 'customs')
if (dummy_version){ ## the regressions will fail if we use dummy data 
  detrended_vars = gpaste(c('main_sample_', ''),'log_total_export_rev_',suffixes, c('', '_cond'),'_detrended_var')
  extended_data[, (detrended_vars) := runif(.N)]
}else{
  for(sample_restriction in c('', '[main_sample_firm==T]')){
  for( suffix in suffixes){
    command_1 = gpaste('feols(extended_data',sample_restriction,', log_total_export_rev_',suffix, '~log_age| firmid_num + year)')
    command_2 = gsub('log_age', gpaste('log_export_streak_age_', suffix), command_1)
    models = list(eval(parse(text = command_1)), eval(parse(text = command_2)))
    for (i in 1:2){
      var_name = gpaste(ifelse(sample_restriction !='','main_sample_',''),'log_total_export_rev_',suffix, ifelse(i==1, '', '_cond'), "_detrended_var")
      non_dropped_obs = setdiff(1:nrow(extended_data),-1*models[[i]]$obs_selection$obsRemoved)
      if(sample_restriction !=''){
        dropped_rows = -1*models[[i]]$obs_selection$obsRemoved;
        non_dropped_obs = data.table(potential_rows = extended_data %>% mutate(idx = 1:nrow(.)) %>% .[main_sample_firm==T] %>% pull(idx)) %>%
          mutate(idx = 1:nrow(.)) %>% .[!idx %in% dropped_rows] %>% pull(potential_rows)
      }
      extended_data[non_dropped_obs, (var_name) := models[[i]]$residuals^2]
    }
  }
  }
}

write_parquet(extended_data, extended_firm_yr_path)
}


# import data and set up for analysis -----------------------------------
base_data <- import_file(firm_yr_path)
extended_data = import_file(extended_firm_yr_path)
# setup output dir 
var_output_dir = paste0(finished_output_dir,'5_var_over_time')
suppressWarnings(dir.create(var_output_dir, recursive = T))



# 5a base graph  -----------------------------------------------------------------------
dep_vars = gpaste('log_total_export_rev_', c('BS', 'customs'), c('', '_cond'), '_detrended_var')

# graph_inputs = base_data[,c(
graph_inputs = extended_data[,c(
  setNames(lapply(.SD[, ..dep_vars], NA_mean),dep_vars),
  setNames(lapply(.SD[, ..dep_vars], function(x) NA_sum(x*turnover) /NA_sum(turnover)) ,
                  paste0('wgted_',dep_vars))), by = year] %>% 
  pivot_longer(cols = -year, names_to = 'var') %>% 
  as.data.table() %>%  mutate(wgted = grepl('wgted',var),
                              var_name = paste0(ifelse(grepl("BS", var), 'BS', 'Customs'),
                                                ifelse(grepl('cond',var), " cond", '')))  

  graph_output = lapply(c(F,T), function(wgt_yn){
    graph = graph_inputs[wgted == wgt_yn] %>% 
      ggplot(aes(x = year, y =value, color = var_name)) + geom_line() + theme_minimal() + 
      labs(y = element_blank(), x = element_blank(), subtitle = 'Weighted by Firm Size', color = 'Variance Metric') +
      scale_x_continuous(breaks = seq(start_year, max(graph_inputs$year), 2)) 
    if (wgt_yn ==F) graph = graph + labs(y =  'log export varaince', subtitle = 'Unweighted') +
        theme(legend.position = 'none') 
    return(graph)
  })
  graph_output = graph_output[[1]] + graph_output[[2]] + plot_annotation(title = 'Detrended Log Export Variance over Time')
  ggsave(paste0(var_output_dir, '/5a_base_graph.png'), graph_output, width = 10.3, height = 5.2)


# 5b analysis of export streak age over sample period  ----------------------------------------------------------------------

# setup output dir 
streak_output_dir = paste0(finished_output_dir,'6_export_streak_over_time')
suppressWarnings(dir.create(var_output_dir, recursive = T))
  
  
probs <- c(0.05, 0.25, 0.50, 0.75, 0.95); p_labels <- sprintf("%02d", probs * 100)
export_streak_info <- melt(extended_data,id.vars = "year",
  measure.vars = c("export_streak_age_BS", "export_streak_age_customs"),
  variable.name = "variable",
  value.name = "value")

export_streak_info = rbindlist(lapply(probs, function(prob){
  export_streak_info[, .(value = quantile(value, prob, na.rm = T)), by =.(variable, year)] %>% .[,percentile_value := prob]
}))

graph_output= lapply(unique(export_streak_info$variable), function(var){
  ggplot(export_streak_info[variable==var], aes(x = year, y =value, color = as.factor(percentile_value))) + geom_line() + theme_minimal() + 
    labs(y = "Streak Age", x = element_blank(), subtitle = if(grepl("BS", var)) "BS" else "Customs", color = 'Percentile') +
    scale_x_continuous(breaks = seq(start_year, max(graph_inputs$year), 2)) 
  
})

graph_output = graph_output[[1]]+   theme(legend.position = 'none')  + graph_output[[2]] + plot_annotation(title = 'Export Streak over Time')
ggsave(paste0(streak_output_dir, '/6_base_graph.png'), graph_output, width = 10.3, height = 5.2)






