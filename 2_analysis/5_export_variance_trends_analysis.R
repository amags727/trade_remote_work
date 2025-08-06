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
# import data and set up for analysis -----------------------------------
base_data <- import_file(firm_yr_path)

# setup output dir 
var_output_dir = paste0(finished_output_dir,'5_var_over_time')
suppressWarnings(dir.create(var_output_dir, recursive = T))



# 5a base graph  -----------------------------------------------------------------------
dep_vars = gpaste('log_total_export_rev_', c('BS', 'customs'), c('', '_cond'), '_detrended_var')

graph_inputs = base_data[,c(
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
    scale_x_continuous(breaks = seq(min(year_range), max(year_range), 2)) 
  if (wgt_yn ==F) graph = graph + labs(y =  'log export varaince', subtitle = 'Unweighted') +
      theme(legend.position = 'none') 
  return(graph)
})
graph_output = graph_output[[1]] + graph_output[[2]] + plot_annotation(title = 'Detrended Log Export Variance over Time')
ggsave(paste0(var_output_dir, '/5a_base_graph.png'), graph_output, width = 10.3, height = 5.2)

  







