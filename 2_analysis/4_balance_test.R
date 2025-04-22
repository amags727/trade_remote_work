# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest', 'kableExtra', 'survival', 'scales')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')}
source("2) code/00_helper_functions.R")
output_dir = '3) output/4) data_summary_stats'
exporting_files = F
inputs_dir = ('1) data/16_inputs_for_data_summary_stats')
base_env = c(ls(),'base_env')


# import data  -------------------------------------------------------------------
firm_yr_lvl = import_file(file.path(inputs_dir, '16g_firm_yr_level_summ_stats_inputs.parquet'))


# cross_quartile graph  ---------------------------------------------------
cross_quartile_graph = firm_yr_lvl[,.(count = .N), by = .(quartile_comp_data, quartile_share_comp_data)] %>%
  na.omit() %>% mutate(share = count / sum(count)) %>% 
  ggplot(., aes(x = quartile_comp_data, y = share,fill = quartile_share_comp_data)) +
  geom_bar(stat = "identity") + 
  labs(fill = 'share data\ncompensation',
       x = 'level data compensation',
       y = element_blank(),
       title = 'Data Compensation\nCross Quartile Correspondences') +
  scale_y_continuous(labels = percent) + theme_minimal()







