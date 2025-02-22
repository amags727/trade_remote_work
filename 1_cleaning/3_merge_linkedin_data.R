# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow','fixest')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../00_helper_functions.R")
setwd('../..')

dummy_version = T
set.seed(42)
# import ------------------------------------------------------------------
linkedin = import_parquet('1) data/14_WRDS_output/linkedin/french_affiliated_firm_roles_collapsed_clean.parquet') %>%
  select(-c(rcid, `__index_level_0__`)) 
export_data = import_csv('1) data/13a_firm_level_growth_analysis_inputs.csv', char_vars =  c('firmid'))


if (dummy_version){
  linkedin = merge(linkedin, data.table(firmid = linkedin[!duplicated(firmid) & has_siren][['firmid']]) %>%
                   mutate(firmid_new =  as.character(sample(seq_len(nrow(.)), size = nrow(.), replace = FALSE)),
                          replace = sample(c(0,1), size = nrow(.), replace = T)),
        all.x = T, by = 'firmid') %>%
  .[!is.na(firmid_new) & replace == 1, firmid := firmid_new] %>% select(-firmid_new, replace)
}

import_csv()


