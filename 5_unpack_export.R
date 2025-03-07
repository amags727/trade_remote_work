# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest', 'zip')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../')
source("2) code/00_helper_functions.R")

# setup -------------------------------------------------------------------
date = "02_25_25"
export_dir = paste0('4) exports-imports/exports_from_casd/', date,'_export_package')
pre_preserve_dir = paste0("4) exports-imports/preserved_states/ante_",date, '_export')
post_preserve_dir = gsub("ante_", 'dopo_',pre_preserve_dir) 
lapply(c(pre_preserve_dir, post_preserve_dir), dir.create)
folders_to_copy = c("1) data", "2) code", "3) output")

## preserve existing state of project 
lapply(folders_to_copy, function(folder){copy_directory(folder, file.path(pre_preserve_dir, folder))}) 
setwd('4) exports-imports/preserved_states')
zip::zip( paste0("ante_",date, '.zip'), paste0("ante_",date, '_export'))
unlink(paste0("ante_",date, '_export'), recursive = TRUE, force = TRUE)
setwd('../..')

## import the new data /output
unzip(paste0(export_dir,'.zip'), exdir = export_dir)
data.frame(file = "16_bs_br_linkedin.parquet", location =  "1) data/16_bs_br_linkedin.parquet") %>% write_rds(
file.path(export_dir, '1) new dummy data/file_place_instructions.rds'))

file_locations = readRDS(file.path(export_dir, '1) new dummy data/file_place_instructions.rds'))
for (i in 1:nrow(file_locations)){
  file.copy(file.path(export_dir,'1) new dummy data', file_locations$file[i]),file_locations$location[i],overwrite = T)
}
unlink('3) output', recursive =  T, force = T)

unlink(export_dir, recursive = T, force = T)
# preserve the project post import 
copy_directory(file.path(export_dir,'3) output'), '3) output')
lapply(folders_to_copy, function(folder){copy_directory(folder, file.path(post_preserve_dir, folder))}) 
setwd('4) exports-imports/preserved_states')
zip::zip( paste0("dopo_",date, '_export.zip'), paste0("dopo_",date, '_export'))
unlink(paste0("dopo_",date, '_export'), recursive = TRUE, force = TRUE)
setwd('../..')



