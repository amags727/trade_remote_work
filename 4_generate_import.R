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

# set parameters ----------------------------------------------------------------------
prior_state_name = '2025_04_22_after_import_to_CASD'
current_import_name = '2025_04_27_import_package'
new_code = c('2) code/1_cleaning/3_it_data_prep.R','2) code/2_analysis/1_run_all_analysis.R')
cutoff_date = as.Date('2025-04-28')

import_directory = paste0('4) exports-imports/imports_to_casd/', current_import_name)
prior_state_dir = paste0('4) exports-imports/preserved_states/', prior_state_name)
gen_import = T

# generate the import -----------------------------------------------------
## generate holder for the new package to import into CASD / add current code base / code runner 
if (gen_import){
unlink(import_directory, recursive = TRUE, force = TRUE)
dir.create(import_directory)
copy_directory('2) code', paste0(import_directory,"/1_code"))
dir.create(paste0(import_directory,'/3_updated_data'))
fwrite(data.frame(file_path = new_code), paste0(import_directory,'/5_code_to_run.csv'))
files_to_add = c('0_unpack_import_gen_export.R','1_import description.docx', '2_export description.docx')
file.copy(paste0('4) exports-imports/', files_to_add), file.path(import_directory, files_to_add))



## unload the previous preserved state 
unlink(prior_state_dir, recursive = TRUE, force = TRUE)
dir.create(prior_state_dir)
unzip(paste0('4) exports-imports/preserved_states/', prior_state_name,'.zip'), exdir = prior_state_dir)
unlink(paste0(prior_state_dir,'/__MACOSX'), recursive = TRUE, force = TRUE)


## check for files that need to be added / updated since last preserved state 
old_data_files = list.files(paste0(prior_state_dir, '/1) data'),recursive = TRUE, full.names = TRUE)
new_data_files = list.files('1) data',recursive = TRUE, full.names = TRUE)

files_to_update = data.frame(file_path = new_data_files) %>% 
  mutate(last_modified =   file.info(file_path)$mtime) %>% 
  filter(last_modified > as.Date(cutoff_date)) %>% arrange(last_modified) %>% pull(file_path) %>%
  con_fil(., '16_inputs_for_data', 'dummy', inc = F)

if (length(files_to_update) !=0){
  updated_names = str_replace(files_to_update, ".*(?=/)", paste0(import_directory, "/3_updated_data"))
  file.copy(files_to_update,updated_names, overwrite = TRUE)
  fwrite(data.frame(file_path = files_to_update), paste0(import_directory,'/4_files_to_update.csv'))
}

## check for files that are absent in current iteration so we can remove to avoid confusion 
files_to_remove = setdiff(gsub(paste0(prior_state_dir,"/"),"", old_data_files), new_data_files) 
if (length(files_to_remove > 0 )){
fwrite(data.frame(file_path = files_to_remove), paste0(import_directory,'/6_files_to_remove.csv'))
}

## zip the import package
setwd('4) exports-imports/imports_to_casd')
zip::zip(paste0(current_import_name,'.zip'), current_import_name)
setwd('../..')
unlink(import_directory, recursive = TRUE, force = TRUE)
}

# check that the import will work  ----------------------------------------
## now check that the import package / all the files work 
setwd(prior_state_dir)
dir.create('4) exports-imports/exports_from_casd', recursive = T)
dir.create('4) exports-imports/imports_to_casd', recursive = T)
unzip(paste0('../../imports_to_casd/', current_import_name,'.zip'), exdir = '4) exports-imports/imports_to_casd')
copy_directory('1) data','1a) dummy data')
setwd(paste0('4) exports-imports/imports_to_casd/', current_import_name,"/"))


job_id = rstudioapi::jobRunScript('0_unpack_import_gen_export.R')
Sys.sleep(5)
while (rstudioapi::jobGetState(job_id) == 'running') Sys.sleep(1)  # Adjust sleep duration as needed

if (rstudioapi::jobGetState(job_id)== "succeeded"){
  print("FUCK YEAH, the transfer process will probably work")
  setwd('../../../../../../')
  unlink(prior_state_dir, recursive = TRUE, force = TRUE)
}else{
  print('time to troubleshoot')
}

