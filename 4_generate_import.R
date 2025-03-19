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
prior_state_name = '2025_02_25_after_export'
current_import_name = '03_19_25_import_package'
new_code = c('2) code/1_cleaning/3_make_data_use_data.R',
             '2) code/2_analysis/2_data_use_summary_stats.R')
data_not_to_update = c("1) data/3_bs_br_data.csv",
  paste0('1) data/16_inputs_for_data_summary_stats/',
       c("16a_bs_br_linkedin.parquet", "16b_complete_birth_data.parquet", "16c_customs_for_data_summ_stats.parquet",
         "16e_data_x_industry_stats.parquet", "16f_data_x_industry_stats_censored.parquet","16g_firm_yr_level_summ_stats_inputs.parquet"
         ,"16h_export_firm_ctry_level_summ_stats_inputs.parquet", "16h_firm_level_summ_stats_inputs.parquet"
         ,"16i_export_firm_ctry_level_summ_stats_inputs.parquet", "16i_firm_level_summ_stats_inputs.parquet")),
  ### ONES THAT I KNOW I HAVENT CHANGED THAT TAKE FOREVER TO CHECK
  '1) data/15_french_affiliated_firm_roles_collapsed_clean.parquet',
  '1) data/tm_patent/patent_record_level_final.parquet',
  '1) data/tm_patent/tm_record_level_final.parquet',
  'siren_level_patent_and_tm_final.parquet'
  )
data_not_to_remove = c()

import_directory = paste0('4) exports-imports/imports_to_casd/', current_import_name)
prior_state_dir = paste0('4) exports-imports/preserved_states/', prior_state_name)

# generate the import -----------------------------------------------------
## generate holder for the new package to import into CASD / add current code base / code runner 
unlink(import_directory, recursive = TRUE, force = TRUE)
dir.create(import_directory)
copy_directory('2) code', paste0(import_directory,"/1_code"))
dir.create(paste0(import_directory,'/3_updated_data'))
fwrite(data.frame(file_path = new_code), paste0(import_directory,'/5_code_to_run.csv'))
files_to_add = c('0_unpack_import_gen_export.R','1_import description.docx', '2_export description.docx')
file.copy(paste0('4) exports-imports/', files_to_add), file.path(import_directory, files_to_add))


## unload the previous export package 
unlink(prior_state_dir, recursive = TRUE, force = TRUE)
unzip(paste0('4) exports-imports/preserved_states/', prior_state_name,'.zip'), exdir = '4) exports-imports/preserved_states')
unlink('4) exports-imports/preserved_states/__MACOSX', recursive = TRUE, force = TRUE)


## check for files that need to be added / updated since last preserved state 
old_data_files = list.files(paste0(prior_state_dir, '/1) data'),recursive = TRUE, full.names = TRUE)
new_data_files = list.files('1) data',recursive = TRUE, full.names = TRUE)
files_to_update = c()
for (file_name in setdiff(new_data_files, data_not_to_update)){
  print(paste0(round(which(setdiff(new_data_files, data_not_to_update) == file_name)[1] / length(setdiff(new_data_files, data_not_to_update))*100,1),
               ": ", file_name))
  old_file_name = gsub("1) data", paste0(prior_state_dir, '/1) data'), file_name)
  
  ## mark if we don't have the file 
  if(!file.exists(old_file_name)){
    print(paste0('added: ', file_name))
    files_to_update = append(files_to_update, file_name)
  }else{
    if (!grepl("\\.(shx|shp|prj|dbf|cpg|txt)$", old_file_name, ignore.case = TRUE)){
    tryCatch({
      if(!identical(import_file(old_file_name), import_file(file_name))){
        print(paste0('updated: ', file_name))
        files_to_update = append(files_to_update, file_name)
        
      }
    },
    error = function(e) {
      print(paste0("present but failed to load: ",old_file_name))
    })
    }
  }
}
files_to_update = setdiff(files_to_update, data_not_to_update)
if (length(files_to_update) !=0){
  updated_names = str_replace(files_to_update, ".*(?=/)", paste0(import_directory, "/3_updated_data"))
  file.copy(files_to_update,updated_names, overwrite = TRUE)
  fwrite(data.frame(file_path = files_to_update), paste0(import_directory,'/4_files_to_update.csv'))
}

## check for files that are absent in current iteration so we can remove to avoid confusion 
files_to_remove = setdiff(gsub(paste0(prior_state_dir,"/"),"", old_data_files), new_data_files) %>% setdiff(., data_not_to_remove)
fwrite(data.frame(file_path = files_to_remove), paste0(import_directory,'/6_files_to_remove.csv'))

## zip the import package
setwd('4) exports-imports/imports_to_casd')
zip::zip(paste0(current_import_name,'.zip'), current_import_name)
setwd('../..')
unlink(import_directory, recursive = TRUE, force = TRUE)


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

