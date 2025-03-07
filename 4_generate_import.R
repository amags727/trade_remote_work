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
prior_export_name = '01_07-31_export_package'
current_import_name = '02_23_25_import_package'
new_code = c('2) code/2_analysis/2_data_use_summary_stats.R')
data_not_to_update = c("1) data/16_bs_br_linkedin.parquet")


## generate holder for the new package to import into CASD / add current code base / code runner 
import_directory = paste0('4) exports-imports/imports_to_casd/', current_import_name)
unlink(import_directory, recursive = TRUE, force = TRUE)
dir.create(import_directory)
copy_directory('2) code', paste0(import_directory,"/1_code"))
dir.create(paste0(import_directory,'/3_updated_data'))
fwrite(data.frame(file_path = new_code), paste0(import_directory,'/5_code_to_run.csv'))
files_to_add = c('0_unpack_import_gen_export.R','1_import description.docx', '2_export description.docx')
file.copy(paste0('4) exports-imports/', files_to_add), file.path(import_directory, files_to_add))


## unload the previous export package 
prior_export_dir = paste0('4) exports-imports/exports_from_casd/', prior_export_name)
unlink(prior_export_dir, recursive = TRUE, force = TRUE)
dir.create(prior_export_dir)
unzip(paste0('4) exports-imports/exports_from_casd/', prior_export_name,'.zip'), exdir = prior_export_dir)

## check for data files not in prior export 
old_data_files = list.files(paste0(prior_export_dir, '/1a) dummy data'),recursive = TRUE, full.names = TRUE)
new_data_files = list.files('1) data',recursive = TRUE, full.names = TRUE)
files_to_update = c()
for (file_name in new_data_files){
  old_file_name = gsub("1) data", paste0(prior_export_dir, '/1a) dummy data'), file_name)
  
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

## zip the import package
setwd('4) exports-imports/imports_to_casd')
zip::zip(paste0(current_import_name,'.zip'), current_import_name)
setwd('../..')
unlink(import_directory, recursive = TRUE, force = TRUE)


## now check that the import package / all the files work 
setwd(prior_export_dir)
dir.create('4) exports-imports/exports_from_casd', recursive = T)
dir.create('4) exports-imports/imports_to_casd', recursive = T)
unzip(paste0('../../imports_to_casd/', current_import_name,'.zip'), exdir = '4) exports-imports/imports_to_casd')
copy_directory('1a) dummy data', '1) data')
setwd(paste0('4) exports-imports/imports_to_casd/', current_import_name,"/"))


job_id = rstudioapi::jobRunScript('0_unpack_import_gen_export.R')
Sys.sleep(5)
while (rstudioapi::jobGetState(job_id) == 'running') {
  Sys.sleep(1)  # Adjust sleep duration as needed
}

if (rstudioapi::jobGetState(job_id)== "succeeded"){
  print("FUCK YEAH, the transfer process will probably work")
  setwd('../../../../../../')
  unlink(prior_export_dir, recursive = TRUE, force = TRUE)
}else{
  print('time to troubleshoot')
}

