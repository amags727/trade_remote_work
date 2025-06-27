# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}; gc();

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

# set parameters ----------------------------------------------------------------------
current_import_name = '2025_06_27_import_package'
import_dir = paste0('4) exports-imports/imports_to_casd/', current_import_name)
cutoff_date = as.Date('2025-06-22')
# generate all components of import -----------------------------------------------------


code_to_run = data.frame(file_path = list.files('2) code',recursive = TRUE, full.names = TRUE)) %>% 
  mutate(last_modified =   file.info(file_path)$mtime) %>% 
  filter(last_modified > as.Date(cutoff_date)) %>% 
  filter(!grepl("round_[1-3]", file_path)) %>%.[['file_path']] %>% 
  con_fil(., '/0_', '00_', 'dummy', '7_revelio/','4_generate_import','6_make_', inc = F) %>% 
  as.data.frame() %>% rename_with(~c('file_path'))

non_output_files_to_export = data.frame(
  file_path = c('1) data/0_misc_data/0c_similarity_matrices',
                '1) data/0_misc_data/0b_dictionaries/0b2_ctry_dict.parquet',
                '1) data/0_misc_data/0a_nac_2d_industry_categories.csv'),
  directory = c(T, F,F))


data_to_import = data.frame(file_path = list.files('1) data',recursive = TRUE, full.names = TRUE)) %>% 
  mutate(last_modified =   file.info(file_path)$mtime) %>% 
  filter(last_modified > as.Date(cutoff_date)) %>% select(-last_modified) %>% 
  rename(actual_file_path = file_path) %>% 
  mutate(import_file_path = gsub('1) data', paste0(import_dir, '/2_updated_data'), actual_file_path),
         import_file_dir =   sub("/[^/]*$", "", import_file_path),
         actual_file_dir = gsub(paste0(import_dir, '/2_updated_data'),'1) data',import_file_dir))



# gen import --------------------------------------------------------------
unlink(import_dir, recursive = TRUE, force = TRUE)
dir.create(import_dir)
copy_directory('2) code', paste0(import_dir,"/1_code"))
dir.create(paste0(import_dir,'/2_updated_data'))
for(i in 1:nrow(data_to_import)){
  dir.create(data_to_import$import_file_dir[i], recursive = T)
  file.copy(data_to_import$actual_file_path[i],
            data_to_import$import_file_path[i], 
            overwrite = T)
}
write_parquet(data_to_import,paste0(import_dir,'/3_data_to_import.parquet'))
write_parquet(code_to_run,paste0(import_dir,'/4_code_to_run.parquet'))
write_parquet(non_output_files_to_export,paste0(import_dir,'/5_non_output_files_to_export.parquet'))
files_to_add = c('0_unpack_import_gen_export.R','6_import_description.docx', '7_export_description.docx')
file.copy(paste0('4) exports-imports/', files_to_add), file.path(import_dir, files_to_add), overwrite = T)




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




