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


#import data / set parameters  -----------------------------------------------------------------------
firm_yr = import_file(firm_yr_path) %>% merge(import_file(firm_pat_prod_path), by=c("firmid_num", "year"), all.x=T)


dep_vars<-c("log_n_products", 'net_product_creat', "net_product_destr", # log number of products, dummy for the introduction of products net of product destructions
            "patent", "total_patent", "log(total_patent)", #dummy for patenting, total number of patents, log total number of patents
            "tm", "total_tm", "log(total_tm)", #dummy for trademarking, total number of trademarks, log total number of trademarks
            "ipcr_creat", "n_ipcr", "log(n_ipcr)") # dummy for entering a new ipcr, number of ipcr patenting fields, log number of ipcr patenting fields

base_controls =  paste("", 'log_comp_total', 'log_comp_total_lag1', 'log_dom_turnover', 'log_dom_turnover_sq',
                       'avg_prestige_total', 'share_empl_college', "capital_intensity", 'log_age', sep = " + ")

base_command = reg_command(dataset = 'firm_yr', dep_var =  dep_vars, ind_var = 'log_comp_data + log_comp_data_lag1', 
                           controls = base_controls, fe = "| firmid_num + year", cluster = 'firmid_num')

alt_1_base_command = gsub('log_comp_data \\+ log_comp_data_lag1',
                          'log_comp_data + log_comp_non_data_rnd + log_comp_data_lag1 + log_comp_non_data_rnd_lag1',
                          base_command)

alt_2_base_command = gsub('log_comp_data \\+ log_comp_data_lag1',
                          'log_comp_data*log_comp_non_data_rnd + log_comp_data_lag1*log_comp_non_data_rnd_lag1',
                          base_command)

alt_3_base_command = gsub('log_comp_data \\+ log_comp_data_lag1',
                          'log_comp_non_data_rnd + log_comp_non_data_rnd_lag1',
                          base_command)

alt_4_base_command = gsub('log_comp_data \\+ log_comp_data_lag1',
                          'log_comp_data + log_comp_stem + log_comp_data_lag1 + log_comp_stem_lag1',
                          base_command)

alt_5_base_command = gsub('log_comp_data \\+ log_comp_data_lag1',
                          'log_comp_data*log_comp_stem + log_comp_data_lag1*log_comp_stem_lag1',
                          base_command)




# 2a generate variations ----------------------------------------------------------------------
baseline_rev = data.table(dep_var = dep_vars, command = c(
  ## add BS versions 
  base_command,
  alt_1_base_command, 
  alt_2_base_command,
  alt_3_base_command,
  alt_4_base_command,
  alt_5_base_command 
  )) 



## setup baseline 
variations = rbindlist(list(baseline_rev), use.names = T, fill = T) %>% 
  as.data.table() %>% 
  
  ## add version with industry fe
  bind_rows(mutate(.,command = gsub("\\| firmid_num", ' | NACE_BR', command))) 

if (!dummy_version){  
  model_output = evaluate_variations(variations)
  if(nrow(model_output$failed_output)!= 0) print('CHECK WHAT WENT WRONG')
  write_rds(model_output, paste0(raw_output_dir, '6_firm_pat_prod_variations.rds'))
}
