# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))};gc()

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

source('2) code/0_set_parameter_values.R')
# Bring in patent and product data --------------------------------------------------------

firm_pat_prod <- import_file("~/Reallocation/6 Publish/2 Data/product_firm_data_pre_high_growth.RDS") %>%
  select(c("firmid",                      "year",                        "number_of_products",          "new_products",                "destroyed_products",         
           "paused_products",             "reintroduced_products",       "exit_post_entry",             "exit_pre_entry",              "entry_post_exit",            
           "entry_pre_exit",              "first_introduction",
           "entry_year",                  "exit_year",                   "exit_with_entry",             "entry_with_exit", 
           "num_patent",                  "num_tm",                     
           "total_patent",                "total_tm",                    "total_patent_l",              "total_patent_bar",            "total_patent_growth",        
           "total_tm_l",                  "total_tm_bar",                "total_tm_growth",             "patent",                      "patent_window_temp",         
           "tm",                          "tm_window_temp",              "patent_window",               "tm_window",                   "log_n_products",             
           "net_product_change",          "net_product_creat",           "net_product_destr",           "net_product_creat_window",    "net_product_destr_window",   
           "nuts3",                       
           "application_year",            "ipcr_cum",                    "NACE_cum",                   
           "n_ipcr",                      "n_NACE",                      "n_NACE_bar",                  "n_ipcr_bar",                  "n_NACE_growth",              
           "n_ipcr_growth",               "ipcr_cum_l",                  "NACE_cum_l",                  "new_ipcr",                    "new_NACE",                   
           "ipcr_creat",                  "ipcr_creat_window",           
           "rev",                         "rev_l",                       "rev_bar",                     "rev_growth",                 
           "ever_patent",                 "ever_tm"   ))

# Transform firmid into firmid num
bs_br_firms <- import_file('1) data/0_misc_data/0b_dictionaries/0b1_matched_firm_dict.parquet')
firm_pat_prod<-merge(firm_pat_prod, bs_br_firms, by="firmid", all.x = T) %>% 
  select(firmid_num, year, everything())%>% select(-firmid)

# Save file
write_csv(firm_pat_prod, "1) data/13_firm_pat_prod.csv")

