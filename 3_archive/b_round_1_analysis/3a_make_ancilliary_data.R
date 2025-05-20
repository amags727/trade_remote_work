# setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm',
             'arrow', 'fixest')
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})

setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')}
source("2) code/00_helper_functions.R")
inputs_dir = ('1) data/16_inputs_for_data_summary_stats')
dir.create(inputs_dir)
base_env = c(ls(),'base_env')



# make the admin birth data -----------------------------------------------

admin_birth_data = import_file('../../IWH/data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv', char_vars = 'siren',
  col_select = c("siren", "dateDebut", 'dateFin', "etatAdministratifUniteLegale")) %>%
  remove_if_NA('siren', 'dateDebut','etatAdministratifUniteLegale') %>% 
 .[, `:=`(start_year = year(dateDebut),end_year = year(dateFin), firmid = siren)] %>%
 .[etatAdministratifUniteLegale != 'C' & !is.na(start_year) & start_year <= 2024 & start_year > 1901] %>%
 .[,last_start := max(dateDebut) == dateDebut, by = firmid] %>% 
 .[, .(birth_year_admin = min(start_year), last_observed_admin = NA_max(end_year[last_start])), by = firmid] %>% 
 .[last_observed_admin == NA_max(last_observed_admin), last_observed_admin := NA]
write_parquet(admin_birth_data, '1) data/14_admin_birth_data.parquet')
# make the nace code database ----------------------------------------------------------------------
 nace_code <- import_file("https://gist.githubusercontent.com/b-rodrigues/4218d6daa8275acce80ebef6377953fe/raw/99bb5bc547670f38569c2990d2acada65bb744b3/nace_rev2.csv") %>%
   rename_with(~tolower(.)) %>% mutate(code = gsub("\\.", "",code)) %>%
   select(level, code,description)

 for (lev in 1:4){
   command = paste0(
     "nace_code = nace_code %>% mutate(nace_",lev,
     " = ifelse(level == ", lev,", code, NA), nace_descrip_",lev,
     " = ifelse(level == ", lev,", description, NA))",
     ifelse(lev != 1, paste0(" %>% group_by(nace_",lev-1,")"), ""),
     ifelse(lev != 4,gpaste(" %>% fill(nace_", c("", "descrip_"), lev, ", .direction = 'down')",
            collapse_str = ""), "")
     )
   eval(parse(text = command))
 }
   nace_code = nace_code %>%
   mutate(nace_descrip_short_1 = case_when(
     nace_1 == "A" ~ "AGRICULTURE",
     nace_1 == "B" ~ "MINING",
     nace_1 == "C" ~ "MANUFACTURING",
     nace_1 == "D" ~ "UTILITIES",
     nace_1 == "E" ~ "WATER MANAGEMENT",
     nace_1 == "F" ~ "CONSTRUCTION",
     nace_1 == "G" ~ "WHOLESALE /\n RETAIL TRADE",
     nace_1 == "H" ~ "TRANSPORTATION",
     nace_1 == "I" ~ "ACCOMMODATION /\n FOOD SERV.",
     nace_1 == "J" ~ "INFO /\nCOMMUNICATION",
     nace_1 == "K" ~ "FINANCE /\nINSURANCE",
     nace_1 == "L" ~ "REAL ESTATE",
     nace_1 == "M" ~ "PROFESSIONAL,\nSCIENCE + TECH",
     nace_1 == "N" ~ "ADMIN",
     nace_1 == "O" ~ "PUBLIC ADMIN + DEFENCE",
     nace_1 == "P" ~ "EDUCATION",
     nace_1 == "Q" ~ "HHS",
     nace_1 == "R" ~ "ARTS + ENTERTAINMENT",
     nace_1 == "S" ~ "OTHER SERVICES",
     nace_1 == "T" ~ "HOUSEHOLD ACTIVITY",
     nace_1 == "U" ~ "EXTRATERRITORIAL ORGS",
     TRUE ~ NA_character_),

     nace_descrip_short_2 = case_when(
     nace_2 == 58 ~ "Publishing",
     nace_2 == 59 ~ "Video + Music /nProduction",
     nace_2 == 60 ~ "Programming/\nBroadcast",
     nace_2 == 61 ~ "Telecom",
     nace_2 == 62 ~ "Computer programming",
     nace_2 == 63 ~ "Information Service",
     nace_2 == 69 ~ "Legal and Accounting",
     nace_2 == 70 ~ "Consultancy",
     nace_2 == 71 ~ "Architectural \n Engineering",
     nace_2 == 72 ~ "R&D",
     nace_2 == 73 ~ "Advertising\nmarket research",
     nace_2 == 74 ~ "Other",
     nace_2 == 75 ~ "Veterinary",
     TRUE ~ nace_descrip_2),

     nace_descrip_short_3 =  nace_descrip_3,
     nace_descrip_short_4 = nace_descrip_4)

fwrite(nace_code, file.path(inputs_dir, '16d_nace_code_breakdown.csv'))

