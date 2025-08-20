# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}; gc()

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

# generate cleaned version of admin birth/death ---------------------------
cleaning_admin = F
if(cleaning_admin){
admin_birth_data = import_file('../IWH/data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv', char_vars = 'siren',
                               col_select = c("siren", "dateDebut", 'dateFin', "etatAdministratifUniteLegale")) %>%
  remove_if_NA('siren', 'dateDebut','etatAdministratifUniteLegale') %>% 
  .[etatAdministratifUniteLegale != 'C' & !is.na(start_year) & start_year <= 2024 & start_year > 1901] %>% 
  .[, `:=`(start_year = year(dateDebut),end_year = year(dateFin),firmid = siren, firmid_num = as.numeric(as.factor(siren)))] 
  
admin_firmid_dict = unique(admin_birth_data[,.(firmid_num, firmid)])

last_observed_dta = admin_birth_data[order(firmid_num, start_year), .SD[.N], by = firmid_num] %>%
  rename(last_observed_admin = end_year) %>% .[,.(firmid_num, last_observed_admin)] %>% 
  .[last_observed_admin == NA_max(last_observed_admin), last_observed_admin := NA]

admin_birth_data = admin_birth_data[, .(birth_year_admin = min(start_year)), by = firmid_num] %>% 
  merge(last_observed_dta, by = 'firmid_num', all = T) %>% 
  merge(admin_firmid_dict, by = 'firmid_num') %>% .[,firmid_num := NULL]
write_parquet(admin_birth_data, '1) data/9_age_data/9a_admin_birth_data.parquet')
}
# generate complete birth data ------------------------------------------
#### prepare customs birth data
customs_birth_data = import_file(raw_customs_firm_lvl_path, col_select = c('year', 'firmid_num','exim')) %>% unique() %>% 
  .[,.(birth_year_customs = min(year), last_observed_customs = max(year),
      first_import_year = NA_min(year[exim ==1]), 
      first_export_year = NA_min(year[exim ==2])),
    by = firmid_num] %>% 
  .[last_observed_customs == max(last_observed_customs), last_observed_customs := NA ]

### prepare bs/br birth data
br_birth_data = import_file(raw_bs_br_path,col_select = c('firmid_num','year','for_turnover')) %>% 
  .[,.(birth_year_BS = min(year), last_observed_br = max(year), first_export_year_BS = NA_min(year[for_turnover >0])), by = firmid_num] %>%
  .[last_observed_br == max(last_observed_br), last_observed_br := NA ] %>% 
  .[,BS_observed_94 := birth_year_BS == 1994] %>% #94 is the first year of our data 
  .[birth_year_BS == 1994, birth_year_BS := NA] 

### merge together component datasets to generate birth data 
first_cust = c("first_import_year", "first_export_year")

birth_data = import_file('1) data/9_age_data/9a_admin_birth_data.parquet',nrows = ifelse(dummy_version, 10, Inf))
  birth_data = birth_data %>% 
  numeric_firmid(.) %>% unique() %>%
  merge(customs_birth_data, all = T, by = 'firmid_num') %>%
  merge(br_birth_data, all = T, by = 'firmid_num') %>% 
  
  ## generate birth year data with order of priority:
  ## 1) bs/br if less than customs 2) admin if less than customs 3) customs if everything else missing 
  .[,birth_year := case_when(
    birth_year_BS <= birth_year_customs ~ birth_year_BS,
    !is.na(birth_year_BS) & is.na(birth_year_customs) ~ birth_year_BS,
    birth_year_admin <= birth_year_customs ~ birth_year_admin,
    !is.na(birth_year_admin) & is.na(birth_year_customs) ~ birth_year_admin,
    birth_year_customs != 1993 & is.na(birth_year_admin) & is.na(birth_year_BS) &! BS_observed_94 ~ birth_year_customs,
    T ~ NA_real_)] %>% 
  .[,type := case_when(is.na(birth_year) ~ "missing",
                       birth_year == birth_year_BS ~ "BS",
                       birth_year == birth_year_admin ~ "admin",
                       birth_year == birth_year_customs ~ "customs",
                       T ~ "missing")] %>%
  
  ## correct the first export / import year: it only exists if we know when the firm was born, that year is <= first export year 
  ## and we've be able to observe it 
  .[, (first_cust) := lapply(.SD, function(x)
    case_when(is.na(birth_year) | (x==1993 & birth_year !=1993) | birth_year < 1993 ~NA,
              birth_year <= x ~x,
              !is.na(birth_year) & is.na(x) ~ 2099, # it could have exported / imported but didn't --> set to 2099 for ease
              T~ NA)),  .SDcols = first_cust] %>% 
  
  .[, first_export_year_BS := lapply(.SD, function(x) 
    case_when(is.na(birth_year) | (x==1994 & birth_year !=1994) | birth_year < 1994 ~NA,
              birth_year <= x ~x,
              !is.na(birth_year) & is.na(x) ~2099,
              T ~NA)), .SDcols = 'first_export_year_BS'] %>% 
  
  ## add consolidate death data
  .[,last_observed := case_when(firmid_num %in% br_birth_data$firmid_num ~ last_observed_br,
                            firmid_num %in% birth_data$firmid_num ~ last_observed_admin,
                            T ~ last_observed_customs)] %>% 
  
  ## finally rename variables 
  rename(first_export_year_customs = first_export_year)

## export   
write_parquet(birth_data,firm_lvl_birth_path)
rm(list= setdiff(ls(), base_env)); gc()



# generate "birth" vars needed for customs data  ------------------------------
customs_birth_vars = c('country_entrance_year', 'streak_start')
birth_data = import_file(firm_lvl_birth_path, col_select = c('firmid_num','first_import_year', 'first_export_year_customs', 'birth_year'))

customs_data = c('year', 'firmid_num','exim', 'ctry_num', 'streak_id') %>%
  import_file(raw_customs_firm_lvl_path,col_select = .) 

max_year = NA_max(customs_data$year)

customs_birth_data = customs_data[, `:=`(streak_start = min(year),streak_end = max(year)), by = 'streak_id'] %>%
  .[streak_end == max_year, streak_end := NA] %>%
  merge(birth_data, all.x = T) %>% 
  .[,country_entrance_year := min(year), by = .(firmid_num,exim, ctry_num)] %>% 
  .[, (customs_birth_vars) := lapply(.SD, function(x)
        case_when(is.na(birth_year) | (x==1993 & birth_year !=1993)~NA,birth_year <= x ~ x, T~ NA)),
        .SDcols = customs_birth_vars] %>% select(-year) %>% distinct(firmid_num,exim,ctry_num, streak_id, .keep_all = T)

write_parquet(customs_birth_data, firm_ctry_lvl_birth_path)
rm(list= setdiff(ls(), base_env)); gc()
# Generate streak information for firms  ----------------------------------
age_data = import_file(firm_lvl_birth_path, col_select = c('birth_year', 'firmid_num'))


streak_info_BS = import_file(raw_bs_br_path, col_select = c('firmid_num', 'year', 'for_turnover')) %>%
  
  ## 2008 for_turnover is almost always missing from data. If the firm
  ## has an export streak on either side of 2008 add a 2008 observation to keep it going 
  .[for_turnover!=0] %>% 
  bind_rows(within_group_filter(., 'any(year ==2007) & any(year == 2009)', 'firmid_num') %>% 
              .[,.(year = 2008, for_turnover = 1), by= firmid_num]) %>%
  .[,temp := NULL]

  ## carry on with variable construction 
  unbalanced_lag(. , 'firmid_num', 'year', 'for_turnover', 1) %>% 
  .[,export_streak_id_BS := cumsum(is.na(for_turnover_lag1))] %>%
  .[, streak_start_year := min(year), by = export_streak_id_BS] %>% 
  merge(age_data[,.(firmid_num, birth_year)], by = 'firmid_num') %>% 
  .[streak_start_year == 1994, streak_start_year := ifelse(birth_year == 1994, streak_start_year, NA)] %>% 
  .[,export_streak_age_BS := year - streak_start_year] %>%
  .[,.(firmid_num,year,  export_streak_id_BS, export_streak_age_BS)]

export_vars = c('firmid_num', 'ctry_num', 'year', 'exim', 'value')
streak_info_customs = import_file(raw_customs_firm_lvl_path, col_select = export_vars) %>% .[exim == 2] %>% 
  .[,.(value = NA_sum(value)), by = .(firmid_num, year)] %>% 
  unbalanced_lag(. , 'firmid_num', 'year', 'value', 1) %>% 
  .[,export_streak_id_customs := cumsum(is.na(value_lag1))] %>%
  .[, streak_start_year := min(year), by = export_streak_id_customs] %>% 
  merge(age_data[,.(firmid_num, birth_year)], by = 'firmid_num') %>% 
  .[streak_start_year == 1994, streak_start_year := ifelse(birth_year == 1994, streak_start_year, NA)] %>% 
  .[,export_streak_age_customs := year - streak_start_year] %>%
  .[,.(firmid_num,year, export_streak_id_customs, export_streak_age_customs)]

output = merge(streak_info_BS, streak_info_customs, all= T, by = c('firmid_num', 'year'))
write_parquet(output,firm_lvl_export_birth_path)

# clear ------------------------------
rm(list= setdiff(ls(), base_env)); gc()
