
# generate cleaned version of admin birth/death ---------------------------
if(cleaning_admin){
admin_birth_data = import_file('../../IWH/data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv', char_vars = 'siren',
                               col_select = c("siren", "dateDebut", 'dateFin', "etatAdministratifUniteLegale")) %>%
  remove_if_NA('siren', 'dateDebut','etatAdministratifUniteLegale') %>% 
  .[, `:=`(start_year = year(dateDebut),end_year = year(dateFin), firmid = siren)] %>%
  .[etatAdministratifUniteLegale != 'C' & !is.na(start_year) & start_year <= 2024 & start_year > 1901] %>%
  .[,last_start := max(dateDebut) == dateDebut, by = firmid] %>% 
  .[, .(birth_year_admin = min(start_year), last_observed_admin = NA_max(end_year[last_start])), by = firmid] %>% 
  .[last_observed_admin == NA_max(last_observed_admin), last_observed_admin := NA]
write_parquet(admin_birth_data, '1) data/14_admin_birth_data.parquet')
}
# generate complete birth data ------------------------------------------
#### prepare customs birth data
customs_birth_data = import_file('1) data/9_customs_cleaned.csv', char_vars =  c('firmid'), col_select = c('year', 'firmid','exim','ctry')) %>%
  .[ctry != 'FR'] %>% select(-ctry) %>% unique() %>% 
  .[,.(birth_year_customs = min(year), last_observed_customs = max(year),
      first_import_year = NA_min(year[exim ==1]), 
      first_export_year = NA_min(year[exim ==2])),
    by = firmid] %>% 
  .[last_observed_customs == max(last_observed_customs), last_observed_customs := NA ]

### prepare bs/br birth data
br_birth_data = import_file(
  '1) data/3_bs_br_data.csv',
  char_vars =  c('firmid'),
  col_select = c('firmid','year')) %>% 
  na.omit() %>% unique() %>% 
  .[,.(birth_year_BS = min(year), last_observed_br = max(year)), by = firmid] %>%
  .[last_observed_br == max(last_observed_br), last_observed_br := NA ] %>% 
  .[,BS_observed_94 := birth_year_BS == 1994] %>% #94 is the first year of our data 
  .[birth_year_BS == 1994, birth_year_BS := NA] 

### merge together component datasets to generate birth data 
first_cust = c("first_import_year", "first_export_year")

birth_data = import_file('1) data/14_admin_birth_data.parquet' ,nrows = ifelse(dummy_version, 10, Inf))
birth_data = birth_data %>% unique() %>% merge(customs_birth_data, all = T) %>% merge(br_birth_data, all = T) %>% 
  
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
  
  ## correct the first export / import year: it only exists if we know when the firm was born 
  ## and that year is <= first export year 
  .[, (first_cust) := lapply(.SD, function(x)
    case_when(is.na(birth_year) | (x==1993 & birth_year !=1993)~NA,
              birth_year <= x ~x,
              !is.na(birth_year) & is.na(x) ~ 2099, # it could have exported / imported but didn't --> set to 2099 for ease
              T~ NA)),  .SDcols = first_cust] %>% 
  
  ## add consolidate death data
  .[,last_observed := case_when(firmid %in% br_death$firmid ~ last_observed_br,
                            firmid %in% birth_data$firmid ~ last_observed_admin,
                            T ~ last_observed_customs)]

## export   
write_parquet(birth_data,file.path(inputs_dir, '16a_firm_lvl_birth_data.parquet'))

# generate "birth" vars needed for customs data  ------------------------------
customs_birth_vars = c('country_entrance_year', 'streak_start')
birth_data = import_file(file.path(inputs_dir, '16a_firm_lvl_birth_data.parquet'), 
                         col_select = c('firmid','first_import_year', 'first_export_year', 'birth_year'))

customs_data = c('year', 'firmid','exim', 'ctry', 'streak_id', 'streak_start') %>%
  import_file('1) data/9_customs_cleaned.csv', char_vars =  c('firmid'),col_select = .) %>%
  .[ctry != 'FR']
max_year = NA_max(customs_data$year)

customs_birth_data = customs_data[,streak_end := NA_max(year), by = 'streak_id'] %>%
  .[streak_end == max_year, streak_end := NA] %>%
  merge(birth_data, all.x = T) %>% 
  .[,country_entrance_year := min(year), by = .(firmid,exim, ctry)] %>% 
  .[, (customs_birth_vars) := lapply(.SD, function(x)
        case_when(is.na(birth_year) | (x==1993 & birth_year !=1993)~NA,birth_year <= x ~ x, T~ NA)),
        .SDcols = customs_birth_vars] %>% select(-year) %>% distinct(firmid,exim,ctry, streak_id, .keep_all = T)

write_parquet(customs_birth_data,file.path(inputs_dir, '16b_firm_ctry_lvl_birth_data.parquet'))
# clear ------------------------------
rm(list= setdiff(ls(), base_env)); gc()