# generate complete birth data ------------------------------------------
#### prepare customs birth data
customs_birth_data = import_file('1) data/9_customs_cleaned.csv', char_vars =  c('firmid'), col_select = c('year', 'firmid','exim','ctry')) %>%
  .[ctry != 'FR'] %>% select(-ctry) %>% 
  unique() %>% .[,min_exim_year := min(year), by = .(firmid,exim)] %>% 
  pivot_wider(id_cols = c(year,firmid), names_from = exim, values_from = min_exim_year, names_prefix = 'exim') %>% as.data.table() %>% 
  .[, .(birth_year_customs = min(year), first_import_year = as.integer(NA_min(exim1)), first_export_year = as.integer(NA_min(exim2))), by = firmid] 

### prepare bs/br birth data
br_birth_data = import_file(
  '1) data/3_bs_br_data.csv',
  char_vars =  c('firmid'),
  col_select = c('firmid','year')) %>% 
  na.omit() %>% unique() %>% 
  .[,.(birth_year_BS = min(year)), by = firmid] %>%
  .[birth_year_BS == 1994, birth_year_BS := NA] #94 is the first year of our data 

### merge together component datasets to generate birth data 
first_cust = c("first_import_year", "first_export_year")
birth_data = import_file('1) data/14_admin_birth_data.parquet')  %>% unique() %>% 
  merge(customs_birth_data, all = T) %>%merge(br_birth_data, all = T) %>% 
  
  ## generate birth year data with order of priority:
  ## 1) bs/br if less than customs 2) admin if less than customs 3) customs if everything else missing 
  .[,birth_year := case_when(
    birth_year_BS <= birth_year_customs ~ birth_year_BS,
    !is.na(birth_year_BS) & is.na(birth_year_customs) ~ birth_year_BS,
    birth_year_admin <= birth_year_customs ~ birth_year_admin,
    !is.na(birth_year_admin) & is.na(birth_year_customs) ~ birth_year_admin,
    birth_year_customs != 1993 & is.na(birth_year_admin) & is.na(birth_year_BS) ~ birth_year_customs,
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
              T~ NA)),  .SDcols = first_cust]


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