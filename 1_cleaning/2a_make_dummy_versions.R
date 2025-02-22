
#DESCRIPTION 
# For each dataset I use in my analysis; I generate a simulated version that fits the criteria for export (non identifiable, no distributional info for groups of less than four)
# To accomplish this I proceeed in several steps 
# 1) I identify groups of observations that are larger than the cutoff threshold for cell cize (IE greater than 4 firms)
# 2) I randomly assign observations in the simulated data to a group identified in step 1
# 3) For the remainder of variables that not used to identify groups, I generate group level summary statistics (eg mean and SD)
#    and then use those summary statistics to generate simulated values 
# 
# Firm ids are randomly generated (not using any of the original siren ids) just to make the variables in the simulated data
# align with those in the original data. 
# 
# For datasets that were already aggregated (datasets 10a-12 which present industry summary stats)
# I drop cells that are made up less than five firm_id_threshold


# Setup -------------------------------------------------------------------
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm'
             )
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../00_helper_functions.R")
raw_dir = 'C:/Users/Public/1. Microprod/0. Raw data processing/Data/'



# 00) parameters / helper functions---------------------------------------------------------
firm_id_threshold = 4; set.seed(1)

simulate_discrete_vars = function(data, data_dummy, group_vars, interest_vars){
  
  # use the group vars to generate unique ids fro each group
  group_keys = data[, ..group_vars] %>% unique() %>% mutate(group_code = 1:nrow(.))
  data = merge(data, group_keys)
  data_dummy = merge(data_dummy, group_keys)
  
  # for each group use the joint empirical distribution of values to generate 
  data_dummy = lapply(1:nrow(group_keys), function(i){
    temp_dummy = data_dummy[group_code == i]; num_in_dummy = nrow(temp_dummy)
    if (num_in_dummy > 0){
      temp = data[group_code == i, ..interest_vars]; num_in_temp = nrow(temp);
      temp_dummy = cbind(temp_dummy,temp[sample(1:num_in_temp, num_in_dummy, T)])
    }
    return(temp_dummy)
  }) %>% rbindlist(fill =T, use.names = T)
  data[, group_code := NULL]; data_dummy[, group_code := NULL]
  return(data_dummy)
}

simulate_continuous_vars = function(data, data_dummy, group_vars, interest_vars){
  
  # use the group vars to generate unique ids fro each group
  group_keys = data[, ..group_vars] %>% unique() %>% mutate(group_code = 1:nrow(.))
  data = merge(data, group_keys)
  data_dummy = merge(data_dummy, group_keys, by = group_vars)
  
  # generate the mins and maxes for the whole dataset, these will serve as bounds
  # for the simulation draws
  mins = apply(data[,..interest_vars],2, NA_min); maxes = apply(data[,..interest_vars],2, NA_max)
  
  # for each group generate the multivariate normal distribution of the variables of interest 
  data_dummy = lapply(1:nrow(group_keys), function(i){
    temp_dummy = data_dummy[group_code == i]; num_in_dummy = nrow(temp_dummy)
    if (num_in_dummy > 0){
      temp_dummy = tryCatch({
        temp = data[group_code == i, ..interest_vars] 
        ## ensure covariance matrix is positive definite
        noise = rnorm(nrow(temp)*length(interest_vars),0, 1e-6) %>% matrix(., nrow = nrow(temp))
        noise = noise - min(noise);temp = temp+ noise
        cov_matrix = cov(temp, use = 'pairwise.complete.obs') %>% nearPD()
        cov_matrix = cov_matrix$mat
        
        # simulate data 
        draws = rtmvnorm(num_in_dummy, colMeans(temp, na.rm = T),cov_matrix,mins,maxes)
        temp_dummy[,(interest_vars) := as.data.table(draws)] 
        return(temp_dummy)
      }, error = function(e){return(temp_dummy)})
    }
    return(temp_dummy)
  }) %>% rbindlist(fill =T, use.names = T)
  
return(data_dummy)
}



# 0)  -------------------------------------------------------------------------
filepath = '../1) data/0_world_bank_france_gdp_deflator_2015_base.csv'
fread(filepath) %>% fwrite(gsub('1) data','1a) dummy data',filepath))

# 3) BS BR data -----------------------------------------------------------
data = fread( '../1) data/3_bs_br_data.csv',nrows = 100000, colClasses = list(character= 'firmid')) %>% na.omit()
data[, count:= .N, by = .(NACE_BR, year)]; data = data[count > firm_id_threshold]; data[, count :=NULL ]
num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data)
year_nace = data[, .(NACE_BR, year)]

# for each obs. randomly assign an id and then draw from the joint distrib of year-NACE industry combinations
data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T)) %>%
  cbind(.,year_nace[sample(1:num_data_points, num_data_points, T)]) %>% unique()

# for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)
interest_vars = setdiff( names(data),c('empl_bucket',names(data_dummy)))
group_vars = c('NACE_BR', 'year')
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)


# add any vars still missing
data_dummy[, empl_bucket := ifelse(empl < 10, "0-10", ifelse(empl < 50, '10-50',
                                                             ifelse(empl < 200, '50-200', ifelse(empl >=200, '200+', NA))))]

setdiff(names(data),names(data_dummy))
fwrite(data_dummy, '../1a) dummy data/3_bs_br_data.csv')
rm(data, data_dummy)
# 4) OFATS -----------------------------------------------------------
data = fread( '../1) data/4_OFATS.csv', colClasses = list(character= 'firmid')) %>% na.omit()
group_vars = c('ctryofats', 'year')

data[, count:= .N, by = group_vars]; data = data[count > firm_id_threshold]; data[, count :=NULL ]
num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data);
group_distrib = data[, ..group_vars]

# for each obs. randomly assign an id and then draw from the joint distrib of year-NACE industry combinations
data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T)) %>%
  cbind(.,group_distrib[sample(1:num_data_points, num_data_points, T)]) %>% unique()

# for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)
interest_vars = setdiff( names(data),c('name',names(data_dummy)))
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)
setdiff(names(data),names(data_dummy))

fwrite(data_dummy, '../1a) dummy data/4_OFATS.csv')



# 9) Firm Level Customs Data  ------------------------------------------------
data = fread('../1) data/9_customs_cleaned.csv'); sample_size = 1000000
data = data[sample(1:nrow(data), sample_size, F)]

group_vars = c('exim', 'ctry', 'year', 'birth_year','firm_age', 'birth_observed', 
               'streak_start', 'streak_length', 'streak_birth_observed', 'year_in_streak', 
               'streak_death_observed')

data[, count:= .N, by = group_vars]; data = data[count > firm_id_threshold]; data[, count :=NULL ]
group_distrib = data[, ..group_vars]
num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data);
num_streaks = data[!duplicated(streak_id)] %>% nrow();


data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T),
             streak_id = sample(1:num_streaks, num_data_points, T)) %>% 
             cbind(.,group_distrib[sample(1:num_data_points, num_data_points, T)]) %>% unique()


# for remaining discrete vars simulate data using observed empirical distrib (all groups contain >= 4 members)
base = c('_region', '_language', '_border') 
varlist = list(paste0('export',base),  paste0('import',base),
               paste0('import',base,"_lag1"), paste0('export',base,"_lag1"),
               c('import_hs_class', 'export_hs_class'), 'first_import_year', 'first_export_year')

for(interest_vars in varlist){
data_dummy = simulate_discrete_vars(data, data_dummy,group_vars, interest_vars) 
}
interest_vars = setdiff( names(data),c('import_portfolio_key_lag',names(data_dummy)))
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)
setdiff(names(data),names(data_dummy))

fwrite(data_dummy,'../1a) dummy data/9_customs_cleaned.csv')
# 10 Product Level Customs data  ------------------------------------------------
data = fread('../1) data/10_customs_product_level_cleaned.csv'); sample_size = 1000000
data = data[sample(1:nrow(data), sample_size, F)]

group_vars = c("ctry","exim","year", "streak_birth_observed" ,"streak_death_observed" ,
               'streak_start',"year_in_streak", "streak_length", 'birth_observed') 
data[, count:= .N, by = group_vars]; data = data[count > firm_id_threshold]; data[, count :=NULL]
group_distrib = data[, ..group_vars]
num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data);
num_streaks = data[!duplicated(streak_id)] %>% nrow();


data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T),
                        streak_id = sample(1:num_streaks, num_data_points, T)) %>% 
  cbind(.,group_distrib[sample(1:num_data_points, num_data_points, T)]) %>% unique()


# for remaining vars simulate from observed distribution (all groups contain >= 4 members)
data_dummy = simulate_discrete_vars(data, data_dummy,group_vars, 'CN8plus') 
data_dummy[,hs_class := substr(CN8plus,1,2)] 

interest_vars = setdiff( names(data),names(data_dummy))
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)

setdiff( names(data),names(data_dummy))
fwrite(data_dummy,'../1a) dummy data/10_customs_product_level_cleaned.csv')
# 10a-12 summary stats ------------------------------------------------
## These are all industry level summary stats so we just need to make sure the number of firms in each cell is greater than or equal to 5
#10a product level summary stats 
filepath = '../1) data/10a_product_lvl_summary_stats.csv'
fread(filepath)[cn8_ctry_num_firms > firm_id_threshold] %>% fwrite(gsub('1) data','1a) dummy data',filepath))

#11 country level data
filepath = '../1) data/11_country_level_data.csv'
fread(filepath)[hs_ctry_num_firms > firm_id_threshold] %>% fwrite(gsub('1) data','1a) dummy data',filepath))


#12
filepath = '../1) data/12_domestic_industry_summary_stats.csv'
fread(filepath)[nace_num_firms > firm_id_threshold] %>% fwrite(gsub('1) data','1a) dummy data',filepath))


# 13a firm level growth analysis  -----------------------------------------
data = fread('../1) data/13a_firm_level_growth_analysis_inputs.csv'); sample_size = 1000000
data = data[sample(1:nrow(data), sample_size, F)]

group_vars = c('ctry', 'year', 'birth_year','firm_age', 'birth_observed', 
               'streak_start', 'streak_length', 'streak_birth_observed', 'year_in_streak', 
               'streak_death_observed')

data[, count:= .N, by = group_vars]; data = data[count > firm_id_threshold]; data[, count :=NULL ]
group_distrib = data[, ..group_vars]
num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data);
num_streaks = data[!duplicated(streak_id)] %>% nrow();
data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T),
                        streak_id = sample(1:num_streaks, num_data_points, T)) %>% 
  cbind(.,group_distrib[sample(1:num_data_points, num_data_points, T)]) %>% unique()


# for remaining discrete vars simulate data using observed empirical distrib (all groups contain >= 4 members)
base = c('_region', '_language', '_border') 
varlist = list(paste0('export',base),  paste0('import',base),
               paste0('import',base,"_lag1"), paste0('export',base,"_lag1"),
               c('import_hs_class', 'export_hs_class','NACE_BR'), 'first_import_year', 'first_export_year', 'streak_always_observed')
for(interest_vars in varlist){
  data_dummy = simulate_discrete_vars(data, data_dummy,group_vars, interest_vars) 
}

# now add the industry stats; use the dummy versions so we don't have to worry about sufficient size 
domestic_industry_data = fread('../1a) dummy data/12_domestic_industry_summary_stats.csv')
domestic_industry_data_lag = domestic_industry_data %>% mutate(year = year +1) %>% rename_with(~paste0(.,"_lag"), -c(NACE_BR,year)) %>%
  select(NACE_BR, year, nace_deflated_rev_quartile_annual_lag)

country_data = fread('../1a) dummy data/11_country_level_data.csv')
country_data_lag = country_data %>% mutate(year = year + 1) %>% rename_with(~paste0(.,"_lag"), -c(ctry, export_hs_class,year)) %>%
  select(ctry, export_hs_class, year, contains(c('deflated_value_quartile', 'gdp', 'num_firms', 'streak_age','population')))

data_dummy = merge(data_dummy, domestic_industry_data, by = c('NACE_BR', 'year'), all.x = T) %>% 
  merge(., domestic_industry_data_lag, by = c('NACE_BR', 'year'), all.x = T) %>% 
  merge(., country_data, by = c('ctry', 'export_hs_class', 'year'), all.x = T) %>%
  merge(., country_data_lag, by = c('ctry', 'export_hs_class', 'year'), all.x = T) 
  

## now simulate the firm level continuous variables 
interest_vars = setdiff( names(data),c('empl_bucket','export_ratio',names(data_dummy)))
part_1 = simulate_continuous_vars(data, data_dummy[!is.na(NACE_BR)], group_vars, interest_vars)
part_2 = simulate_continuous_vars(data, data_dummy[is.na(NACE_BR)], group_vars, c('products', 'deflated_value'))
data_dummy = rbind(part_1, part_2, fill = T)

# add last two variables 
data_dummy[, empl_bucket := ifelse(empl < 10, "0-10", ifelse(empl < 50, '10-50', ifelse(empl < 200, '50-200', ifelse(empl >=200, '200+', NA))))]
data_dummy[,export_ratio := deflated_value/deflated_dom_turnover][deflated_dom_turnover ==0, export_ratio := NA]
setdiff( names(data),names(data_dummy))
fwrite(data_dummy,'../1a) dummy data/13a_firm_level_growth_analysis_inputs.csv')

# 13b product level growth analysis  --------------------------------------
data = fread( '../1) data/13b_product_level_growth_analysis_inputs.csv'); sample_size = 1000000
data = data[sample(1:nrow(data), sample_size, F)]

group_vars = c("ctry","year", "streak_birth_observed" ,"streak_death_observed" ,
               'streak_start',"year_in_streak", "streak_length", 'birth_observed') 

data[, count:= .N, by = group_vars]; data = data[count > firm_id_threshold]; data[, count :=NULL]
group_distrib = data[, ..group_vars]
num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data);
num_streaks = data[!duplicated(streak_id)] %>% nrow();


data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T),
                        streak_id = sample(1:num_streaks, num_data_points, T)) %>% 
  cbind(.,group_distrib[sample(1:num_data_points, num_data_points, T)]) %>% unique()

## simulate the product category 
data_dummy = simulate_discrete_vars(data, data_dummy,group_vars, 'CN8plus') 
data_dummy[,hs_class := substr(CN8plus,1,2)] 


## add in pre-censored product category level data 
product_collapsed = fread( '../1a) dummy data/10a_product_lvl_summary_stats.csv')[exim ==2 & cn8_ctry_num_firms > 0][,exim:= NULL]
product_collapsed_lag = product_collapsed %>% mutate(year = year +1) %>% rename_with(~paste0(.,"_lag"), -c(ctry,CN8plus,year)) %>%
  select(ctry, CN8plus, year, contains('deflated_value_quartile'))

country_level_data = fread('../1a) dummy data/11_country_level_data.csv') %>% select(-contains('hs')) %>% unique()
country_level_data_lag = country_level_data %>% mutate(year = year + 1) %>% rename_with(~paste0(.,"_lag"), -c(ctry, year)) %>%
  select(ctry, year, contains('deflated_value_quartile'))

data_dummy = merge(data_dummy, country_level_data, all.x = T, by = c( 'ctry', 'year')) 
data_dummy = merge(data_dummy, country_level_data_lag, all.x = T, by = c( 'ctry', 'year')) 
data_dummy = merge(data_dummy, product_collapsed, all.x = T, by = c( 'ctry', 'CN8plus', 'year'))
data_dummy = merge(data_dummy, product_collapsed_lag, all.x = T, by = c( 'ctry', 'CN8plus', 'year'))

## add in the remainder of the simulated variables 
interest_vars = setdiff( names(data),c('empl_bucket','export_ratio',names(data_dummy)))
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)

# add last two variables 
data_dummy[, empl_bucket := ifelse(empl < 10, "0-10", ifelse(empl < 50, '10-50', ifelse(empl < 200, '50-200', ifelse(empl >=200, '200+', NA))))]
data_dummy[,export_ratio := deflated_value/deflated_dom_turnover][deflated_dom_turnover ==0, export_ratio := NA]
setdiff( names(data),names(data_dummy))
fwrite(data_dummy,'../1a) dummy data/13b_product_level_growth_analysis_inputs.csv')


