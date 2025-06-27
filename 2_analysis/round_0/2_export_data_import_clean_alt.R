# setup
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}
library(data.table)
library(haven)
library(readxl)
library(openxlsx)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(plm)
library(Matrix)
library(parallel)
library(tictoc)
library(bigmemory)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

project_dir = 'C:/Users/NEWPROD_A_MAGNUSO/Documents/Big data Project'
raw_dir = 'C:/Users/Public/1. Microprod/0. Raw data processing/Data/'


size = function(data_frame){
  size = c(nrow(data_frame), ncol(data_frame))
  return(size)
}
# Import raw data for analysis -------------------------------------
setwd(raw_dir)
start = 1993
end = 2021

export_data = rbindlist(lapply(c(start:end),function(yr){
  print(yr)
  export_data = fread(paste0("itgs/itgs",yr,".csv"))
  export_data = export_data[exim == 2]
  export_data[, firmid := as.character(firmid)]
  export_data = export_data[, .(products = .N, value = sum(ntrade)), by = .(firmid,ctry, year)]
}), fill = T)

setorder(export_data, firmid, ctry, year)

#identify streaks in exporting 
export_data[, `:=`(year_diff = c(1, diff(year)), next_year =  shift(year, type = 'lead')), by = .(firmid, ctry)]
export_data[, streak_id := rleid(firmid, ctry, year_diff!=1)]
export_data[, streak_id := ifelse(!is.na(next_year) & year == next_year -1, shift(streak_id, type= 'lead'), streak_id)]
export_data[, `:=`(year_in_streak = seq_len(.N) -1,
                   streak_start = min(year)), by = .(streak_id)]
export_data[, c('year_diff', 'next_year'):= NULL]

#remove streaks when we don't know when they start 
export_data[streak_start ==start, `:=`(streak_start = NA, year_in_streak =NA)]
export_data
## add in the BR registry data
start = 1994
end = 2020

br_data = rbindlist(lapply(c(start:end),function(yr){
  print(yr)
  br_path = paste0(raw_dir,"br/br",yr,".csv" )
  br_data_temp = fread(br_path, select = c('ENT_ID','year','empl'))
  br_data_temp[, `:=`(firmid = as.character(ENT_ID))]
  br_data_temp = br_data_temp[empl > 0]
  br_data_temp = br_data_temp %>% select(firmid, year,empl)
}), fill = T)
br_data[, `:=`(birth_year = min(year),
               death_year = max(year)), by = firmid]
# br_data[birth_year == start, birth_year := NA]
# br_data[death_year == end, death_year := NA]
birth_death_data = unique(br_data %>% select(firmid, birth_year, death_year))
write.csv(birth_death_data, '../1) data/br_birth_and_death.csv', row.names = F)
export_data = merge(export_data, birth_death_data, all.x = T)
write.csv(export_data, 'export_data.csv')
export_data_og = export_data


# generate data on firm export portfolio --------------------------------------------------
export_data = fread('../1) data/export_data.csv') 
export_data = export_data[!is.na(birth_year) & year <= death_year & year >= birth_year]
export_data$in_country =1
setorder(export_data, firmid, year)

## we have to divide the dataset to get it to run; this way ensures we don't split firms 
export_data$indeces = 1:nrow(export_data)
interval_num = 10000
firms = unique(export_data$firmid)
division_firms = firms[ceiling(seq(1, length(firms), length.out = interval_num))]
indeces = export_data[firmid %in% division_firms, .(index = min(indeces)), by = firmid] %>% pull(index)
bounds = data.frame( lb = indeces[1: interval_num-1],
                     ub = indeces[2: interval_num]-1)
bounds$ub[interval_num-1] = nrow(export_data)
# construct each firm's portfolio of countries ----------------------------------------
#construct the initial dataset; including where firms do and do not export
similiarity_data = readRDS('../1) data/similarity_matrices/outputs/similiarity_data.rds')
countries = unique(similiarity_data$ctry)
countries_border = paste0('border_', countries)
countries_region = paste0('region_', countries)
countries_language = paste0('language_', countries)

border_matrix = readRDS('../1) data/similarity_matrices/outputs/border_matrix.rds')
region_matrix = readRDS('../1) data/similarity_matrices/outputs/region_matrix.rds')
language_matrix = readRDS('../1) data/similarity_matrices/outputs/language_matrix.rds')
similiarity_matrix = merge(merge(border_matrix, region_matrix),language_matrix)
tic()
temp = export_data[bounds$lb[i]: bounds$ub[i]]
temp = temp[ctry %in% countries]
portfolio_long = temp %>% select('firmid', 'ctry','year', 'in_country')
birth_death = unique(temp %>% select(firmid, birth_year, death_year))
potential_data_points = as.data.table(expand.grid(unique(temp$firmid), 1994:2020, countries)
                                      %>% rename(firmid = Var1, year = Var2, ctry = Var3))
potential_data_points = merge(potential_data_points, birth_death)
potential_data_points = potential_data_points[year <= death_year & year >= birth_year]
portfolio_long = merge(potential_data_points %>% select(-birth_year, -death_year), 
                       temp %>% select('firmid', 'ctry','year', 'in_country'),
                       by= c('firmid', 'ctry','year'),
                       all.x = T)
portfolio_long[, in_country := replace(in_country, is.na(in_country), 0)]

portfolio_wide = portfolio_long %>% spread(key = ctry, value = in_country)

portfolio_long = merge(portfolio_long, portfolio_wide, by = c('firmid', 'year'))
portfolio_long[, total_countries := rowSums(.SD, na.rm = T), .SDcols = countries]

vars = c('region', 'language', 'border', 'total_countries', 'in_countries')
vars_l = paste0(vars, '_l')
portfolio_long = merge(portfolio_long, similiarity_matrix, by = 'ctry')
for (j in seq_along(vars)){
if (j<4){
countries_var = get(paste0('countries_', vars[j]))
portfolio_long[, vars[j] := Reduce('+', Map('*',.SD[,..countries], .SD[,..countries_var]))]
portfolio_long[,(countries_var):= NULL]
}
portfolio_long[,vars_l[j]:= shift(get(vars[j])), by = .(firmid,ctry)]
}
portfolio_long[, (countries) := NULL]
toc()

portfolio_long = merge(portfolio_long, language_matrix, by = 'ctry')
portfolio_long[,region := 0]
for (j in seq_along(countries)){
  portfolio_long[, `:=`(region = Reduce('+', Map('*',.SD[,..countries], .SD[,..countries_r])))]
  
}


cl = makeCluster(3)
input =as.list(1:(interval_num -1))
make_portfolio = function(i){
  print(i)
  temp = export_data[bounds$lb[i]: bounds$ub[i],]
  portfolio_long = temp %>% select('firmid', 'ctry','year', 'in_country')
  birth_death = unique(temp %>% select(firmid, birth_year, death_year))
  potential_data_points = as.data.table(expand.grid(unique(temp$firmid), 1994:2020, countries)
                                        %>% rename(firmid = Var1, year = Var2, ctry = Var3))
  potential_data_points = merge(potential_data_points, birth_death)
  potential_data_points = potential_data_points[year <= death_year & year >= birth_year]
  portfolio_long = merge(potential_data_points %>% select(-birth_year, -death_year), 
                         temp %>% select('firmid', 'ctry','year', 'in_country'),
                         by= c('firmid', 'ctry','year'),
                         all.x = T)
  portfolio_long[, in_country := replace(in_country, is.na(in_country), 0)]
  
  #add in the similarity data
  portfolio_long = merge(portfolio_long, similiarity_data, by = 'ctry') %>% select(-ctry) %>% 
    rename(ctry = ctry_code) %>% select(firmid, year, ctry, in_country, everything())
  
  setorder(portfolio_long,firmid, ctry, year)
  vars = c('language', 'border', 'region', 'in_country')
  share_vars = paste0('share_', vars)
  lag_vars = paste0(vars, '_l')
  for( j in seq_along(vars)){
    if(vars[j]!= 'in_country'){
      portfolio_long[in_country==0, share_vars[j] :=""]
      portfolio_long[, vars[j] :=  as.numeric(ctry %in% Reduce(union, get(share_vars[j]))), by = .(firmid,year)]
      # portfolio_long[, vars[j] := 
      #                  ifelse(in_country==1,1,
      #                         as.numeric(ctry %in% Reduce(union, get(share_vars[j])))),
      #                by = .(firmid,year)]
      portfolio_long[, share_vars[j]:= NULL]  
    }
    portfolio_long[, lag_vars[j]:= shift(get(vars[j])), by = .(firmid,ctry)]
  }
  portfolio_long = as(as.matrix(portfolio_long), 'sparseMatrix')
  saveRDS(portfolio_long, paste0('../1) data/portfolio component data/comp_', i,'.rds'))
}

clusterExport(cl, c('make_portfolio', 'bounds', 'similiarity_data', 'countries', 'export_data'))
clusterEvalQ(cl, {
  library(data.table)
  library(haven)
  library(readxl)
  library(openxlsx)
  library(stringr)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(janitor)
  library(plm)
  library(Matrix)
})

parLapply(cl,input, make_portfolio)
stopCluster(cl)
saveRDS(portfolio_long, 'portfolio_long.rds')
portfolio_long = do.call(rbind, portfolio_long) 
saveRDS(portfolio_long, 'portfolio_long_final.rds')



components = list.files('../1) data/portfolio component data/')
indeces = data.frame(lb = rep(0, length(components)),
                     ub = rep(0, length(components)))
nrow = 0 
ncol = 0
dataset_list = lapply(seq_along(components), function(i){
  print(i)
  readRDS(paste0('../1) data/portfolio component data/', components[i]))
})

for ( i in seq_along(components)){
  if (i == 1){
    indeces$lb[i] = 1
    indeces$ub[i] = nrow(dataset_list[[i]])
  }
  else{
    indeces$lb[i] = indeces$ub[i-1]+1
    indeces$ub[i] = indeces$ub[i-1]+nrow(dataset_list[[i]])
  }
  nrow = nrow + nrow(dataset_list[[i]])
  ncol = max(ncol, ncol(dataset_list[[i]]))
}

hi = as.big.matrix(as.matrix(dataset_list[[i]]))
hi = as.big.matrix(hi)
portfolio_long = filebacked.big.matrix(nrow = nrow, ncol = ncol, type = typeof(as.big.matrix(as.matrix(dataset_list[[i]]))),
                                       backingfile =  'portfolio_long.bin',
                                       descriptorfile =  'portfolio_long.desc',
                                       backingpath = '../1) data/') 

for (i in seq_along(components)){
  portfolio_long[indeces$lb[i]:indeces$ub[i],] = as.numeric(as.matrix(dataset_list[[i]]))
}

