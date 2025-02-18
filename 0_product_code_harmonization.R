# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','harmonizer')
lapply(packages, function(package){ tryCatch({ library(package, character.only = T)},
                                             error = function(cond){ install.packages(package);library(package, character.only = T)})})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../1) data/CN8_code_harmonization')

# harmonize CN-08 codes  ---------------------------------------------------

# ##harmonizer doesn't go back all the way so we use it to generate 1995-2002 then add the remaining vars manually
# ##concordances come from harmonizer documentation 
# CN8_95_2022 = as.data.table(harmonize_cn8(1995, 2022)  %>% select(-flag:-BEC_agr,-SNA))
# fwrite(CN8_dt, 'output/CN8_harmonized_1995to2022.csv')

CN8_95_2022=read_csv('output/CN8_harmonized_1995to2022.csv', col_types = cols(.default = 'c'))
CN8_concordances_1988_2022 <-as.data.table(read.csv('input/CN8_concordances_1988_2022.csv', sep=";", 
                                                    colClasses = c(rep('numeric', 2), rep('character',2))))

#the concordance table for 94-95 equals codes that changed between the two years
# and codes that were in 95 but not amongst the changers. This will add new 95 codes to 94
# which isnt a problem bc no products will be assigned to them, and drop 94 codes that have no 95 equiv
# which is also fine since the goal is a harmonized dataset that excludes those anyway

CN8_94_95 = CN8_concordances_1988_2022[from == 1994] %>% select(obsolete,new)
constant_94_95 = unique(CN8_95_2022$CN8_1995[!CN8_95_2022$CN8_1995 %in% CN8_94_95$new])
CN8_94_95 = rbind(CN8_94_95, data.frame(obsolete=constant_94_95, new= constant_94_95)) %>% 
  rename(CN8_1994 = obsolete, CN8_1995 = new)

CN8_93_94 = CN8_concordances_1988_2022[from == 1993] %>% select(obsolete,new)
constant_93_94 = unique(CN8_94_95$CN8_1994[!CN8_94_95$CN8_1994 %in% CN8_93_94$new])
CN8_93_94 = rbind(CN8_93_94, data.frame(obsolete=constant_93_94, new= constant_93_94)) %>% 
  rename(CN8_1993 = obsolete, CN8_1994 = new)

CN8_94_22= merge(CN8_94_95, CN8_95_2022, all.y = T)
CN8_93_22 = merge(CN8_93_94, CN8_94_22, by = 'CN8_1994', all.y =T) %>% select(paste0('CN8_',1993:2022), everything())


## now harmonize the remaining two years 
dt = CN8_93_22
start = 1993
end = 2022
dt$ignorable = T
for (yr in start:end){
  year_var = paste0('CN8_',yr)
  dt[,count := .N, by = get(year_var)] 
  dt[count >1, ignorable :=F]
}
dt[,count:=NULL]

unproblematic_codes = dt %>% filter(ignorable) %>% mutate(CN8plus = CN8_2022)
problematic_codes = as.data.table(dt %>% filter(!ignorable)) %>% mutate(CN8plus = CN8_2022)
problematic_codes$index = 1:nrow(problematic_codes)



i_cleared = F
for (yr in start:end){
  print(yr)
  year_var = paste0('CN8_', yr)
  for (i in 1:nrow(problematic_codes)){
    year_value = problematic_codes[[year_var]][i]
    final_value = problematic_codes$CN8plus[i]
    
    while (!i_cleared){
      unmatched_indeces = problematic_codes[index>i & get(year_var) == year_value & CN8plus != final_value,index]
      if(length(unmatched_indeces) > 0){
        current_l = length(unmatched_indeces); growing = T
        while(growing){
          unmatched_indeces_new = problematic_codes[CN8plus %in% problematic_codes$CN8plus[unmatched_indeces] |
                                                    get(year_var) %in% problematic_codes[[year_var]][unmatched_indeces]] %>% pull(index)
          new_l = length(unmatched_indeces_new)
          if(new_l > current_l){
            current_l = new_l; unmatched_indeces = unmatched_indeces_new
          }else{
            growing = F
          }
        }
        problematic_codes[get(year_var) == year_value | index %in% unmatched_indeces, CN8plus := final_value]  
      }else{
        break
      }
    }
  }
}


## each year code should have 1 unique final code 
for (yr in start:end){
  year_var = paste0('CN8_',yr)
  temp = problematic_codes %>% group_by(!!sym(year_var)) %>%
    filter(!duplicated(CN8plus)) %>%
    mutate(count = ifelse(!is.na(get(year_var)), n(), NA)) %>%
    filter(count>1)
  
  if (nrow(temp) > 0){
    print(paste('error in',yr))
  }
}


harmonized_CN8 = rbind(problematic_codes %>% select(-index), unproblematic_codes)
fwrite(harmonized_CN8, 'output/CN8_harmonized_1993to2022.csv')
