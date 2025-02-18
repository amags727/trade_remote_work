# Setup -------------------------------------------------------------------
rm(list = ls())
packages = c('rstudioapi', 'data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','harmonizer',
             'rnaturalearth', 'rnaturalearthdata', 'geosphere', 'sf', 'countrycode')
lapply(packages, function(package){ tryCatch({ library(package, character.only = T)},
                                             error = function(cond){install.packages(package);
                                               library(package, character.only = T)})})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../1) data/similarity_matrices')


# import raw data and generate countries list ---------------------------
border_data <- read.csv("inputs/country_borders_dta.csv")  %>% 
  mutate(country_code = ifelse(country_name == "Namibia", "NA", country_code),
         country_border_code = ifelse(country_border_name == "Namibia", "NA", country_border_code))

region_data = read.csv('inputs/Countries_by_region.csv') %>% 
  rename(country = Economy,wb_region = Region) %>% select(-Code) %>%
  mutate(country_code = ifelse(country == 'Kosovo', 'XK', countrycode(country, "country.name", "iso2c"))) %>%
  add_row(., country = "Ivory Coast",wb_region = 'Sub-Saharan Africa',country_code =  'CI')

language_data <- st_read('inputs/soc_071_world_languages/World_Languages.shp') %>% filter(TYPE_=='Sovereign country' | TYPE_=='Country') %>% 
  mutate(country_code = ifelse(COUNTRY == 'Kosovo', 'XK', countrycode(COUNTRY, "country.name", "iso2c"))) %>%
  select(country_code, COUNTRY, everything()) %>%
  separate(WORLD_LANG, into = paste0("WORLD_LANG_",1:2), sep = "_", fill = "right") %>% as.data.table() %>% 
  select(c(country_code,FIRST_OFFI, SECOND_OFF, THIRD_OFFI, WORLD_LANG_1, WORLD_LANG_2), -geometry) %>%
  pivot_longer(cols = -country_code, values_to = 'language') %>% filter(!is.na(language), language != "Other") %>% select(-name) %>% unique()

population = read.csv('inputs/population.csv'); names(population) = c('country', as.character(1980:2028))
population = population %>% mutate(ctry = countrycode(country,"country.name", "iso2c")) %>%
  filter(!is.na(ctry)) %>% select(-country)

gdp_per_capita = read.csv('inputs/gdp_per_capita.csv'); names(gdp_per_capita) = c('country', as.character(1980:2028))
gdp_per_capita =  gdp_per_capita %>% mutate(ctry = countrycode(country,"country.name", "iso2c")) %>%
  filter(!is.na(ctry)) %>% select(-country)

countries = c('CI',intersect(population$ctry,gdp_per_capita$ctry) %>% intersect(., c("BS", language_data$country_code))) %>% sort()

# generate similiarity data ---------------------------
similiarity_data =  lapply(countries, function(ctry){
  share_language = language_data %>% filter(country_code == ctry) %>% select(language) %>%
    merge(language_data, all.x = T) %>% pull(country_code) %>% unique() %>% sort()
  share_region = region_data %>% filter(country_code == ctry) %>% select(wb_region) %>%
    merge(region_data, all.x = T) %>% pull(country_code) %>% unique() %>% sort()
  share_border = border_data %>% filter(country_code == ctry) %>% pull(country_border_code) %>% 
    unique() %>% sort()
  
  output = data.frame(ctry = ctry, share_language = I(list(share_language)),
                      share_region = I(list(share_region)), share_border = I(list(share_border)))
}) %>% rbindlist()
write_rds(similiarity_data,'outputs/similiarity_data.rds')



# Generate GDP and population time series ---------------------------------------------
vars = c('population','gdp_per_capita')

data = lapply(1:2, function(i){
  data = get(vars[i])
  data = data %>% pivot_longer(cols = -ctry, names_to = 'year', values_to = vars[i]) %>% 
    filter(year > 1992, year< 2021, ctry %in% countries)
})
write.csv(merge(data[[1]], data[[2]]), 'outputs/gdp_population_data.csv')

# generate distance to france ---------------------------------------------
france_distance_data = read_xls('inputs/dist_cepii.xls') %>% 
  mutate(ctry = countrycode(iso_d, 'iso3c', 'iso2c')) %>% 
  filter(iso_o == 'FRA', ctry %in% countries) %>%
  rename(distance_to_france = dist) %>%
  select(ctry, dist_to_france)

fwrite(france_distance_data, 'outputs/france_distance_data.csv')


