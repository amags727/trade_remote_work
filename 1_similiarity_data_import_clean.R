# Setup -------------------------------------------------------------------
rm(list = ls())
#install.packages('harmonizer')
#install.packages('tidyverse')
#install.packages('rstudioapi')
#install.packages('data.table')
library(harmonizer)
library(tidyverse)
library(rstudioapi)
library(data.table)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(geosphere)
library(sf)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(countrycode)
setwd('../1) data/similarity_matrices/inputs')




# import raw data and generate countries list ---------------------------
  border_data <- read.csv("country_borders_dta.csv")  %>% 
    mutate(country_code = ifelse(country_name == "Namibia", "NA", country_code),
           country_border_code = ifelse(country_border_name == "Namibia", "NA", country_border_code))
  
  region_data = read.csv('Countries_by_region.csv') %>% 
    rename(country = Economy,wb_region = Region) %>% select(-Code) %>%
    mutate(country_code = ifelse(country == 'Kosovo', 'XK', countrycode(country, "country.name", "iso2c"))) %>%
    filter(country!= 'Channel Islands')
  
  language_data <- st_read('soc_071_world_languages/World_Languages.shp') %>% filter(TYPE_=='Sovereign country' | TYPE_=='Country') %>% 
    mutate(country_code = ifelse(COUNTRY == 'Kosovo', 'XK', countrycode(COUNTRY, "country.name", "iso2c"))) %>%
    select(country_code, COUNTRY, everything()) 
  countries <- sort(c('BS',unique(language_data$country_code)))
  
  population = read.csv('population.csv'); names(population) = c('country', as.character(1980:2028))
  population = population %>% mutate(ctry = countrycode(country,"country.name", "iso2c")) %>%
    filter(!is.na(ctry)) %>% select(-country)
  
  gdp_per_capita = read.csv('gdp_per_capita.csv'); names(gdp_per_capita) = c('country', as.character(1980:2028))
  gdp_per_capita =  gdp_per_capita %>% mutate(ctry = countrycode(country,"country.name", "iso2c")) %>%
    filter(!is.na(ctry)) %>% select(-country)
  
  worldbank_countries = unique(intersect(population$ctry,gdp_per_capita$ctry) )
  countries = intersect(countries, worldbank_countries)
  
  
  
  border_matrix <- matrix(0, nrow = length(countries), ncol = length(countries))
  

  colnames(border_matrix) <- countries
  rownames(border_matrix) <- countries

  language_matrix = border_matrix
  region_matrix = border_matrix 
  
  setwd('../outputs')
# generate border, language, region similiarity ---------------------------
  ## border matrix -----------------------------------------------------------
  for(row in 1:nrow(border_data)) {
    print(row)
    country <- border_data$country_code[row]
    border_country <- border_data$country_border_code[row]
    
    if (country %in% countries & border_country %in% countries ){
    if (nchar(border_country)>1){
  
    border_matrix[country, border_country] <- 1
    border_matrix[border_country, country] <- 1  # Assuming the relationship is bidirectional
    }
  
    }
  }
  for (i in 1:nrow(border_matrix)){
    border_matrix[i,i] =1
  }
  
  export = as.data.table(border_matrix, keep.rownames = T) %>% rename(ctry = rn)
  colnames(export) = c('ctry', paste0('border_', colnames(border_matrix)))
  saveRDS(export, 'border_matrix.rds')
  
  border_matrix_og = border_matrix
  border_matrix = as.data.table(border_matrix, keep.rownames = T) %>% rename(ctry = rn)
  border_matrix = border_matrix[,share_border := apply(.SD, 1, function(x) names(.SD)[x==1]), .SDcols = 2:ncol(border_matrix)] %>%
    select(ctry, share_border)
  ## Language similarity matrix  ---------------------------------------------
  #deal with the coding of the world language variable 
  split_columns <- strsplit(language_data$WORLD_LANG, "_")
  max_length <- max(sapply(split_columns, length))
  split_columns <- lapply(split_columns, function(x) {
    length(x) <- max_length
    x
  })
  new_columns <- do.call(cbind, split_columns)
  language_data <- cbind(language_data, t(new_columns))  %>% select(-WORLD_LANG) %>% 
    rename(WORLD_LANG_1 = X1,WORLD_LANG_2 = X2)
  
  
  # Iterate over each pair of countries
  for (i in 1:length(countries)) {
    print(i)
    if (countries[i] =='BS'){
      langs_i = "English"
    }else{
    langs_i <- unlist(language_data[language_data$country_code == countries[i], c("FIRST_OFFI", "SECOND_OFF", "THIRD_OFFI", "WORLD_LANG_1", "WORLD_LANG_2")], use.names = FALSE)[1:5]
    langs_i <- langs_i[!is.na(langs_i) & langs_i != "Other"]
    }
    for (j in i:length(countries)) {
      if(countries[j] == 'BS'){
        langs_j = "English"
      } else{
      langs_j <- unlist(language_data[language_data$country_code == countries[j], c("FIRST_OFFI", "SECOND_OFF", "THIRD_OFFI", "WORLD_LANG_1", "WORLD_LANG_2")], use.names = FALSE)[1:5]
      langs_j <- langs_j[!is.na(langs_j) & langs_j != "Other"]
      }
        # Check for common languages
        language_matrix[i, j] <- as.numeric(length(intersect(langs_i, langs_j)) > 0)
        language_matrix[j, i] <- language_matrix[i, j]
    }
  }
  export = as.data.table(language_matrix, keep.rownames = T) %>% rename(ctry = rn)
  colnames(export) = c('ctry', paste0('language_', colnames(language_matrix)))
  saveRDS(export, 'language_matrix.rds')
  
  language_matrix = as.data.table(language_matrix, keep.rownames = T) %>% rename(ctry = rn)
  language_matrix = language_matrix[,share_language := apply(.SD, 1, function(x) names(.SD)[x==1]), .SDcols = 2:ncol(language_matrix)] %>%
    select(ctry, share_language)
  ##  wb region ---------------------------------------------------
  for (i in 1:length(countries)) {
    wb_region_i = region_data$wb_region[region_data$country_code == countries[i]]
    for(j in i:length(countries)){
      wb_region_j = region_data$wb_region[region_data$country_code == countries[j]]
      if(length(wb_region_i)> 1){
        print(paste0('i messup: ', i, "j = "))
      }
      if(length(wb_region_j)> 1){
        print(paste0('j messup: ', i))
      }
      # countries are in the same region if listed as such or if they share a border
      if (wb_region_i == wb_region_j | border_matrix_og[i,j] ==1) {
        region_matrix[i,j] = 1
        region_matrix[j,i] = 1
      } 
    }
  }
  export = as.data.table(region_matrix, keep.rownames = T) %>% rename(ctry = rn)
  colnames(export) = c('ctry', paste0('region_', colnames(region_matrix)))
  saveRDS(export, 'region_matrix.rds')
  
  region_matrix = as.data.table(region_matrix, keep.rownames = T) %>% rename(ctry = rn)
  region_matrix = region_matrix[,share_region := apply(.SD, 1, function(x) names(.SD)[x==1]), .SDcols = 2:ncol(region_matrix)] %>%
    select(ctry, share_region)
  
  ## export data -------------------------------------------------------------
  similiarity_data = merge(merge(region_matrix, border_matrix),language_matrix) 
  saveRDS(similiarity_data, 'similiarity_data.rds')
  

  # Generate GDP and population time series ---------------------------------------------
  vars = c('population','gdp_per_capita')
  
  data = lapply(1:2, function(i){
    data = get(vars[i])
    data = data %>% pivot_longer(cols = -ctry, names_to = 'year', values_to = vars[i]) %>% 
      filter(year > 1992, year< 2021, ctry %in% countries)
  })
  write.csv(merge(data[[1]], data[[2]]), 'gdp_population_data.csv')

# generate distance to france ---------------------------------------------
  ##we're going to use the distance from the center of the largest continuous area of each country 
  find_largest_polygon_centroid = function(geometry){
    if(st_geometry_type(geometry)!= 'MULTIPOLYGON'){
      return(st_centroid(geometry))
    }else{ 
      polygons = st_cast(geometry, 'POLYGON')
      areas = st_area(polygons)
      largest_polygon = polygons[which.max(areas)]
      return(st_centroid(largest_polygon))}
  }
  
  world = ne_countries(scale = 'medium', returnclass = 'sf')
  world = st_transform(world, crs = 3395)
  
  centroids = world
  for (i in 1:nrow(world)){
    print(i/nrow(world))
    centroids$geometry[i] = find_largest_polygon_centroid(world$geometry[i])
  }
  
  france_centroid = centroids[world$name == 'France',]
  distances = st_distance(centroids, france_centroid, by_element = T)
  france_distance_data = data.frame(ctry = world$iso_a2, distance_to_france = as.numeric(distances)/1000) %>% filter(ctry %in% countries)
  write.csv(france_distance_data, 'france_distance_data.csv', row.names = F)

