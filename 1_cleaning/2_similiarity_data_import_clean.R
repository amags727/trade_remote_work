# setup -------------------------------------------------------------------
rm(list = ls());gc()

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
similiarity_dir = '1) data/0_misc_data/0c_similarity_matrices/'


# import raw data  ---------------------------
ctry_dict = import_file('1) data/0_misc_data/0b_dictionaries/0b2_ctry_dict.parquet')

## import border data and add missing observations for equitorial guinea 
border_data <- import_file(similiarity_dir,"inputs/country_borders_dta.csv")  %>% 
  mutate(country_code = ifelse(country_name == "Namibia", "NA", country_code),
         country_border_code = ifelse(country_border_name == "Namibia", "NA", country_border_code))  %>% 
  rename(ctry = country_code) %>% numeric_ctry() %>% 
  rename(ctry = country_border_code) %>% numeric_ctry() %>% rename_with(~c('x','y', 'ctry_num', 'border_ctry_num')) %>%
  .[,.(ctry_num, border_ctry_num)] %>% rbind(., data.frame(ctry_num =c(68,68, 47, 78), border_ctry_num = c(47,78, 68, 68)))


region_data = import_file(similiarity_dir,'inputs/Countries_by_region.csv') %>% 
  rename(country = Economy,wb_region = Region) %>% select(-Code) %>%
  mutate(country_code = ifelse(country == 'Kosovo', 'XK', countrycode(country, "country.name", "iso2c"))) %>%
  add_row(., country = "Ivory Coast",wb_region = 'Sub-Saharan Africa',country_code =  'CI') %>% rename(ctry =country_code) %>% 
  select(ctry, wb_region) %>% 
  rbind(., import_file(similiarity_dir, 'inputs/supplementary_region_data.csv', col_select = c('ctry', 'wb_region'))) %>% 
  .[wb_region != ''] %>% numeric_ctry()

language_data <- import_file(similiarity_dir, 'inputs/soc_071_world_languages/World_Languages.shp') %>% 
  mutate(ctry = ifelse(COUNTRY == 'Kosovo', 'XK', countrycode(COUNTRY, "country.name", "iso2c"))) %>%
  select(ctry, COUNTRY, everything()) %>%
  separate(WORLD_LANG, into = paste0("WORLD_LANG_",1:2), sep = "_", fill = "right") %>% as.data.table() %>% 
  select(c(ctry,FIRST_OFFI, SECOND_OFF, THIRD_OFFI, WORLD_LANG_1, WORLD_LANG_2), -geometry) %>%
  pivot_longer(cols = -ctry, values_to = 'language') %>% filter(!is.na(language), language != "Other") %>% select(-name) %>% unique() %>% 
  select(ctry, language) %>% 
  rbind(import_file(similiarity_dir, 'inputs/supplementary_language_data.csv', col_select = c('ctry', 'language'))) %>% numeric_ctry()

# generate similiarity data ---------------------------
similiarity_data =  lapply(ctry_dict$ctry_num, function(ctry){
  share_language = language_data %>% filter(ctry_num == ctry) %>% select(language) %>%
    merge(language_data, all.x = T) %>% pull(ctry_num) %>% unique() %>% sort() %>% setdiff(., ctry)
  share_region = region_data %>% filter(ctry_num == ctry) %>% select(wb_region) %>%
    merge(region_data, all.x = T, by = 'wb_region') %>% pull(ctry_num) %>% unique() %>% sort()  %>% setdiff(., ctry)
  share_border = border_data %>% filter(ctry_num == ctry_num) %>% pull(border_ctry_num) %>% 
    unique() %>% sort() %>% setdiff(., ctry)
  
  output = data.frame(ctry_num = ctry, share_language = I(list(share_language)),
                      share_region = I(list(share_region)), share_border = I(list(share_border)))
}) %>% rbindlist()
write_rds(similiarity_data, paste0(similiarity_dir,'outputs/similiarity_data.rds'))

# generate distance to france ---------------------------------------------
countries = ne_countries(scale = 'medium', returnclass ='sf') %>% 
  mutate(geometry = st_make_valid(geometry)) %>%
  mutate(mainland = st_geometry(.)) %>% 
  rowwise() %>% mutate(mainland = get_largest_polygon(mainland)) %>% 
  ungroup() %>% 
  mutate(mainland_centroid = st_centroid(mainland))
coords = st_coordinates(countries$mainland_centroid)
countries= data.table(ctry = countries$iso_a2, lon = coords[,"X"],lat = coords[,"Y"]) %>% numeric_ctry()

country_distances = expand(countries$ctry_num, countries$ctry_num, names = c('o_ctry_num', 'd_ctry_num')) %>% 
  merge(countries %>% rename_with(~paste0('o_', names(countries))), by = 'o_ctry_num') %>%
  merge(countries %>% rename_with(~paste0('d_', names(countries))), by = 'd_ctry_num') %>% 
  .[o_ctry_num != d_ctry_num] %>% 
  .[,distance_km := distHaversine(cbind(o_lon, o_lat), cbind(d_lon, d_lat)) / 1000] %>% 
  select(-con_fil(.,'lon', 'lat'))
fwrite(country_distances, paste0(similiarity_dir,'outputs/overall_distance_data.csv'))

france_distance_data = country_distances[o_ctry_num == 0] %>% rename(ctry_num = d_ctry_num, distance_to_france_km = distance_km) %>% select(-o_ctry_num)
fwrite(france_distance_data, paste0(similiarity_dir,'outputs/france_distance_data.csv'))


