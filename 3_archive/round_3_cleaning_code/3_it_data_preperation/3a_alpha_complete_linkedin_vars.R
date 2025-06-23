
linkedin_base_data = import_file(gsub("_dummy", "",linkedin_basic_path)) %>% 
  .[, `:=`(empl_abroad = empl_total - empl_french,
           comp_abroad = comp_total - comp_french,
           share_comp_abroad =(comp_total - comp_french)/ comp_total) ] %>% 
  unbalanced_lag(., 'firmid', 'year',con_fil(names(.),'comp', 'empl'),1)

write_parquet(linkedin_base_data,gsub("_dummy", "",linkedin_basic_path))

