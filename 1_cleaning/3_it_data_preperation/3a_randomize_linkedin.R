set.seed(43)
matching_output = import_file(linkedin_match_path) %>% 
  mutate(firmid_new =  as.character(sample(seq_len(nrow(.)), size = nrow(.), replace = FALSE)))

for (file in c(linkedin_match_path,linkedin_ctry_lvl_path, linkedin_basic_path)){
  merge(import_file(file), matching_output[,.(firmid,firmid_new)]) %>%
    select(-firmid) %>% rename('firmid' ='firmid_new') %>% 
    write_parquet(.,gsub('.par', '_dummy.par', file))
}

linkedin_ctry_lvl_path = gsub('.par', '_dummy.par', linkedin_ctry_lvl_path)
linkedin_basic_path = gsub('.par', '_dummy.par',  linkedin_basic_path)
linkedin_match_path = gsub('.par', '_dummy.par', linkedin_match_path)
rm(list= setdiff(ls(), base_env)); gc()