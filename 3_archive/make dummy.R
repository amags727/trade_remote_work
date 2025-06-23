if (!file.exists('1) data/16_bs_br_linkedin.parquet')){
  # prepare for randomization if necessary 
  dummy_version = grepl("/Users/amagnuson/Library/CloudStorage/", getwd()); set.seed(1)
  
  linkedin = import_parquet('1) data/15_french_affiliated_firm_roles_collapsed_clean.parquet') %>%
    select(-c(rcid, `__index_level_0__`)) 
  
  ## if necessary anonymize linkedin data 
  if (dummy_version){
    linkedin = merge(linkedin, data.table(firmid = linkedin[!duplicated(firmid) & has_siren][['firmid']]) %>%
                       mutate(firmid_new =  as.character(sample(seq_len(nrow(.)), size = nrow(.), replace = FALSE)),
                              replace = sample(c(0,1), size = nrow(.), replace = T)),
                     all.x = T, by = 'firmid') %>%
      .[!is.na(firmid_new) & replace == 1, firmid := firmid_new] %>% select(-firmid_new, replace)
  }
  
  ## merge together
  bs_br_linkedin = import_csv('1) data/3_bs_br_data.parquet', char_vars =  c('firmid')) %>% 
    merge(linkedin, all.x = T)
  
  #### output both a real and dummy version 
  {
    write_parquet(bs_br_linkedin, '1) data/16_bs_br_linkedin.parquet') 
    
    ### generate the dummy dataset for later use   
    firm_id_threshold = 4
    data = import_file('1) data/16_bs_br_linkedin.parquet') %>%
      select(-c('has_siren', 'rcid_count','needs_collapse')) %>% 
      .[, matched := !is.na(comp_data)] %>%
      .[, count:= .N, by = .(NACE_BR, year, matched)] %>%
      .[count > firm_id_threshold] %>% .[,count := NULL]
    
    num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data)
    year_nace_matched = data[, .(NACE_BR, year, matched)]
    
    # for each obs. randomly assign an id and then draw from the joint distrib of year-NACE industry combinations
    data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T)) %>%
      cbind(.,year_nace_matched[sample(1:num_data_points, num_data_points, T)]) %>% unique()
    
    # for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)
    group_vars = c('NACE_BR', 'year', 'matched')
    discrete_vars = c('public', 'likely_french', 'subsidiary', 'has_lei')
    linkedin_only_vars = names(data) %>% .[grepl('emp_|comp_',.)]
    data_dummy = lapply(c(F,T), function(linkedin_match){
      continuous_vars = setdiff( names(data),c('empl_bucket','replace','group_code', names(data_dummy), discrete_vars)) 
      if(linkedin_match){
        temp = simulate_discrete_vars(data, data_dummy[matched==T], group_vars, discrete_vars)
      } else{
        temp = data_dummy[matched== F] 
        continuous_vars = setdiff(continuous_vars,linkedin_only_vars) 
      }
      temp = simulate_continuous_vars(data, temp, group_vars, continuous_vars)
    }) %>% rbindlist(use.names = T, fill = T)
    
    
    # add any vars still missing
    data_dummy[, empl_bucket := ifelse(empl < 10, "0-10", ifelse(empl < 50, '10-50', ifelse(empl < 200, '50-200', ifelse(empl >=200, '200+', NA))))]
    if(dir.exists('1a) dummy data/')){
      write_parquet(data_dummy, '1a) dummy data/16_bs_br_linkedin.parquet') 
    } else{
      print('dummy data directory doesnt exist')
    }
    }
  
}
