#basic setup 
rm(list = ls());
setwd('../../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../../..')}
source('2) code/0_set_parameter_values.R')


# update firm_yr_lvl ---------------------------------------------------------------
d_vars = c("comp_data", "share_comp_data")
divisions_list = list(list('nace', c('NACE_BR', 'year')),list('nace_exporter', c('NACE_BR', 'currently_export', 'year'))) 
base = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% select(- con_fil(., 'quartile'))

for (i in length(divisions_list)){inner = divisions_list[[i]][[1]]; outer = ''; group = divisions_list[[i]][[2]]
for (j in 1:2){if (j ==2){outer = "_age"; group = c(group,'young')}
  base = base %>% 
    .[, (gpaste(d_vars,"_", inner,'_quartile', outer)) := lapply(d_vars, function(x) as.factor(ntile(get(x), 4))), by = group] %>% 
    .[, (gpaste(d_vars,'_',inner,'_pct_rank',outer)) := lapply(d_vars, function(x) percent_rank(get(x))), by = group] %>%
    .[, (gpaste(d_vars,'_',inner,'_sd_from_mean',outer)) := lapply(d_vars, function(x)(get(x)- NA_mean(get(x)))/ NA_sd(get(x))), by = group]
}}

write_parquet(base,file.path(inputs_dir, '16c_firm_yr_lvl.parquet'))
rm(list= setdiff(ls(), base_env)); gc()



# update firm_yr_ctry_lvl -------------------------------------------------
d_vars = c("comp_data", "share_comp_data")
divisions_list = list(list('ctry', c('ctry', 'year')), list('ctry_nace', c('ctry', 'NACE_BR', 'year')))
firm_yr_vars = c('firmid', 'year','share_comp_data', gpaste(c("comp_data", "share_comp_data"),"_", c('nace', 'nace_exporter'),"_",
                gpaste(c('pct_rank','sd_from_mean'),c('', "_age")))) 


firm_yr = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'), col_select = firm_yr_vars) %>% remove_if_NA(names(.)[3])
base = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')) %>%
      .[,(c(con_fil(., 'quartile'), setdiff(firm_yr_vars, c('firmid', 'year')))) := NULL] %>% 
      merge(firm_yr, by = c('firmid','year'), all.x = T)

for (i in length(divisions_list)){inner = divisions_list[[i]][[1]]; outer = ''; group = divisions_list[[i]][[2]]
  for (j in 1:2){if (j ==2){outer = "_age"; group = c(group,'young')}
    base = base %>% 
      .[, (gpaste(d_vars,'_',inner,'_pct_rank',outer)) := lapply(d_vars, function(x) percent_rank(get(x))), by = group] %>%
      .[, (gpaste(d_vars,'_',inner,'_sd_from_mean',outer)) := lapply(d_vars, function(x)(get(x)- NA_mean(get(x)))/ NA_sd(get(x))), by = group]
  }
}

write_parquet(base,file.path(inputs_dir,'16d_firm_ctry_yr_lvl.parquet'))
rm(list= setdiff(ls(), base_env)); gc()


# update firm_yr variance dataset -----------------------------------------------------------------
vars_to_mean = gpaste(c('comp_data', 'share_comp_data'),"_", c('nace', 'nace_exporter'), "_", gpaste(c('pct_rank', 'sd_from_mean'), c('', "_age")))

new_meaned = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% 
  remove_if_NA(., 'year', 'comp_data') %>% .[!is.na(dom_turnover) | !is.na(total_export_rev_customs)] %>% 
  .[year %in% year_range] %>% distinct(firmid, year, .keep_all = T) %>% 
  .[,c(setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean)), by = firmid]

base = import_file(file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet')) %>% .[,(vars_to_mean) := NULL] %>% 
      merge(new_meaned, by = 'firmid') 
write_parquet(base, file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet'))
   

