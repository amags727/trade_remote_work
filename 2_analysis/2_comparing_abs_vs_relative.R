# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
root = case_when(
  ## AZM running locally and not testing if it will work CASD 
  grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work",
  
  ## update as makes sense for CASD / your own use 
  T ~ "idk ")
}

source('2) code/0_set_parameter_values.R')
# 2a firm_yr level analsysis --------------------------------------------------------------
base = import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet')) %>% select(- con_fil(., 'quartile'))

## construct dom rev variations 
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean',
             'share_comp_data', 'share_comp_data_nace_pct_rank', 'share_comp_data_nace_sd_from_mean')
control_vars = paste(" ", 'log_comp_rnd', 'log_age', 'comp_weighted_prestige', sep = "+")
variations_dom_rev = data.frame(ind_var = ind_vars) %>% mutate(block = '2a.i_dom_rev', command = reg_command(
  dep_var = 'log_dom_turnover', 
  ind_var = ind_var,
  dataset = 'base',
  controls = control_vars, 
  fe = "|NACE_BR + year",
  cluster = 'firmid'))
                                                               
## construct total export rev variations 
ind_vars =  c(ind_vars, gsub('nace','nace_exporter', ind_vars))
control_vars = paste0('+ log_dom_turnover', control_vars)
variations_total_export_rev = data.frame(ind_var = ind_vars) %>% mutate(command = reg_command(
  dep_var = 'log_total_export_rev_customs', 
  ind_var = ind_var,
  dataset = 'base',
  controls = control_vars, 
  fe = "|NACE_BR + year",
  cluster = 'firmid'))
variations_total_export_rev$block = rep(c('2a.ii.1_total_export_rev','2a.ii.2_total_export_rev'),each = 6)

## construct likelihood of entry into exporting
ind_vars = ind_vars[1:6]
control_vars = gsub("\\+log_age", '', control_vars)
variations_export_entrance = data.frame(ind_var = ind_vars) %>% mutate(block = '2a.iii_export_entrance', command = reg_command(
  dataset = 'base[year <= first_export_year]',
  dep_var = 'is_first_export_year',
  ind_var = ind_var,
  time_var = 'age',
  controls = control_vars,
  fe = 'NACE_BR, year',
  cluster = 'firmid',
  family = 'cox'))
 

## combine and run 
variation_output = rbind(variations_dom_rev,variations_total_export_rev,variations_export_entrance)
if(running_regressions){write_rds(evaluate_variations(variation_output, full_df = F), paste0(raw_output_dir,'block_2a_firm_yr.rds'))}

## cleanup 
rm(list= setdiff(ls(), base_env)); gc()

# 2b firm-ctry analysis  -----------------------------------------------------
## generate the ctry level revenue 
base = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet'))
controls =  "+ log_age + log_dom_turnover + log_comp_rnd + comp_weighted_prestige"
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean',
             'share_comp_data', 'share_comp_data_nace_pct_rank', 'share_comp_data_nace_sd_from_mean') %>% c(., 
    gsub('nace', 'nace_exporter',.), gsub('nace', 'ctry',.), gsub('nace', 'ctry_nace',.))

variations_ctry_rev = data.frame(ind_var = ind_vars, block =  rep(gpaste('2b.i.',1:4,'_ctry_lvl_rev'),each = 6)) %>% mutate(command = reg_command(
  dataset = 'base',
  dep_var = 'log_export_rev_customs',
  ind_var = ind_var,
  controls = controls,
  fe = '|NACE_BR + ctry + year',
  cluster = 'firmid'
))


## generate the ctry exit regressions 
controls = gsub("\\+log_age", '', controls)
variations_ctry_exit = data.frame(ind_var = ind_vars, block =  rep(gpaste('2b.ii.',1:4,'_ctry_exit'),each = 6)) %>% mutate(command = reg_command(
  dataset = 'base',
  dep_var = 'last_year_of_streak',
  ind_var = ind_var,
  controls = controls,
  fe = 'NACE_BR, ctry, year',
  cluster = 'firmid',
  family = 'cox',
  time_var = 'streak_age'
))

variation_output = rbind(variations_ctry_rev,variations_ctry_exit)
if(running_regressions){write_rds(evaluate_variations(variation_output, full_df = F), paste0(raw_output_dir,'block_2b_firm_ctry_yr.rds'))}

## cleanup 
rm(list= setdiff(ls(), base_env)); gc()

# 2c total rev variance ----------------------------------------------------------------------

#set parameter values 
base = import_file(file.path(inputs_dir, '16f_firm_lvl_collapsed_variance.parquet'))
dom_ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean', 'share_comp_data', 'share_comp_data_nace_pct_rank', 'share_comp_data_nace_sd_from_mean')
dom_controls = '+log_dom_turnover + log_comp_rnd + comp_weighted_prestige +log_min_age_dom_rev_observed + log_years_dom_rev_observed'
dom_fe = "| NACE_BR  + min_first_year_dom_rev_observed"

export_ind_vars =  c(dom_ind_vars, gsub('nace','nace_exporter', dom_ind_vars))
export_controls = gsub('dom_rev', 'exports', paste0("+log_total_export_rev_customs", dom_controls))
export_fe = gsub('dom_rev', 'exports',dom_fe)


## construct dom rev variations 
variations_dom_variance = data.frame(ind_var = dom_ind_vars) %>% mutate(block = '2c.i_dom_rev_variance', command = reg_command(
  dep_var = 'detrended_var_log_dom_turnover', 
  ind_var = ind_var,
  dataset = 'base',
  controls = dom_controls, 
  fe = dom_fe,
  cluster = "NACE_BR"))

## construct total export rev variations 
variations_export_variance = data.frame(ind_var = export_ind_vars, block =  rep(gpaste('2c.ii',1:2,'_total_export_rev_variance'),each = 6)) %>%
  mutate(command = reg_command(
  dep_var = 'detrended_var_log_total_export_rev_customs', 
  ind_var = ind_var,
  dataset = 'base',
  controls = export_controls, 
  fe = export_fe,
  cluster =  "NACE_BR"))


variation_output = rbind(variations_dom_variance,variations_export_variance)
if(running_regressions){write_rds(evaluate_variations(variation_output, full_df = F), paste0(raw_output_dir,'block_2c_firm_variance.rds'))}
## cleanup 
rm(list= setdiff(ls(), base_env)); gc()

# 2d streak level variance  -----------------------------------------------
base = import_file(file.path(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet')) 

#set parameters 
controls =  "+ log_export_rev_customs + log_dom_turnover + log_min_age + log_comp_rnd + comp_weighted_prestige + log_years_observed"
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean',
             'share_comp_data', 'share_comp_data_nace_pct_rank', 'share_comp_data_nace_sd_from_mean') %>% c(., 
            gsub('nace', 'nace_exporter',.), gsub('nace', 'ctry',.), gsub('nace', 'ctry_nace',.))

variations = data.frame(ind_var = ind_vars, block =  rep(gpaste('2d.i.',1:4,'_ctry_var'),each = 6)) %>% mutate(command = reg_command(
  dataset = 'base',
  dep_var = 'detrended_var_log_export_rev_customs',
  ind_var = ind_var,
  controls = controls,
  fe = '| NACE_BR + min_year + ctry',
  cluster = 'firmid'
))

if(running_regressions){write_rds(evaluate_variations(variations, full_df = F), paste0(raw_output_dir,'block_2d_firm_ctry_variance.rds'))}
## cleanup 
rm(list= setdiff(ls(), base_env)); gc()



# 2e entrance analysis  ---------------------------------------------------
base = rbindlist(lapply(list.files('1) data/temp_data',recursive = TRUE, full.names = TRUE),import_file))
controls =  " + log_dom_turnover + log_comp_rnd + comp_weighted_prestige"
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean',
             'share_comp_data', 'share_comp_data_nace_pct_rank', 'share_comp_data_nace_sd_from_mean')


variations= data.frame(ind_var = ind_vars) %>% mutate(block = '2e_ctry_entrance', command = reg_command(
  dataset = 'base',
  dep_var = 'entered_market',
  ind_var = ind_var,
  controls = controls, 
  fe =  'NACE_BR, ctry, year',
  cluster = 'firmid',
  family = 'cox', 
  time_var = 'age'))


if(running_regressions){write_rds(evaluate_variations(variations, full_df = F), paste0(raw_output_dir,'block_2e_country_entry.rds'))}

## cleanup 
rm(list= setdiff(ls(), base_env)); gc()

