# setup -------------------------------------------------------------------
rm(list = ls());

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work",
    
    ## update as makes sense for CASD / your own use 
    T ~ "idk ")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')

## import base data 
base = import_file(file.path(inputs_dir, '16d_firm_ctry_yr_lvl.parquet')) %>% remove_if_NA('comp_data','age')

# run regressions -----------------------------------------------------------------------
## Setup mkt revenue regressions 
ind_vars = c('log_comp_data', 'comp_data_nace_pct_rank', 'comp_data_nace_sd_from_mean')
controls =  "+ log_age + log_dom_turnover + log_comp_rnd + comp_weighted_prestige"
interactions = list(c('log_other_market_rev', 'log_dom_turnover','log_num_markets','log_comp_now','first_time_in_ctry',  'first_time_exporting'),
                   c(gpaste('mkt_', c('entrance', 'exit', 'failure', 'churn'), "_rate"),
                    gpaste('mkt_de_trended_log_variance_', c('ind', 'group'), "_lvl")),
                    c('log_distance_to_france', 'log_mkt_size_rev', 'mkt_share_active_exporters',
                      gpaste('grav_',c('region', 'language', 'border')))
                    )
interactions = lapply(interactions, function(x) append('', gpaste('*',x)))

variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j'))%>% .[,counter := 1:nrow(.)]
variations_mkt_rev = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(dep_var = 'log_export_rev_customs',
           ind_var = ind_vars[j],
           block = paste0('4',letters[i],".", j), 
           command = reg_command(
             dataset = 'base',
             dep_var = dep_var,
             ind_var = ind_var,
             controls = paste0(interaction, controls),
             fe = "|NACE_BR + year + ctry",cluster = 'firmid'))}))

## Setup mkt time to exit regressions 
controls = gsub("\\+ log_age", '', controls)
interactions[[1]] = append(interactions[[1]], "*log_export_rev_customs", after = 1)
variations = expand(1:length(interactions), 1:length(ind_vars), names = c('i','j')) %>% .[,`:=`(counter = 1:nrow(.), block_letter = letters[i +max(variations$i)])] 
variations_mkt_exit = rbindlist(lapply(variations$counter, function(index){for (name in names(variations)) assign(name, variations[[name]][index])
  data.frame(interaction = interactions[[i]]) %>%
    mutate(dep_var = 'last_year_of_streak',
           ind_var = ind_vars[j],
           block = paste0('4',letters[i],".", j), 
           command = reg_command(
             dataset = 'base',
             dep_var = dep_var,
             ind_var = ind_var, 
             controls = paste0(interaction, controls),
             fe =  'NACE_BR, ctry, year',
             family = 'cox', 
             cluster = 'firmid',
             time_var = 'streak_age'))}))


variation_output = rbind(variations_mkt_rev,variations_mkt_exit)
if(running_regressions){write_rds(evaluate_variations(variation_output, full_df = F), paste0(raw_output_dir,'block_4.rds'))}

# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()

