# setup -------------------------------------------------------------------
rm(list = ls());
setwd('../..')
if (!file.exists("2) code/00_helper_functions.R")){setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../../..')}
source('2) code/0_set_parameter_values.R')


# 1) add the comp abroad variables -----------------------------------------------------------------------
source('2) code/1_cleaning/3_it_data_preperation/3a_alpha_complete_linkedin_vars.R')

# 2) update the age data to account for edge case  ---------------------------
source('2) code/1_cleaning/3_it_data_preperation/3b_make_firm_age.R')



# 3) update firm-yr  ------------------------------------------------------
source('2) code/1_cleaning/3_it_data_preperation/3c_make_firm_yr_lvl.R')

# 4) update firm_ctry_yr -----------------------------------------------------------------
source('2) code/1_cleaning/3_it_data_preperation/3d_make_firm_ctry_yr_lvl.R')


# 5) update ctry entrance yr -----------------------------------------------------------------
source('2) code/1_cleaning/3_it_data_preperation/3f_make_ctry_entrance.R')

# 6) update variance data  -----------------------------------------------
source('2) code/1_cleaning/3_it_data_preperation/3e_make_firm_variance.R')

# update ctry-yr lvl variance ---------------------------------------------
linkedin_vars = c('firmid', 'year', gpaste( 'comp_', c('data',  'rnd', 'weighted_prestige')))

vars_to_any = c('first_time_in_ctry', 'first_time_exporting')

  ## NEW CODE 
  .[, c( setNames(lapply(.SD[, ..vars_to_mean], NA_mean), vars_to_mean),
         setNames(lapply(.SD[, ..vars_to_any], NA_any), vars_to_any)),
    by = .(ctry, firmid, streak_id)]
  
  

  base_data = import_file(file.path(inputs_dir, '16g_firm_ctry_lvl_collapsed_variance.parquet')) %>% select(-years_observed) %>% 
    merge(additional_vars_dta, by = c('ctry', 'firmid', 'streak_id')) %>% 
    merge(fread('1) data/similarity_matrices/outputs/france_distance_data.csv'), by = 'ctry') %>% 
  .[,young_at_start :=min_age <=5 ]


  






