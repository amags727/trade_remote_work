# firm_birth_data =  file.path(inputs_dir,'16a_firm_lvl_birth_data.parquet')
# firm_ctry_birth_data =  file.path(inputs_dir,'16b_firm_ctry_lvl_birth_data.parquet')
# firm_yr_data = file.path(inputs_dir,'16c_firm_yr_lvl.parquet')
# firm_ctry_data = file.path(inputs_dir,'16d_firm_ctry_yr_lvl.parquet')
# ctry_entrance_data = file.path(inputs_dir,'16e_ctry_entrance.parquet')
# firm_variance_data = file.path(inputs_dir,'16f_firm_lvl_collapsed_variance.parquet')
# firm_country_variance_data = file.path(inputs_dir,'16g_firm_ctry_lvl_collapsed_variance.parquet')



# 1) make balance test  ---------------------------------------------------
key_var =  "quartile_comp_data"  
firm_yr_data = import_file(file.path(inputs_dir,'16c_firm_yr_lvl.parquet'))
firm_yr_variables = c('turnover','intangible_fixed_assets', 'capital', 'labor_cost',
                      'age','has_subsid', 'is_subsid', 'is_public',
                      'comp_weighted_prestige',
                      gpaste(c("", 'share_'), 'comp_', c('data', 'stem', 'rnd', 'engineer')),
                      'currently_export', 'total_export_rev_customs', "num_export_countries",
                      "avg_products_per_ctry", 'avg_streak_age' ,'inter_market_hhi',
                      gpaste('export_mkt_avg_comp_', c('now', 'l5', 'ever'))
                      )

