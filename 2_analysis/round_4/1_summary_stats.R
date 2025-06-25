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
# 1a generate summary stats  -------------------------------------------------------------------------
firm_yr_w_unmatched = import_file('11_firm_yr_summary_stats_input.parquet')
int_vars = c('comp_data', 'comp_total', 'share_empl_college', 'avg_prestige_total',
             'dom_turnover','currently_export_BS' ,'total_export_rev_BS_cond', 'capital', 'intangible_fixed_assets','age',
             'currently_export_customs', 'total_export_rev_customs_cond', 'num_mkts_cond','products_per_ctry_cond')

coef_names = c('payroll data', 'payroll total', 'share empl. college grad', 'avg empl. prestige', 'dom. revenue',
               'currently exporting', 'export revenue*', 'capital', 'intagible fixed assets', 'firm age',
               'currently exporting', 'export revenue*', 'num. markets*', 'products per market*')
balance_sheet_input = reshape_to_summary(firm_yr_w_unmatched, int_vars,'nace_comp_data_quartile')
balance_sheet_input[,c(1,7,2:6)]
label = '1a_balance_tests'
format_table(summary_table_input = balance_sheet_input[,c(1,7,2:6)],
             label = label,
             headers = "&&&&\\multicolumn{4}{c}{\\textbf{Data Spend Quartile}}\\\\",
             coef_names = coef_names, 
             divisions_before = 3,
             custom_rows = list('\\textbf{Linkedin Data}','\\textbf{Balance Sheet Data}', '\\textbf{Customs Data}'),
             custom_row_placement = c(8, 17,30),
             spacer_size = 1,
             notes = '* value is contingent upon exporting. Quartiles at the industry year-level',
             note_width = 1,
             rescale_factor =  1,
             output_path = paste0(finished_output_dir, label, '.tex'), make_tex = F)
rm(list= setdiff(ls(), c(base_env))); gc()


# 1b Industry descriptives -------------------------------------------------------------------------
NACE_2d_info<- import_file("1) Data/0_misc_data/0a_nace_2d_industry_categories.csv")
industry_descriptives = lapply(list('NACE_2d', c('NACE_2d', 'year')), function(group_vars){
industry_summary = import_file(firm_yr_path) %>% 
  .[, NACE_2d := as.integer(substr(as.character(str_pad(NACE_BR, 4, side="left", pad="0")), 1, 2))] %>% 
  .[, .(share_exporter_BS = NA_mean(currently_export_BS),
        share_exporter_customs = NA_mean(currently_export_customs),
        use_data = NA_mean(use_data), 
        share_comp_data = NA_mean(share_comp_data), 
        use_data_exporter_BS = NA_mean(use_data[currently_export_BS]),
        share_comp_data_exporter_BS = NA_mean(share_comp_data[currently_export_BS]), 
        use_data_exporter_customs = NA_mean(use_data[currently_export_customs]),
        share_comp_data_exporter_customs = NA_mean(share_comp_data[currently_export_customs]),
        count = .N,
        count_exporter_BS = NA_sum(currently_export_BS),
        count_exporter_customs = NA_sum(currently_export_customs)),  by = group_vars] %>% 
  .[, diff_between_export_shares := share_exporter_BS - share_exporter_customs] %>% 
  .[count >= 5] %>% 
  merge(NACE_2d_info, all.x = T)})
fwrite(industry_descriptives[[1]], paste0(raw_output_dir, '1b1_industry_summary_stats.csv'))
fwrite(industry_descriptives[[2]], paste0(raw_output_dir, '1b2_industry_summary_stats_yr.csv'))



# 1c intensive vs. extensive margin changes  ------------------------------
linkedin_data = import_file(linkedin_firm_yr_path)
changes = linkedin_data[empl_data_delta != 0] %>% 
  .[,`:=`(extensive_margin = empl_data == 0 | empl_data_lag1 == 0,
          share_comp_data_delta = share_comp_data_delta*100)]

change_graphs = lapply(c('empl_data_delta', 'share_comp_data_delta'), function(var){
  value_1 = round(quantile(changes[[var]], probs = 0.01, na.rm = TRUE)); 
  value_99 = round(quantile(changes[[var]], probs = 0.99, na.rm = TRUE))
  graph_input= changes[, delta := case_when(
    get(var) < value_1 ~ value_1,
    get(var) > 0 & get(var) <1 ~1,
    get(var) < 0 &  get(var) >-1 ~-1,
    get(var) > value_99 ~ value_99,
    T ~ round(get(var)))] %>% 
    .[,.(count = .N), by = .(extensive_margin, delta)] %>% 
    .[, count := count / NA_sum(count)]
  if(grepl('share',var)) graph_input[,delta := delta*1e-2]

  graph = ggplot(graph_input, aes(x = delta, y = count, fill = extensive_margin)) + geom_col(position = 'stack') + 
    theme_minimal() +  scale_y_continuous(labels = percent) +  labs(x = element_blank(), y = element_blank()) 
  
  if (grepl('share',var)){
    graph = graph + scale_x_continuous(labels = percent) + labs(
      subtitle = '% Change in Share of Payroll to Data',
      fill = 'Extensive\nMargin')
  }else{
    graph = graph + labs(subtitle = "Change in Data Employees") + theme(legend.position = 'none')
  }
  return(graph)
})
change_graphs[[1]] + change_graphs[[2]]
ggsave(paste0(finished_output_dir, '1c_change_distribution.png'),
       change_graphs[[1]] + change_graphs[[2]],
       height = 4, width = 8)



# generate plots of data use  ---------------------------------------------
firm_yr = import_file(firm_yr_path) 
hi=firm_yr[, .(comp_data = NA_mean(comp_data/comp_total), firms_using_data = NA_sum(comp_data>0)/.N), by = year] %>% 
  pivot_longer(-year) %>% as.data.table() %>% .[,value_adjusted := value / value[year == 2008], by = name] %>%
  ggplot(., aes(x = year, y = value_adjusted,  color = name)) + geom_line()

firm_yr = import_file(firm_yr_path)
suffixes = c('customs', 'BS')
prefixes  = c("eventually_", 'never_','not_', 'currently_', 'not_currently_')
expansion = expand(prefixes, suffixes, names = c('prefix','suffix' )) %>% mutate(
  export_var = paste0(prefix, 'use_data_and_export_', suffix),
  dom_rev_var = paste0(prefix, 'use_data_dom_rev'),
  export_rev_var = paste0(prefix, 'total_export_rev_',suffix),
  export_rev_var_cond = paste0(prefix, 'total_export_rev_',suffix, '_cond'),
  relative_exports =  paste0(prefix, 'relative_export_rev_',suffix),
  pre_condition = paste0(prefix, 'use_data'),
  post_condition = paste0('currently_export_', suffix))
vars_to_mean = c('dom_turnover', paste0('total_export_rev_', suffixes), 
                 unique(expansion$dom_rev_var),
                 expansion$export_rev_var,
                 expansion$export_rev_var_cond,
                 expansion$relative_exports)


graph_input = firm_yr[age>20, age := NA] %>%
  .[,eventually_use_data := any(use_data), by = firmid_num] %>% 
  .[, never_use_data := ! eventually_use_data] %>% 
  .[,currently_use_data := use_data] %>% 
  .[, not_currently_use_data := eventually_use_data & !currently_use_data] %>% 
  .[,not_use_data := !use_data] %>% 
  .[year >= first_export_year_BS, years_export_BS := year - first_export_year_BS] %>% 
  .[year >= first_export_year_customs, years_export_customs := year - first_export_year_customs]  


for(i in 1:nrow(expansion)){
  graph_input[get(expansion$pre_condition[i])== T, (expansion$export_var[i]):= get(expansion$post_condition[i])]
  graph_input[get(expansion$pre_condition[i])== T, (expansion$dom_rev_var[i]):= dom_turnover]
  graph_input[get(expansion$pre_condition[i])== T, (expansion$export_rev_var[i]):= get(paste0('total_export_rev_',expansion$suffix[i]))]
  graph_input[get(expansion$pre_condition[i])== T, (expansion$export_rev_var_cond[i]):= get(paste0('total_export_rev_',expansion$suffix[i],'_cond'))]
  graph_input[get(expansion$pre_condition[i])== T & dom_turnover!=0, (expansion$relative_exports[i]):= get(paste0('total_export_rev_',expansion$suffix[i]))/dom_turnover]
}

graph_input = rbindlist(lapply(c('years_export_BS','years_export_customs', 'age'), function(age_variable){
  temp = graph_input[, c(setNames(lapply(.SD[, ..vars_to_mean], NA_median), vars_to_mean)), by = c(age_variable)] %>% pivot_longer(., cols= -1) %>% 
    rename(age = names(.)[1]) %>% as.data.table() %>%
    .[,age_variable := age_variable] %>% .[age < 20] %>% 
    .[,value_adjusted := value / value[age==0], by = name]
}))

# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()




