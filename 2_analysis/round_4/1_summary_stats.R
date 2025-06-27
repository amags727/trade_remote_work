# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))};gc()

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
firm_yr_w_unmatched = import_file(firm_yr_summary_stats_path)
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





# 1d/e data graduates/employees across france ----------------------------------------------
uni_data = import_file('1) data/7_revelio_data/c_final_outputs/7c2_nuts_3_uni_lvl_output.parquet') %>%
  group_by(NUTS_ID) %>% summarize(across(con_fil(.,'grad'), ~NA_mean(.))) %>% 
  mutate(log_data_grads = asinh(data_grads)) %>% as.data.table()
  
firm_data = import_file(linkedin_firm_yr_region_path) %>% 
  .[,.(empl_data = NA_sum(empl_data)), by = .(NUTS_ID, year)] %>% 
  .[,.(empl_data = NA_mean(empl_data)), by = .(NUTS_ID)] %>% 
  .[, log_empl_data := asinh(empl_data)]

geo_data = gisco_nuts %>% filter(LEVL_CODE == 3,
  CNTR_CODE == 'FR', !grepl('FRY',NUTS_ID)) %>%
  st_transform(2154)
  
cities_sf <- data.frame(
  name = c("Paris", "Marseille", "Lyon", "Toulouse", "Nice",
           "Nantes", "Montpellier", "Strasbourg", "Bordeaux", "Lille"),
  lon = c( 2.3522, 5.3698, 4.8357, 1.4442, 7.2620,
           -1.5536, 3.8777, 7.7521, -0.5792, 3.0573),
  lat = c(48.8566, 43.2965, 45.7640, 43.6047, 43.7102,
          47.2184, 43.6119, 48.5734, 44.8378, 50.6292)) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(2154)

graphs = lapply(1:2, function(num){
if (num == 1){
  input = uni_data[,var := log_data_grads];
  break_vec = c(0,4, 40, 400, 4000);
  legend_name = 'Data\nGrads'}else{
    input = firm_data[,var := log_empl_data]
    break_vec = c(0,20, 200, 2000, 20000)
    legend_name = "Data\nJobs"
  }

avg_graph  = merge(geo_data, input, by = 'NUTS_ID', all.x = T)  %>% 
  mutate(var = replace_na(var, 0)) %>%
  ggplot(.) + geom_sf(aes(fill = var), lwd = 0) +
  scale_fill_gradientn(
    colours = c("#D73027", "#FC8D59", "#FEE08B", "#91BFDB", "#4575B4"),
    values = scales::rescale(asinh(break_vec)),  
    limits = asinh(c(min(break_vec), max(break_vec))),
    breaks = asinh(break_vec),
    labels = break_vec,
    name = legend_name) +
  geom_sf_text(data = cities_sf, aes(label = name), size = 3, nudge_y = 20000) + 
  theme_void() 
})

ggsave(paste0(gsub('1a) dummy data/99_fake_output/', '3) output/',finished_output_dir), '1d_data_graduates_map.png'),
       graphs[[1]], width = 7, height = 7 )

ggsave(paste0(finished_output_dir, '1e_data_employment_map.png'),
       graphs[[2]], width = 7, height = 7 )

# clean up  ---------------------------------------------------------------
rm(list= setdiff(ls(), base_env)); gc()




