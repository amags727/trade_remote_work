# setup -------------------------------------------------------------------
rm(list = ls()); gc();

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

## import helper functions 
source('2) code/0_set_parameter_values.R')
# import data and set up for analysis -----------------------------------
base_data <- import_file(firm_yr_path) %>% arrange(empl) %>% mutate(empl_bin= factor(empl_bin, levels = unique(empl_bin)))
casd_n_obs<-4

# construct collapsed data to use in the plots ---------------------------------
if (!dummy_version){
collapsed_data = base_data[, 
                           share_comp_data_cond := ifelse(comp_data>0, share_comp_data, NA)] %>%  .[, .(
                             unique_firms = uniqueN(firmid_num),
                             share_comp_data = NA_mean(share_comp_data),
                             share_comp_data_cond = NA_mean(share_comp_data_cond),
                             size_adjusted_comp_data = NA_mean(size_adjusted_comp_data),
                             use_data = NA_mean(use_data)),  by = .(Industry_Category,empl_bin)] %>% 
  group_by(empl_bin) %>% mutate(across(con_fil(.,'data'), ~frank(-.), .names = '{col}_ord')) %>%
  ungroup() %>% as.data.table()
fwrite(collapsed_data[unique_firms>casd_n_obs], paste0(raw_output_dir, '4_industry_collapsed_data.csv'))
}

# generate plots -------------------------------------------------------------------------
filter_lvl = 25; industry_cutoff = 5
collapsed_data = import_file(de_dummy(raw_output_dir), '4_industry_collapsed_data.csv') %>% 
  .[unique_firms > filter_lvl] %>%
  mutate(empl_bin = factor(empl_bin, levels = unique(empl_bin)), empl_bin_num = as.numeric(empl_bin))

top_industries = collapsed_data[,.(max_var = NA_max(size_adjusted_comp_data)), by = Industry_Category] %>%
  arrange(-max_var) %>% pull(Industry_Category) %>% .[1:industry_cutoff]
variations = fread("var, subtitle_
                   use_data, % Firms Using Data
                   size_adjusted_comp_data, Size Adjusted Data Compensation")
graph = lapply(1:nrow(variations),function(i){
  for (name in names(variations)) assign(name, variations[[name]][i])
  graph_dta = rbindlist(list(
    collapsed_data[Industry_Category %in% top_industries],
    collapsed_data[!Industry_Category %in% top_industries] %>%
      .[, setNames(list(NA_median(get(var))), var), by = .(empl_bin, empl_bin_num)] %>% 
      .[,Industry_Category := 'All Other NACE\nCodes']), use.names =T , fill = T)
  
  graph = graph_dta %>% ggplot(aes(x = empl_bin_num, y = .[[var]], color = Industry_Category)) +
    geom_line() + theme_minimal() +
    scale_color_discrete(breaks = c(top_industries, 'All Other NACE\nCodes')) + 
    scale_x_continuous( breaks = seq_along(levels(graph_dta$empl_bin)), labels = levels(graph_dta$empl_bin)) +
    labs(x = 'Employee Count', color = 'NACE-2 Code', y = element_blank(), subtitle = subtitle_) 
  
  if (var == 'use_data') graph = graph + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    theme(legend.position = 'none')
  return(graph)
})
graph =  graph[[1]] + graph[[2]] + plot_annotation(title = 'Data Use Patterns Across Top 5 Industries')
ggsave(paste0(de_dummy(finished_output_dir), '4_industry_descriptives.png'),graph, width = 11.5, height = 5.2)

