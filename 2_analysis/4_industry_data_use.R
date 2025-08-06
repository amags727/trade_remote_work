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
NACE_2d_info = if(!dummy_version){ 
    import_file("1) Data/0_misc_data/0a_nace_2d_industry_categories.csv")}else{
    data.table(NACE_2d = 0:5, industry_category = letters[1:6]) 
    }

## merge in the industry category info 
base_data <- import_file(firm_yr_path) %>% 
  .[, NACE_BR:=str_pad(NACE_BR, 4, side="left", pad="0")] %>% 
  .[, NACE_2d := as.integer(substr(as.character(NACE_BR), 1, 2))] %>% 
  merge(NACE_2d_info) %>% .[, `:=`(
  industry_category=factor(industry_category, levels=unique(NACE_2d_info$industry_category)),
  year=factor(year))]

## add employee bins 
bin_boundaries = c(0,50, 100, 200, 500, 1000, Inf); num_bins = length(bin_boundaries) -1
bin_labels = rep('', num_bins); for (i in 1:num_bins) bin_labels[i] = paste0(bin_boundaries[i],'-', bin_boundaries[i+1])
bin_labels[num_bins] = paste0(bin_boundaries[num_bins], '+')
base_data = base_data %>% arrange(empl) %>% mutate(base_data, 
  empl_bin = as.factor(cut(empl, breaks = bin_boundaries,labels = bin_labels, include.lowest =T, right = T)))

# setup output dir 
industry_output_dir = paste0(finished_output_dir,'4_industry_results')
suppressWarnings(dir.create(industry_output_dir, recursive = T))
# construct plots of data use x industry ---------------------------------
filter_lvl = 100;
collapsed_data = base_data[, 
  share_comp_data_cond := ifelse(comp_data>0, share_comp_data, NA)] %>%  .[, .(
  unique_firms = uniqueN(firmid_num),
  share_comp_data = NA_mean(share_comp_data),
  share_comp_data_cond = NA_mean(share_comp_data_cond),
  use_data = NA_mean(use_data)),  by = .(industry_category,empl_bin)] %>% 
  group_by(empl_bin) %>% mutate(across(con_fil(.,'data'), ~frank(-.), .names = '{col}_ord')) %>%
  ungroup() %>% as.data.table()

variations = expand(c('share_comp_data','share_comp_data_cond','use_data'), c(F,T), c(F,T), 
  names = c('var','filter', 'ord')) %>% 
  
  # assign var name based on interest var / ordinal 
  .[,var_name := case_when(grepl('use', var) ~ 'share firms using data',
                       grepl('cond', var)~ 'data share of payroll (data users)',
                       T ~ 'data share of payroll')] %>% 
  .[ord == T, `:=`(var = paste0(var,"_ord"),var_name = paste0('rank: ', var_name))] %>%
  
  # generate output path 
  .[, out_path := paste0('/4',rep(letters[1:3],each = 4), rep(1:4, 3),"_", var)] %>% 
  .[filter == T, out_path := paste0(out_path, '_filter')]

# generate all the graphs 
for (i in 1:nrow(variations)){
  for (name in names(variations)) assign(name, variations[[name]][i])
  graph_dta = collapsed_data[unique_firms > ifelse(filter, filter_lvl, 0)] 
  # gen base graph 
   graph = ggplot(graph_dta, aes(x = as.numeric(empl_bin), y = graph_dta[[var]], color = industry_category)) +
    geom_line() + theme_minimal() +
    labs(x = 'Num. Employees', y = var_name, color = 'NACE-2') +
    scale_x_continuous(breaks = seq_along(levels(graph_dta$empl_bin)), labels = levels(graph_dta$empl_bin))
  
  # if ordinal version reverse the y axis 
  if (ord) graph = graph +scale_y_reverse()
   
  # export
  ggsave(paste0(industry_output_dir, out_path, '.png'), graph, height = 4, width = 5)
}


  
