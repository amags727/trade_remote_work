# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}; gc();

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
d_input_dir = '2) code/6_matlab_simulation_work/d_deterministic_open_econ/d_output/'
#gen baseline graphs -------------------------------------------------------------------------
base_graph_inputs = import_file(d_input_dir, 'do2_base_graph.mat')
fields <- dimnames(base_graph_inputs$graph.output)[[1]]
base_graph_inputs <- setNames(lapply(fields, function(nm) base_graph_inputs$graph.output[nm,,]), fields)


profit_graph = as.data.table(base_graph_inputs$profit.path) %>% 
  rename_with(~c('Profit')) %>% .[,time := .I] %>% filter(time < 500) %>% 
  ggplot(.,aes(x = time, y =Profit)) + geom_line() + theme_minimal() +
  labs(subtitle = 'Profits\nOver Time', x = element_blank())

x_graph = as.data.table( base_graph_inputs$output.path) %>%
  rename_with(~c(paste0(1:2))) %>% .[,time := .I] %>% 
  pivot_longer(cols = -time) %>% filter(time < 500) %>% 
  ggplot(.,aes(x = time, y = value, color = name)) +geom_line() + 
  theme_minimal() + labs(subtitle = 'Firm Output\nOver Time',y = 'Market Output', x = element_blank()) +
  theme(legend.position = 'none')

Sigma_graph = as.data.table(base_graph_inputs$Sigma.path) %>% .[,-2] %>% 
  mutate(across(everything(), ~1-./max(.))) %>% 
  rename_with(~c(paste0(1:2))) %>% .[,time := .I] %>% .[time < 500] %>% 
  pivot_longer(cols = -time) %>% 
  ggplot(.,aes(x = time, y = value, color = name)) +geom_line() + 
  labs(subtitle = 'Technique Certainty\nOver Time',y ='Technique Certainty', x = element_blank(), color = 'Market' ) +
  scale_y_continuous(labels = percent_format()) + theme_minimal()

ggsave(paste0(de_dummy( finished_output_dir), '4a_baseline_results.png'),profit_graph + x_graph + Sigma_graph )


# gen phi comparison ------------------------------------------------------
phi_comp_inputs = import_file(d_input_dir, 'do3_phi_d_variations.mat') %>% 
  .[['phi.d.variations']] %>% as.data.frame() %>% 
  rename_with(~c('phi_d', 'time', '1', '2')) %>% pivot_longer(cols = 3:4) %>% 
  rename(market = name, output = value)

phi_comp = lapply(c(.9,1,1.1), function(phi_level){
  output =  ggplot(phi_comp_inputs %>% filter(phi_d == phi_level, time < 500),
    aes(x = time, y = output, color = market)) + geom_line() + theme_minimal() +
    labs(subtitle = TeX(paste0("$\\phi_d = ", phi_level, "$")), x = element_blank())
  
  if (phi_level != 1.1) {output = output + theme(legend.position = "none")}
  if (phi_level != .9){output = output + labs(y = element_blank())}
  return(output)})

ggsave(paste0(de_dummy(finished_output_dir), '4b_phi_comp.png'), phi_comp[[1]] + phi_comp[[2]] + phi_comp[[3]])

  




# h -----------------------------------------------------------------------
lambda_comp_inputs = import_file(d_input_dir, 'do4_lambda_variations.mat') %>% 
  .[['lambda.variations']] %>% as.data.frame() %>% 
  rename_with(~c('lambda', 'time', '1', '2')) %>% pivot_longer(cols = 3:4) %>% 
  rename(market = name, output = value)

lambda_comp = lapply(c(.45,.5,.55), function(lambda_level){
  output =  ggplot(lambda_comp_inputs %>% filter(lambda == lambda_level, time < 500),
                   aes(x = time, y = output, color = market)) + geom_line() + theme_minimal() +
    labs(subtitle = TeX(paste0("$\\lambda = ", lambda_level, "$")), x = element_blank())
  
  if (lambda_level != .55) {output = output + theme(legend.position = "none")}
  if (lambda_level != .45){output = output + labs(y = element_blank())}
  return(output)})
paste0(finished_output_dir, '4c_lambda_comp.png')
ggsave(paste0(de_dummy( finished_output_dir), '4c_lambda_comp.png'),
       lambda_comp[[1]] + lambda_comp[[2]] + lambda_comp[[3]])



