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
d_input_dir = '3) output/simulation_output/raw/'
d_output_dir = '3) output/simulation_output/clean/'

#gen baseline graphs -------------------------------------------------------------------------
colnames = c('network', gpaste(c('Sigma', 'certainty_lvl'),"_",1:2), 'total_pi', gpaste(c('pi', 'x', 'l', 'R'), "_", 1:2))
base_graph_inputs = import_file(d_input_dir, '1_base_ss_progression.mat')[['graph.output']] %>% 
  as.data.table() %>% rename_with(~c(colnames)) %>% mutate(t = 1:nrow(.)) %>% 
  pivot_longer(cols = -t) %>% as.data.table() %>% mutate(market = ifelse(grepl('1', name), '1','2'))


profit_graph = ggplot(base_graph_inputs[grepl('pi_', name)], aes(x = t, y = value, color = market)) + 
  geom_line() + theme_minimal() + 
  labs(subtitle = 'Flow Profits', x = element_blank(), element_blank()) 
  
x_graph = ggplot(base_graph_inputs[grepl('x_', name)], aes(x = t, y = value, color = market)) + 
  geom_line() + theme_minimal() + 
  labs(subtitle = 'Quality Adjusted Output', x = element_blank(), y = element_blank()) + theme(legend.position = 'none')

certainty_graph = ggplot(base_graph_inputs[grepl('certainty_lvl', name)], aes(x = t, y = value, color = market)) + 
  geom_line() + theme_minimal() + 
  labs(subtitle = 'Technique Certainty', x = element_blank(), y = element_blank()) + theme(legend.position = 'none') +
  scale_y_continuous(labels = percent_format())

composite_graph = certainty_graph + x_graph + profit_graph + plot_annotation(title = 'Firm Progression to Steady State')
ggsave(paste0(d_output_dir, 'baseline_ss_progression.png'),composite_graph, width = 10.6, height = 5.22 )


# gen phi comparison ------------------------------------------------------
phi_graph_inputs = import_file(d_input_dir, '2_phi_g_x_data.mat')[['results.mat']] %>% as.data.table() %>% 
  rename_with(~c('phi_g', '1', '2')) %>% pivot_longer(cols = - phi_g, names_to = 'market') %>% 
  ggplot(., aes(x = phi_g, y = value, color = market)) + geom_line() + theme_minimal()  + 
  labs(x = expression(phi[g]), y= 'data spend', title = 'Data Spend as a Function of Manufacutring Productivity')





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
