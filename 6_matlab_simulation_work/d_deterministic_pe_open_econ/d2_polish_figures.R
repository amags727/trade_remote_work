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

# 1_base_ss_progression -------------------------------------------------------------------------
base_graph_inputs = import_file(d_input_dir, '1_base_ss_progression.csv') %>% 
  select(-c('pi_tot','Cert_12')) %>%  pivot_longer(cols = -t) %>% as.data.table() %>%
  .[,market := ifelse(grepl('1', name), '1','2')] %>% .[value != 0]


profit_graph = ggplot(base_graph_inputs[grepl('pi_', name)], aes(x = t, y = value, color = market)) + 
  geom_line() + theme_minimal() + 
  labs(subtitle = 'Flow Profits', x = element_blank(), y = element_blank()) 
  
x_graph = ggplot(base_graph_inputs[grepl('x_', name)], aes(x = t, y = value, color = market)) + 
  geom_line() + theme_minimal() + 
  labs(subtitle = 'Quality Adjusted Output', x = element_blank(), y = element_blank()) + theme(legend.position = 'none')

certainty_graph = ggplot(base_graph_inputs[grepl('Cert', name)], aes(x = t, y = value, color = market)) + 
  geom_line() + theme_minimal() + 
  labs(subtitle = 'Technique Certainty', x = element_blank(), y = element_blank()) + theme(legend.position = 'none') +
  scale_y_continuous(labels = percent_format())

composite_graph = certainty_graph + x_graph + profit_graph + plot_annotation(title = 'Firm Progression to Steady State')
ggsave(paste0(d_output_dir, '1_baseline_ss_progression.png'),composite_graph, width = 10.6, height = 5.22 )


# 2_phi_g_x_data ------------------------------------------------------
phi_g_graph = import_file(d_input_dir, '2_phi_g_x_data.csv')  %>%
  pivot_longer(cols = - phi_g, names_to = 'market') %>% mutate(market = gsub('l_', '', market)) %>% 
  ggplot(., aes(x = phi_g, y = value, color = market)) + geom_line() + theme_minimal()  + 
  labs(x = expression(phi[g]), y= 'data spend', title = 'Steady State Data Spend as a Function\nof Manufacuturing Productivity') 
ggsave(paste0(d_output_dir, '2_phi_g_x_data.png'), phi_g_graph,width = 8.71, height = 6.19)
# 3_phi_d_x_pe_growth  -----------------------------------------------------------------------
phi_d_inputs = import_file(d_input_dir, '3_phi_d_x_pe_growth.csv') %>%
  pivot_longer(cols = -c(phi_d,t), names_to = 'Market') %>%
  mutate(Market = gsub('x', '',Market)) %>%
  as.data.table() %>% .[value != 0]
phi_d_vec = unique(phi_d_inputs$phi_d); max_t = max(phi_d_inputs$t); 


phi_d_pe_graph = lapply(phi_d_vec, function(c_phi_d){
graph = ggplot(phi_d_inputs[phi_d == c_phi_d], aes(x = t, y= value, color = Market)) +
  geom_line() + theme_minimal() + labs(subtitle = bquote(phi[d] == .(c_phi_d)), y = element_blank(), x = element_blank()) +
  theme(legend.position = 'none') + scale_x_continuous(limits = c(0,max(phi_d_inputs$t))) + 
  scale_y_continuous(limits = c(min(phi_d_inputs$value), 1.1*max(phi_d_inputs$value)))
if (c_phi_d == min(phi_d_vec)) graph = graph+ labs(y = 'Quality Adjusted Output')
if (c_phi_d == max(phi_d_vec)) graph = graph+ theme(legend.position = 'right')
return(graph)
})
phi_d_pe_graph = phi_d_pe_graph[[1]] + phi_d_pe_graph[[2]] + phi_d_pe_graph[[3]] + 
  plot_annotation(caption = "Time", title = 'Market Entry as a Function of Data Scraping Productivity') & 
  theme(plot.caption = element_text(hjust = 0.5, size = 11),
        plot.title = element_text(size = 12))
ggsave(paste0(d_output_dir, '3_phi_d_x_pe_growth.png'), phi_d_pe_graph, height = 5.17, width = 8.14)


# 4_lambda_x_pe_growth -------------------------------------------------------
lambda_inputs = import_file(d_input_dir, '4_lambda_x_pe_growth.csv') %>%
  pivot_longer(cols = -c(lambda,t), names_to = 'Market') %>%
  mutate(Market = gsub('x', '',Market)) %>%
  as.data.table() %>% .[value != 0]
lambda_vec = unique(lambda_inputs$lambda); max_t = max(lambda_inputs$t); 


lambda_pe_graph = lapply(lambda_vec, function(c_lambda){
  graph = ggplot(lambda_inputs[lambda == c_lambda], aes(x = t, y= value, color = Market)) +
    geom_line() + theme_minimal() + labs(subtitle = bquote(lambda == .(c_lambda)), y = element_blank(), x = element_blank()) +
    theme(legend.position = 'none') + scale_x_continuous(limits = c(0,max(lambda_inputs$t))) + 
    scale_y_continuous(limits = c(min(lambda_inputs$value), 1.1*max(lambda_inputs$value)))
  if (c_lambda == min(lambda_vec)) graph = graph+ labs(y = 'Quality Adjusted Output')
  if (c_lambda == max(lambda_vec)) graph = graph+ theme(legend.position = 'right')
  return(graph)
})

lambda_pe_graph = lambda_pe_graph[[1]] + lambda_pe_graph[[2]] + lambda_pe_graph[[3]] + 
  plot_annotation(caption = "Time", title = 'Entry as a Function of Market Correlation') & 
  theme(plot.caption = element_text(hjust = 0.5, size = 11),
        plot.title = element_text(size = 12))
ggsave(paste0(d_output_dir, '4_lambda_x_pe_growth.png'), lambda_pe_graph, height = 5.17, width = 8.14)





# 5_symmetric_concentration_analysis ------------------------------------------------
entry_graph_input = import_file(d_input_dir, '5b_concentration_anal_graph_results.csv') %>% .[,phi_d := as.factor(round(phi_d,2))] 
entry_graph = lapply(1:2, function(i){
graph = entry_graph_input %>% .[!get(paste0('x_',i))== 0] %>%  ggplot( aes(x = t, y= .[[paste0('x_',i)]], color = phi_d)) + geom_line() + 
  scale_color_brewer(palette = "RdYlGn") + theme_minimal() +
  labs(y = element_blank(), 
       color = bquote(phi[d]),
       x = "Time",
       subtitle = paste0('Entrant Growth (mkt ', i,')')) + 
  theme(axis.title.x = element_text(size = 10))

if( i == 1) graph = graph + labs(y = 'Quality Adjusted Output') + theme(legend.position = 'none')
return(graph)
})

num_firms_graph = import_file(d_input_dir, '5a_concentration_anal_summ_stats.csv')  %>% 
  ggplot(aes(x = phi_d, y= num_firms)) + geom_line() + theme_minimal() + 
  labs(subtitle = 'Number of Firms', y = element_blank(), x = bquote(phi[d])) + 
  theme(plot.margin = margin(r = 30))

ge_graph = num_firms_graph  + entry_graph[[1]] + entry_graph[[2]] +  plot_annotation(
  title = "GE Characteristics as a Function of Aggregate Scraping Productivity") 

ggsave(paste0(d_output_dir, '5_symmetric_concentration_analysis.png'), ge_graph, height = 6.19, width = 12.1)









