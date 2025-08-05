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


firm_yr = import_file(firm_yr_path)

year_start<-2009


data_sources<-c("BS", "customs")
max_y_axis<- 4

for(ds in data_sources){

  vars_to_mean<-c(paste0("nace_avg_log_total_export_rev_", ds, "_detrended_var"))
  vars_to_sum <- c(paste0("total_export_rev_", ds),
                   "empl_total")
  
  
  variance<-firm_yr[, c(as.list(lapply(.SD[, ..vars_to_mean], mean, na.rm=T)),
                        as.list(lapply(.SD[, ..vars_to_sum], sum, na.rm=T))), 
                    by=.(NACE_BR, year)] %>% 
    unbalanced_lag("NACE_BR", "year", vars_to_sum, 1) %>%
    .[, (paste0(vars_to_sum, "_bar")) := lapply(vars_to_sum, function(v) (get(v) + get(paste0(v, "_lag1")))/2)]
  
  vars_to_trend <- vars_to_mean
  vars_weight <- paste0(vars_to_sum, "_bar") 
  time_var<-"year"
  
  variance_trend<-trend_metrics(variance, vars_to_trend, vars_weight, time_var) %>%
    melt(id.vars="year", value.name = "variance") %>% filter(year>=year_start)
  
  names_vars<-as.vector(unique(variance_trend$variable))
  labels_names<-c("Unweighted", paste0("Weighted by ", ds, " Revenue"), paste0("Weighted by Employment"))
  labels_vars<-c(names_vars[grep("unwtd", names_vars)], 
                 setdiff(names_vars, c(names_vars[grep("unwtd", names_vars)], names_vars[grep(vars_to_sum[2], names_vars)])),
                 names_vars[[grep(vars_to_sum[2], names_vars)]])
  variance_trend[, variable:=factor(variable, levels=labels_vars, labels=labels_names)]

  
  temp_plot<-ggplot(variance_trend, aes(x=year, y=variance, color=variable)) + 
    geom_line(size=1) + 
    labs(title= "Detrended Variance of Export Revenues over Time",
         subtitle= paste0("Source: ", ds),
         x="Year",
         y="Variance",
         color="")+
    scale_y_continuous(limits=c(0, max_y_axis)) + 
    scale_x_continuous(limits=c(year_start, max(variance_trend$year)),
                       breaks=seq(year_start, max(variance_trend$year), by=2))
  
  assign(paste0(ds, "_plot"), temp_plot)
  
}

plot<-(BS_plot + theme(legend.position = "bottom"))+
  (customs_plot + labs(title = NULL, y=NULL) + theme(legend.position = "bottom"))

print(plot)
ggsave(paste0(finished_output_dir, "5a_detrended_variance_over_time.png"), height=6, width = 12)



