rm(list = ls()); gc()

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data/5) reduced_form_work",
    
    ## update as makes sense for CASD / your own use 
    T ~ "C:/Users/Public/Documents/Big data Project/")
  setwd(root)
}


# Source parameters, define variables and import data
source('2) code/0_set_parameter_values.R')
output_industry_data_use<-paste0(root, "3) output/industry_data_use/")
if(!dir.exists(output_industry_data_use)){
  dir.create(output_industry_data_use)
}
base_vars <- c('turnover', 'capital', 'intangible_fixed_assets', 'empl', 'age', 'comp_weighted_prestige')
linkedin_vars =  gpaste(c("", 'share_'), 'comp_', c('data', 'stem', 'rnd', 'engineer'))
linkedin_vars = c(linkedin_vars, "comp_total")
base_data <- import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'))
NACE_2d_info<- import_file("1) Data/NACE_2d_info/nace_2d_industry_categories.csv")


base_data[, NACE_BR:=str_pad(NACE_BR, 4, side="left", pad="0")]
base_data[, NACE_2d := as.integer(substr(as.character(NACE_BR), 1, 2))]
base_data<-merge(base_data, NACE_2d_info, by="NACE_2d", all.x = T)
base_data[, Industry_Category:=factor(Industry_Category, levels=unique(NACE_2d_info$Industry_Category))][
  , year:=factor(year)
]


# 1) data intensity per industry ---------------

data_usage <- base_data[, lapply(.SD, mean, na.rm = TRUE),
                        by = .(Industry_Category),
                        .SDcols = linkedin_vars]


metrics<-list(mean=function(x) mean(x, na.rm=T),
              top10=function(x){
                x<-x[!is.na(x)]
                top_n <- ceiling(0.1*length(x))
                mean(sort(x, decreasing=T)[1:top_n])/mean(x)
              },
              hhi=function(x){
                x<-x[!is.na(x)]
                s<-x/sum(x)
                sum(s^2)
              })
results_list<-lapply(names(metrics), function(name){
  dt<-base_data[, lapply(.SD, metrics[[name]]), by=Industry_Category, .SDcols=linkedin_vars]
  dt[, dimension:=name]
  return(dt)
})

result_table<-rbindlist(results_list)
setcolorder(result_table, c("Industry_Category", "dimension", linkedin_vars))
fwrite(result_table, paste0(output_industry_data_use, "industry_comp_mean_hhi_top10.csv"))

# 1) data intensity per industry ---------------

# Calculate share of young firms and average data usage at industry-year level
share_young <- base_data[, .(
  total_firms = .N,
  young_firms = sum(young, na.rm = TRUE)
), by = .(NACE_BR, year)][
  , share_young := young_firms / total_firms]

data_usage <- base_data[, lapply(.SD, mean, na.rm = TRUE),
                        by = .(NACE_BR, year),
                        .SDcols = linkedin_vars]

ind_young_data_use <- merge(share_young, data_usage, by = c("NACE_BR", "year"))

# Ensure base_data has 2-digit NACE codes
ind_young_data_use[, NACE_BR:=str_pad(NACE_BR, 4, side="left", pad="0")]
table(nchar(ind_young_data_use$NACE_BR))
ind_young_data_use[, NACE_2d := as.integer(substr(as.character(NACE_BR), 1, 2))]
ind_young_data_use<-merge(ind_young_data_use, NACE_2d_info, by="NACE_2d", all.x = T)
ind_young_data_use[, Industry_Category:=factor(Industry_Category, levels=unique(NACE_2d_info$Industry_Category))][
  , year:=factor(year)
]

for(li_var in linkedin_vars){
  for(var in c("year", "Industry_Category")){
    
    stop()
    plot<-ggplot(ind_young_data_use[share_comp_data<=0.05], aes(x = share_young, y = .data[[li_var]], color = .data[[var]],
                                         text = paste("Industry:", NACE_BR))) +
      geom_point(size = 2, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(group = .data[[var]])) +
      labs(
        title = "Data Usage vs. Share of Young Firms",
        x = "Share of Young Firms",
        y = li_var,
        color = gsub("_", " ", var)
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggplotly(plot)
    ggsave(paste0(output_industry_data_use, "industry_age_data_usage_",li_var, "_", var, ".png"), height=6, width = 8)
  
    
    }
}

setDT(ind_young_data_use)
ind_young_data_use<-ind_young_data_use[total_firms>=4, - "young_firms"]
fwrite(ind_young_data_use, paste0(output_industry_data_use, "ind_young_data_use.csv"))

# 2) graphs key variables by pctile of data intensity ---------------

data_rd_vars =  gpaste(c("", 'share_', "log_"), 'comp_', c('data', 'rnd'))
total_vars = c("comp_total", "log_comp_total")
stem_engineer_vars =  gpaste(c("", 'share_'), 'comp_', c('stem', 'engineer'))
revenue_vars = gpaste(c("", "log_"),  c('dom_turnover', 'total_export_rev_customs'))
pct_vars<-gpaste(c("", "share_"),"comp_data_nace_percentile", c("", "_age"))


d_vars = c("comp_data", "share_comp_data")
divisions_list = list(list('nace', c('NACE_BR', 'year')),list('nace_exporter', c('NACE_BR', 'currently_export', 'year'))) 

for (i in 1:length(divisions_list)){inner = divisions_list[[i]][[1]]; outer = ''; group = divisions_list[[i]][[2]]
for (j in 1:2){if (j ==2){outer = "_age"; group = c(group,'young')}
  
  print(gpaste(d_vars,"_", inner,'_percentile', outer))
  base_data = base_data %>% 
    .[, (gpaste(d_vars,"_", inner,'_percentile', outer)) := lapply(d_vars, function(x) as.factor(ntile(get(x), 100))), by = group] 
}}


for(pct_var in pct_vars){
  
  path<-paste0(output_industry_data_use, "non_disag/", pct_var, "/")
  if(!dir.exists(path)){
    dir.create(path)
  }
  
  summary_pct<-base_data[, lapply(.SD, mean, na.rm=T), .SDcols=c(base_vars, data_rd_vars, stem_engineer_vars, revenue_vars, total_vars), by=pct_var]
  summary_pct<-summary_pct[!is.na(get(pct_var))]
  summary_pct<-summary_pct[, (pct_var):=as.numeric(as.character(get(pct_var)))]

  for(var in c(base_vars, data_rd_vars, stem_engineer_vars, revenue_vars, total_vars)){
    ggplot(summary_pct, aes(x=.data[[pct_var]], y=.data[[var]])) + 
      geom_point() +
      # geom_smooth(method="rlm", se=F, color="blue") +
      labs(x=tools::toTitleCase(gsub("_", " ", pct_var)), y=tools::toTitleCase(gsub("_", " ", var))) +
      scale_x_continuous(breaks=seq(0, 100, by=25)) + 
      theme_minimal()  
      # ggtitle(paste0("Product Entry Rates by Firm-",  j, " ", distrib_text),
      #         subtitle=paste0("Product Entry Variable: ", new_var))
    
    ggsave(paste0(paste0(path, var, "_", pct_var, "_rank.png")), height=4, width = 8)
    
  }
  
}

base_data[, data_category:=fifelse(Industry_Category %in% c("Info & Comms", "Finance", "Professional Services"),
                                   "Comms, Finance & Prof. Services",
                                   fifelse(Industry_Category=="Utilities", "Utilities", "Rest"))]
base_data[, data_category:=as.factor(data_category)]

for(pct_var in c("comp_data_nace_percentile", "share_comp_data_nace_percentile", "comp_data_nace_percentile_age", "share_comp_data_nace_percentile_age")){
  
  # base_data[, N:=.N, by=.(get(pct_var), data_category)]
  base_data[, N_total:=.N, by=get(pct_var)]
  # base_data[, weight:=N/N_total]
  
  
  path<-paste0(output_industry_data_use, "disag/", pct_var, "/")
  if(!dir.exists(path)){
    dir.create(path)
  }
  
  summary_pct<-base_data[, lapply(.SD, function(x) sum(x/N_total, na.rm=T)), 
                         .SDcols=c(base_vars, data_rd_vars, stem_engineer_vars, revenue_vars, total_vars), 
                         by=.(get(pct_var), data_category)]
  summary_pct<-summary_pct[!is.na(get)]
  summary_pct<-summary_pct[, get:=as.numeric(as.character(get))]

  for(var in c(base_vars, data_rd_vars, stem_engineer_vars, revenue_vars, total_vars)){
    ggplot(summary_pct, aes(x=get, y=.data[[var]], fill=data_category)) + 
      geom_bar(stat="identity", position="stack") +
      # scale_y_continuous(labels=scales::percent_format()) +
      # geom_smooth(method="rlm", se=F, color="blue") +
      labs(x=tools::toTitleCase(gsub("_", " ", pct_var)), y=tools::toTitleCase(gsub("_", " ", var))) +
      scale_x_continuous(breaks=seq(0, 100, by=25)) +
      theme_minimal()  + 
      theme(legend.position = "bottom", legend.direction = "horizontal")
    # ggtitle(paste0("Product Entry Rates by Firm-",  j, " ", distrib_text),
    #         subtitle=paste0("Product Entry Variable: ", new_var))
    
    ggsave(paste0(paste0(path, var, "_", pct_var, "_rank.png")), height=4, width = 8)
    
  }
  
}





