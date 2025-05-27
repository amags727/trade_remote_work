rm(list = ls()); gc()

## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data",
    
    ## update as makes sense for CASD / your own use 
    T ~ "G:/My Drive/IWH/PhD/GitHub/MDI/Big Data Project_local/")
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
base_data <- import_file(file.path(inputs_dir, '16c_firm_yr_lvl.parquet'))
NACE_2d_info<- import_file("1) Data/NACE_2d_info/nace_2d_industry_categories.csv")


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
    ggplot(ind_young_data_use, aes(x = share_young, y = .data[[li_var]], color = .data[[var]])) +
      geom_point(size = 2, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      labs(
        title = "Data Usage vs. Share of Young Firms",
        x = "Share of Young Firms",
        y = li_var,
        color = gsub("_", " ", var)
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggsave(paste0(output_industry_data_use, "industry_age_data_usage_",li_var, "_", var, ".png"), height=6, width = 8)
  }
  
}

fwrite(ind_young_data_use, paste0(output_industry_data_use, "ind_young_data_use.csv"))
