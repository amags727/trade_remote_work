rm(list = ls())

# libraries
library(data.table)
library(haven)
library(readxl)
library(openxlsx)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)

setwd('C:/Users/NEWPROD_A_MAGNUSO/Documents/Big data Project')
raw_data_dir = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"

# import the data ----------------------------------------------
ict_dt = read.csv('C:/Users/NEWPROD_A_MAGNUSO/Documents/Big data Project/1) data/ict_dt.csv')

ict_dt = ict_dt %>% mutate(
  DATA_purchase = ifelse(is.na(DATA_purchase),0,DATA_purchase),
  DATA_sell = ifelse(is.na(DATA_sell),0,DATA_sell),
  data_transactions = factor(ifelse(!DATA_purchase & !DATA_sell, "don't transact data", 
                                                             ifelse(DATA_purchase & !DATA_sell, "buy data",
                                                                    ifelse(!DATA_purchase & DATA_sell, 'sell data', 'buy and sell data')))))



# output basic summary stats about data use  ------------------------------
basic_summary_stats = ict_dt %>% filter(year == 2019) %>% summarize(DATA_use = mean(DATA_use, na.rm = T),
                                                                 DATA_sell = mean(DATA_sell, na.rm = T),
                                                                 DATA_purchase = mean(DATA_purchase, na.rm = T),
                                                                 DATA_inhouse = mean(DATA_inhouse, na.rm = T),
                                                                 DATA_outsource = mean(DATA_outsource, na.rm = T))
basic_summary_stats

industry_specialization = ict_dt %>% filter(year == 2019, !is.na(dominant_nace)) %>% mutate(count = 1) %>% group_by(dominant_nace) %>%
  summarize(firms_in_sample = sum(count),
            DATA_use = mean(DATA_use),
            DATA_sell = mean(ifelse(is.na(DATA_sell),0, DATA_sell)),
            DATA_purchase = mean(ifelse(is.na(DATA_purchase),0, DATA_purchase))
                                 )


industry_specialization_BR = ict_dt %>% filter(year == 2019, !is.na(NACE_BR)) %>% mutate(count = 1) %>% group_by(NACE_BR) %>%
  summarize(firms_in_sample = sum(count),
            DATA_use = mean(DATA_use),
            DATA_sell = mean(ifelse(is.na(DATA_sell),0, DATA_sell)),
            DATA_purchase = mean(ifelse(is.na(DATA_purchase),0, DATA_purchase))
  )
# data use  ---------------------------------------------------------------
## data use x log value add   
ggplot(ict_dt %>% filter(year == 2019, !is.na(value_add)) %>% mutate(DATA_use = as.factor(DATA_use)), aes(x = DATA_use, y= ifelse(value_add>0, log(value_add), .01))) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Use of Big Data by Firm Value Add", x = 'Data Use', y = "Log Value Add") + theme_minimal()

## data use x product reallocation 
ggplot(ict_dt %>% filter(year == 2019, !is.na(survivor_reallocation_rate)) %>% mutate(DATA_use = as.factor(DATA_use)), aes(x = DATA_use, y= survivor_reallocation_rate)) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Use of Big Data by Product Reallocation Rate", x = 'Data Use', y = "Product Reallocation Rate") + theme_minimal()

## data use x product introduction 
ggplot(ict_dt %>% filter(year == 2019, !is.na(survivor_introduced_share)) %>% mutate(DATA_use = as.factor(DATA_use)), aes(x = DATA_use, y= survivor_introduced_share)) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Use of Big Data by Product Introduction Rate", x = 'Data Use', y = "Product Introduction Rate") + theme_minimal()


## detailed use status x log value add
ggplot(ict_dt %>% filter(year == 2019, !is.na(value_add)), aes(x = DATA_use_status, y= ifelse(value_add>0, log(value_add), .01))) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Use of Big Data by Firm Value Add", x = 'Data Use', y = "Log Value Add") + theme_minimal()

## detailed use status x product reallocation
ggplot(ict_dt %>% filter(year == 2019, !is.na(survivor_reallocation_rate)), aes(x = DATA_use_status, y= survivor_reallocation_rate)) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Use of Big Data by Product Reallocation Rate", x = 'Data Use', y = "Product Reallocation Rate") + theme_minimal()

# data transactions -------------------------------------------------------
## transactions x value add
ggplot(ict_dt %>% filter(year == 2019, !is.na(value_add)), aes(x = data_transactions, y= ifelse(value_add>0, log(value_add), .01))) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Use of Big Data by Firm Value Add", x = 'Data Transaction Category', y = "Log Value Add") + theme_minimal()

## purchase data x value add 
ggplot(ict_dt %>% filter(year == 2019, !is.na(value_add)) %>% mutate(DATA_purchase = as.factor(DATA_purchase)), aes(x = DATA_purchase, y= ifelse(value_add>0, log(value_add), .01))) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Purchase of Big Data by Firm Value Add", subtitle =" (All Firms)", x = 'Buy Data', y = "Log Value Add") + theme_minimal()

ggplot(ict_dt %>% filter(year == 2019, !is.na(value_add), DATA_use==1) %>% mutate(DATA_purchase = as.factor(DATA_purchase)), aes(x = DATA_purchase, y= ifelse(value_add>0, log(value_add), .01))) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Purchase of Big Data by Firm Value Add", subtitle =" (Among Big Data Users)", x = 'Buy Data', y = "Log Value Add") + theme_minimal()


ggplot(ict_dt %>% filter(year == 2019, !is.na(value_add)) %>% mutate(DATA_sell = as.factor(DATA_sell)), aes(x = DATA_sell, y= ifelse(value_add>0, log(value_add), .01))) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Sale of Big Data by Firm Value Add", subtitle =" (All Firms)", x = 'Sell Data', y = "Log Value Add") + theme_minimal()

ggplot(ict_dt %>% filter(year == 2019, !is.na(value_add), DATA_use==1) %>% mutate(DATA_sell = as.factor(DATA_sell)), aes(x = DATA_sell, y= ifelse(value_add>0, log(value_add), .01))) + geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  labs(title = "Sale of Big Data by Firm Value Add", subtitle =" (Among Big Data Users)", x = 'Sell Data', y = "Log Value Add") + theme_minimal()





