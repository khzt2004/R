library(tidyverse)
library (bigrquery)
library (lubridate)

# Big Query Setting
project <- "unified-welder-172709"
targets_dataset <- "TH_Marketing_Performance_Dashboard_Targets"

target_data <- read_csv("TH_Monthly_Targets.csv")
shopee_lazada <- read_csv("Shopee_Lazada_Ach.csv")

# master table
target_data_master <- target_data %>%
  gather("Metric", "Target", 8:13) 

shopee_lazada_master <- shopee_lazada %>%
  gather("Metric", "Value", 9:12)

combined_Target_Ach_master <- shopee_lazada_master %>%
  left_join(target_data_master, by = c("Year", "Country", "Month", "Channel", 
                                        "Category", 
                                       "Partnership", "Metric"))

# monthly table
target_data_MTD <- target_data %>%
  gather("Metric", "Value", 8:13) %>%
  filter(Year == format(Sys.time(), "%Y") & Month == format(Sys.time(), "%b")) %>%
  group_by(Metric) %>%
  summarise(Value = sum(Value)) %>%
  select(Metric, Target = Value)

shopee_lazada_long_MTD <- shopee_lazada %>%
  gather("Metric", "Value", 9:12) %>%
  filter(Year == format(Sys.time(), "%Y") & Month == format(Sys.time(), "%b")) %>%
  group_by(Metric) %>%
  summarise(Value = sum(Value)) %>%
  select(Metric, Achieved = Value)

combined_Target_Ach_MTD <- target_data_MTD %>%
  left_join(shopee_lazada_long_MTD, by = "Metric") %>%
  mutate(pct_Achieved = Achieved/Target)

# yearly table
target_data_YTD <- target_data %>%
  gather("Metric", "Value", 8:13) %>%
  filter(Year == format(Sys.time(), "%Y")) %>%
  group_by(Metric) %>%
  summarise(Value = sum(Value)) %>%
  select(Metric, Target = Value)

shopee_lazada_long_YTD <- shopee_lazada %>%
  gather("Metric", "Value", 9:12) %>%
  filter(Year == format(Sys.time(), "%Y")) %>%
  group_by(Metric) %>%
  summarise(Value = sum(Value)) %>%
  select(Metric, Achieved = Value)

combined_Target_Ach_YTD <- target_data_YTD %>%
  left_join(shopee_lazada_long_YTD, by = "Metric") %>%
  mutate(pct_Achieved = Achieved/Target)


# get the data by each date
BQ_get_actuals <- function(dateStart, dateEnd){
  
  get_actuals_query <- paste0("
                             ")
  
  actuals_data <- query_exec(get_actuals_query, project, destination_table = NULL, max_pages = Inf)
  return(actuals_data)
}

BQ_actuals_data <- BQ_get_actuals(start_date, end_date)

# Variables for the BigQuery upload portion
destinationProject <- 'unified-welder-172709'
destinationDataset <- 'TH_Marketing_Performance_Dashboard_Targets'
reportName <- 'BQ_Target_Actuals'

# Check if the table exists, if table exists, then delete the table
tryCatch(delete_table(destinationProject, destinationDataset, reportName),
         error = function(e){
           print(paste0(reportName, " not available for deletion"))
         })

# Upload the table into big query
tryCatch(insert_upload_job(destinationProject, destinationDataset, reportName, BQ_actuals_data),
         error = function(e){
           print(paste0(reportName, " failed to upload"))
         })


