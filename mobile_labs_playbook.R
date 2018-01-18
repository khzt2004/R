library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)
library(zoo)


ga_auth(new_user = TRUE)
## get your accounts and view ID
account_list <- ga_account_list()

view_id <- account_list$viewId[account_list$viewName=='Master Global All markets - Filtered View']

id_combined <- c(view_id)

# selecting segments
my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-11-13"
endDate <- "2018-01-15"

# Slide 11 - Current State of Play
# ecommerce conversion rate is used here but other conversion types can be included too
# other conversion types: goalXXCompletions
ga_data_currentstate <- 
  google_analytics_4(view_id, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "transactions", "transactionRevenue"), 
                     dimensions = c("yearMonth", "deviceCategory", "userType"),
                     segments = c(seg_allUsers),
                     anti_sample = TRUE,
                     max = -1)

ga_data_currentstate <-  ga_data_currentstate %>%
  mutate(yearMonth = as.yearmon(as.character(yearMonth), "%Y%m"))


chartData <- ga_data_currentstate %>%
  group_by(yearMonth) %>%
  summarize(Sessions = sum(sessions),
            transactions = sum(transactions)) %>%
  mutate(transactionsPerSession = transactions / Sessions) %>%
  select(-transactions) %>%
  rename(`Conversion Rate` = transactionsPerSession)

# traffic growth rate by device
currentState_trafficGrowth_device <- ga_data_currentstate %>%
  select(yearMonth, deviceCategory, sessions) %>%
  group_by(deviceCategory, yearMonth) %>%
  summarize(sessions = sum(sessions))

currentState_trafficGrowth_device <- currentState_trafficGrowth_device %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  spread(yearMonth, sessions)

currentState_trafficGrowth_device$`% change` <- (currentState_trafficGrowth_device[[3]]-currentState_trafficGrowth_device[[2]])/currentState_trafficGrowth_device[[2]]
currentState_trafficGrowth_device$`% change` <- round(currentState_trafficGrowth_device$`% change`, 3)  
  
# traffic growth rate by new vs returning
currentState_trafficGrowth_userType <- ga_data_currentstate %>%
  select(yearMonth, userType, sessions) %>%
  group_by(userType, yearMonth) %>%
  summarize(sessions = sum(sessions))

currentState_trafficGrowth_userType <- currentState_trafficGrowth_userType %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  spread(yearMonth, sessions)

currentState_trafficGrowth_userType$`% change` <- (currentState_trafficGrowth_userType[[3]]-currentState_trafficGrowth_userType[[2]])/currentState_trafficGrowth_userType[[2]]
currentState_trafficGrowth_userType$`% change` <- round(currentState_trafficGrowth_userType$`% change`, 3)  

# traffic growth rate by new/returning and device category
ga_data_currentstate_all <- ga_data_currentstate %>%
  group_by(userType, deviceCategory, yearMonth) %>%
  summarize(sessions= sum(sessions))

currentState_trafficGrowth_all <- ga_data_currentstate_all %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  spread(yearMonth, sessions)

currentState_trafficGrowth_all$`% change` <- (currentState_trafficGrowth_all[[4]]-currentState_trafficGrowth_all[[3]])/currentState_trafficGrowth_all[[3]]
currentState_trafficGrowth_all$`% change` <- round(currentState_trafficGrowth_all$`% change`, 3)  

# Transactions & Revenue growth rate
ga_data_currentstate_txnrevenue <- ga_data_currentstate %>%
  group_by(deviceCategory, yearMonth) %>%
  summarize(transactions = sum(transactions),
            transactionRevenue = sum(transactionRevenue)) %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  gather(variable, value, -(yearMonth:deviceCategory)) %>%
  unite(temp, yearMonth, variable) %>%
  spread(temp, value)

# Table for Ecommerce Conversion Rate by Device
ga_data_currentstate_ecomm_conv <- ga_data_currentstate %>%
  group_by(deviceCategory, yearMonth) %>%
  summarize(transactions = sum(transactions),
            sessions = sum(sessions)) %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  gather(variable, value, -(yearMonth:deviceCategory)) %>%
  unite(temp, yearMonth, variable) %>%
  spread(temp, value)


# upload data to Googlesheets - what if owner of google sheet is different
my_sheets <- gs_ls()
myworksheet <- gs_key("1ENbfT76-Q_HcmvzuePMTqQjF3EU4IJxMevJ4rgdNK3c")

# alternative - get access to specified google sheets by title
# myworksheet <- gs_title("Sendo_Measurement Deck")
gs_edit_cells(myworksheet, ws = "GA Data", input = chartData, anchor = "J3")
gs_edit_cells(myworksheet, ws = "GA Data", input = currentState_trafficGrowth_device, anchor = "A17")
gs_edit_cells(myworksheet, ws = "GA Data", input = currentState_trafficGrowth_userType, anchor = "A24") 
gs_edit_cells(myworksheet, ws = "GA Data", input = currentState_trafficGrowth_all, anchor = "A30") 
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_currentstate_txnrevenue, anchor = "A39")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_currentstate_ecomm_conv, anchor = "A45") 

