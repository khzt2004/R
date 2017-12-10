library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()

id_stJ <- account_list$viewId[account_list$viewName=='Main Site & Booking Excl. St John Staff']

my_segments <- ga_segment_list()
segs <- my_segments$items

#usersHDILA_Segment <- "gaid::l1sUng85RTaQLNgGmwmIyA"
#seg_HDILA_users <- segment_ga4("usersHDILA_Segment", segment_id = usersHDILA_Segment)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-06-08"
endDate <- "2017-11-28"

# Dataframe - customer ID
ga_traffic_data_merged <- data.frame()

ga_data_temp <- 
  google_analytics_4(id_stJ, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("dimension3", "userType", "deviceCategory"),
                     anti_sample = TRUE,
                     max = -1)

CID_devices <- ga_data_temp %>%
  group_by(dimension3, userType, deviceCategory) %>%
  summarise(sessions = sum(sessions)) %>%
  ungroup() %>%
  spread(userType, sessions) %>%
  group_by(dimension3, deviceCategory) %>%
  summarise(`New Visitor` = sum(`New Visitor`),
            `Returning Visitor` = sum(`Returning Visitor`)) %>%
  arrange(dimension3)

CID_devices_devicecategory <- ga_data_temp %>%
  group_by(dimension3, userType, deviceCategory) %>%
  summarise(sessions = sum(sessions)) %>%
  ungroup() %>%
  spread(deviceCategory, sessions) %>%
  filter(desktop > 0 & mobile > 0)


write_csv(ga_usertraffic_data_merged_test, "ga_user_traffic_data_merged.csv")