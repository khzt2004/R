library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()


id_hk <- account_list$viewId[account_list$viewName=='HK Web Live View (hkwl hk$)']

id_combined <- c(id_hk,id_indo, id_my, id_ph, id_sg, id_tw)

my_segments <- ga_segment_list()
segs <- my_segments$items


segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-11-13"
endDate <- "2017-12-03"

# Slide 11 - Current State of Play
ga_traffic_data_merged <- data.frame()

ga_data_temp_sg <- 
  google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sourceMedium", "date", "campaign"),
                     segments = c(seg_HDILA_sessions),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_sg$id_combined <- id_sg
ga_data_temp_sg$segment <- "Sessions visited HDILA"

ga_data_temp_my <- 
  google_analytics_4(id_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sourceMedium", "date", "campaign"),
                     segments = c(seg_HDILA_sessions),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_my$id_combined <- id_my
ga_data_temp_my$segment <- "Sessions visited HDILA"




# upload data to Googlesheets
my_sheets <- gs_ls()
myworksheet <- gs_title("Sendo_Measurement Deck")
gs_edit_cells(myworksheet, ws = "GA Data", input = "cow", anchor = "A3")
