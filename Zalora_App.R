library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()
#id_hk <- account_list[64,'viewId']
#id_indo <- account_list[65,'viewId']
#id_my <- account_list[66,'viewId']
#id_ph <- account_list[67,'viewId']
#id_sg <- account_list[68,'viewId']
#id_tw <- account_list[69,'viewId']

id_hk <- account_list$viewId[account_list$viewName=='HK Web Live View (hkwl hk$)']
id_indo <- account_list$viewId[account_list$viewName=='ID Web Live View (idwl idr)']
id_my <- account_list$viewId[account_list$viewName=='MY Web Live View (mywl myr)']
id_ph <- account_list$viewId[account_list$viewName=='PH Web Live View (phwl php)']
id_sg <- account_list$viewId[account_list$viewName=='SG Web Live View (sgwl sgd)']
id_tw <- account_list$viewId[account_list$viewName=='TW Web Live View (twwl twd)']

id_combined <- c(id_hk,id_indo, id_my, id_ph, id_sg, id_tw)

id_app_hk <- account_list$viewId[account_list$viewName=='HK App Live View (hkal)']
id_app_indo <- account_list$viewId[account_list$viewName=='ID App Live View (idal)']
id_app_my <- account_list$viewId[account_list$viewName=='MY App Live View (myal)']
id_app_ph <- account_list$viewId[account_list$viewName=='PH App Live View (phal)']
id_app_sg <- account_list$viewId[account_list$viewName=='SG App Live View (sgal)']
id_app_tw <- account_list$viewId[account_list$viewName=='TW App Live View (twal)']

id_app_combined <- c(id_app_hk,id_app_indo, id_app_my, id_app_ph, id_app_sg, id_app_tw)

my_segments <- ga_segment_list()
segs <- my_segments$items

HDILA_appSegment <- "gaid::QAn5V6e-TaO4NUwtSnsdQg"
seg_HDILA_appSegment <- segment_ga4("HDILA_appSegment", segment_id = HDILA_appSegment)

HDILA_app_landingscreensegment <- "gaid::2s1LpBdnQkuBsnS0uBdMeQ"
seg_HDILA_app_landingscreensegment <- segment_ga4("HDILA_app_landingscreensegment", segment_id = HDILA_app_landingscreensegment)

HDILA_app_users_screensegment <- "gaid::38idcWJwRGWopoWcHEezrw"
seg_HDILA_app_users_screensegment <- segment_ga4("HDILA_app_users_screensegment", segment_id = HDILA_app_users_screensegment)

HDILA_app_users_non_screensegment <- "gaid::7CyqXwvpSP6gN57g3cQkYg"
seg_HDILA_app_users_non_screensegment <- segment_ga4("HDILA_app_users_non_screensegment", segment_id = HDILA_app_users_non_screensegment)

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-09-08"
endDate <- "2017-10-29"


# App Traffic HDILA Report - place data in "App Traffic Sources" worksheet
ga_data_temp_sg <- 
  google_analytics_4(id_app_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "date"),
                     segments = c(seg_HDILA_appSegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_sg$id_combined <- id_app_sg
ga_data_temp_sg$segment <- "HDILA Sessions"

ga_data_temp_my <- 
  google_analytics_4(id_app_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "date"),
                     segments = c(seg_HDILA_appSegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_my$id_combined <- id_app_my
ga_data_temp_my$segment <- "HDILA Sessions"

ga_data_temp_indo <- 
  google_analytics_4(id_app_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "date"),
                     segments = c(seg_HDILA_appSegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_indo$id_combined <- id_app_indo
ga_data_temp_indo$segment <- "HDILA Sessions"

ga_data_temp_ph <- 
  google_analytics_4(id_app_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "date"),
                     segments = c(seg_HDILA_appSegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_ph$id_combined <- id_app_ph
ga_data_temp_ph$segment <- "HDILA Sessions"

ga_data_temp_hk <- 
  google_analytics_4(id_app_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "date"),
                     segments = c(seg_HDILA_appSegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_hk$id_combined <- id_app_hk
ga_data_temp_hk$segment <- "HDILA Sessions"

ga_data_temp_tw <- 
  google_analytics_4(id_app_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "date"),
                     segments = c(seg_HDILA_appSegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_tw$id_combined <- id_app_tw
ga_data_temp_tw$segment <- "HDILA Sessions"

ga_data_app_traffic <- rbind(ga_data_temp_sg, 
                             ga_data_temp_indo,
                             ga_data_temp_sg,
                             ga_data_temp_my,
                             ga_data_temp_ph,
                             ga_data_temp_tw)

ga_data_app_traffic <- ga_data_app_traffic %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(Country = case_when(grepl("HK", viewName, ignore.case = TRUE) ~"HK",
                             grepl("ID", viewName, ignore.case = TRUE) ~"ID",
                             grepl("SG", viewName, ignore.case = TRUE) ~"SG",
                             grepl("MY", viewName, ignore.case = TRUE) ~"MY",
                             grepl("PH", viewName, ignore.case = TRUE) ~"PH",
                             grepl("TW", viewName, ignore.case = TRUE) ~"TW"),
         date = ymd(date)) %>%
  mutate(Week = case_when(date >= '2017-09-08' & date <= '2017-09-10' ~ "1",
                          date >= '2017-09-11' & date <= '2017-09-17' ~ "2",
                          date >= '2017-09-18' & date <= '2017-09-24' ~ "3",
                          date >= '2017-09-25' & date <= '2017-10-01' ~ "4",
                          date >= '2017-10-02' & date <= '2017-10-08' ~ "5",
                          date >= '2017-10-09' & date <= '2017-10-15' ~ "6",
                          date >= '2017-10-16' & date <= '2017-10-22' ~ "7",
                          date >= '2017-10-23' & date <= '2017-10-29' ~ "8",
                          date >= '2017-10-30' & date <= '2017-11-05' ~ "9",
                          date >= '2017-11-06' & date <= '2017-11-12' ~ "10",
                          date >= '2017-11-13' & date <= '2017-11-19' ~ "11",
                          date >= '2017-11-20' & date <= '2017-11-26' ~ "12",
                          date >= '2017-11-27' & date <= '2017-12-03' ~ "13",
                          date >= '2017-12-04' & date <= '2017-12-10' ~ "14",
                          date >= '2017-12-11' & date <= '2017-12-17' ~ "15"
  ))

# export dataframe as csv to your working directory
write_csv(ga_data_app_traffic, "app_traffic_HDILA_wk7.csv")


# App Traffic landing screen Report - place data in "App Traffic Sources Filtered" worksheet
ga_app_trafficfilter_data_merged <- data.frame()

for (i in id_app_combined) {
  ga_data_temp4 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions", "users"), 
                       dimensions = c("deviceCategory", "date", "landingScreenName"),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp4$id_combined <- i
  ga_app_trafficfilter_data_merged <- rbind(ga_app_trafficfilter_data_merged, ga_data_temp4)
}

ga_app_trafficfilter_data_merged <- ga_app_trafficfilter_data_merged %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  filter(grepl("HDILA", landingScreenName, ignore.case = TRUE)) %>%
  mutate(Country = case_when(grepl("HK", viewName, ignore.case = TRUE) ~"HK",
                             grepl("ID", viewName, ignore.case = TRUE) ~"ID",
                             grepl("SG", viewName, ignore.case = TRUE) ~"SG",
                             grepl("MY", viewName, ignore.case = TRUE) ~"MY",
                             grepl("PH", viewName, ignore.case = TRUE) ~"PH",
                             grepl("TW", viewName, ignore.case = TRUE) ~"TW"),
         date = ymd(date)) %>%
  mutate(Week = case_when(date >= '2017-09-08' & date <= '2017-09-10' ~ "1",
                          date >= '2017-09-11' & date <= '2017-09-17' ~ "2",
                          date >= '2017-09-18' & date <= '2017-09-24' ~ "3",
                          date >= '2017-09-25' & date <= '2017-10-01' ~ "4",
                          date >= '2017-10-02' & date <= '2017-10-08' ~ "5",
                          date >= '2017-10-09' & date <= '2017-10-15' ~ "6",
                          date >= '2017-10-16' & date <= '2017-10-22' ~ "7",
                          date >= '2017-10-23' & date <= '2017-10-29' ~ "8",
                          date >= '2017-10-30' & date <= '2017-11-05' ~ "9",
                          date >= '2017-11-06' & date <= '2017-11-12' ~ "10",
                          date >= '2017-11-13' & date <= '2017-11-19' ~ "11",
                          date >= '2017-11-20' & date <= '2017-11-26' ~ "12",
                          date >= '2017-11-27' & date <= '2017-12-03' ~ "13",
                          date >= '2017-12-04' & date <= '2017-12-10' ~ "14",
                          date >= '2017-12-11' & date <= '2017-12-17' ~ "15"
  ))

# export dataframe as csv to your working directory
write_csv(ga_app_trafficfilter_data_merged, "app_traffic_filter_wk7.csv")

# Clicked Add to Cart report - APP  - place data in "App_Add Cart" worksheet
ga_data_temp6_sg <- 
  google_analytics_4(id_app_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("deviceCategory", "sourceMedium", "date", "shoppingStage", "landingScreenName"),
                     segments = c(seg_HDILA_app_landingscreensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp6_sg$id_combined <- id_app_sg
ga_data_temp6_sg$segment <- "HDILA Landing Screen"

ga_data_temp6_my <- 
  google_analytics_4(id_app_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("deviceCategory", "sourceMedium", "date", "shoppingStage", "landingScreenName"),
                     segments = c(seg_HDILA_app_landingscreensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp6_my$id_combined <- id_app_my
ga_data_temp6_my$segment <- "HDILA Landing Screen"

ga_data_temp6_indo <- 
  google_analytics_4(id_app_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("deviceCategory", "sourceMedium", "date", "shoppingStage", "landingScreenName"),
                     segments = c(seg_HDILA_app_landingscreensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp6_indo$id_combined <- id_app_indo
ga_data_temp6_indo$segment <- "HDILA Landing Screen"

ga_data_temp6_ph <- 
  google_analytics_4(id_app_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("deviceCategory", "sourceMedium", "date", "shoppingStage", "landingScreenName"),
                     segments = c(seg_HDILA_app_landingscreensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp6_ph$id_combined <- id_app_ph
ga_data_temp6_ph$segment <- "HDILA Landing Screen"

ga_data_temp6_tw <- 
  google_analytics_4(id_app_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("deviceCategory", "sourceMedium", "date", "shoppingStage", "landingScreenName"),
                     segments = c(seg_HDILA_app_landingscreensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp6_tw$id_combined <- id_app_tw
ga_data_temp6_tw$segment <- "HDILA Landing Screen"

ga_data_temp6_hk <- 
  google_analytics_4(id_app_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("deviceCategory", "sourceMedium", "date", "shoppingStage", "landingScreenName"),
                     segments = c(seg_HDILA_app_landingscreensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp6_hk$id_combined <- id_app_hk
ga_data_temp6_hk$segment <- "HDILA Landing Screen"


ga_addCart_app_data_merged <- rbind(ga_data_temp6_sg,
                                    ga_data_temp6_my,
                                    ga_data_temp6_indo,
                                    ga_data_temp6_ph,
                                    ga_data_temp6_tw,
                                    ga_data_temp6_hk)

ga_addCart_app_data_merged <- ga_addCart_app_data_merged %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  filter(grepl("HDILA", landingScreenName, ignore.case = TRUE) & 
           grepl("^ADD_TO_CART$", shoppingStage, ignore.case = TRUE)) %>%
  mutate(Country = case_when(grepl("HK", viewName, ignore.case = TRUE) ~"HK",
                             grepl("ID", viewName, ignore.case = TRUE) ~"ID",
                             grepl("SG", viewName, ignore.case = TRUE) ~"SG",
                             grepl("MY", viewName, ignore.case = TRUE) ~"MY",
                             grepl("PH", viewName, ignore.case = TRUE) ~"PH",
                             grepl("TW", viewName, ignore.case = TRUE) ~"TW"),
         date = ymd(date)) %>%
  mutate(Week = case_when(date >= '2017-09-08' & date <= '2017-09-10' ~ "1",
                          date >= '2017-09-11' & date <= '2017-09-17' ~ "2",
                          date >= '2017-09-18' & date <= '2017-09-24' ~ "3",
                          date >= '2017-09-25' & date <= '2017-10-01' ~ "4",
                          date >= '2017-10-02' & date <= '2017-10-08' ~ "5",
                          date >= '2017-10-09' & date <= '2017-10-15' ~ "6",
                          date >= '2017-10-16' & date <= '2017-10-22' ~ "7",
                          date >= '2017-10-23' & date <= '2017-10-29' ~ "8",
                          date >= '2017-10-30' & date <= '2017-11-05' ~ "9",
                          date >= '2017-11-06' & date <= '2017-11-12' ~ "10",
                          date >= '2017-11-13' & date <= '2017-11-19' ~ "11",
                          date >= '2017-11-20' & date <= '2017-11-26' ~ "12",
                          date >= '2017-11-27' & date <= '2017-12-03' ~ "13",
                          date >= '2017-12-04' & date <= '2017-12-10' ~ "14",
                          date >= '2017-12-11' & date <= '2017-12-17' ~ "15"
  ))

# export dataframe as csv to your working directory
write_csv(ga_addCart_app_data_merged, "addcart_app_wk7.csv")

#Completed purchase report - app user engagement  - place data in "App Engagement" worksheet
  ga_data_temp9_sg_users <- 
    google_analytics_4(id_app_sg, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_sg_users$id_combined <- id_app_sg
  ga_data_temp9_sg_users$segment <- "Users - HDILA Screen"
  
  ga_data_temp9_sg_excludeusers <- 
    google_analytics_4(id_app_sg, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_non_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_sg_excludeusers$id_combined <- id_app_sg
  ga_data_temp9_sg_excludeusers$segment <- "Users - exclude HDILA screen"
  
  
  ga_data_temp9_my_users <- 
    google_analytics_4(id_app_my, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_my_users$id_combined <- id_app_my
  ga_data_temp9_my_users$segment <- "Users - HDILA Screen"
  
  ga_data_temp9_my_excludeusers <- 
    google_analytics_4(id_app_my, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_non_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_my_excludeusers$id_combined <- id_app_my
  ga_data_temp9_my_excludeusers$segment <- "Users - exclude HDILA screen"
  
  ga_data_temp9_indo_users <- 
    google_analytics_4(id_app_indo, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_indo_users$id_combined <- id_app_indo
  ga_data_temp9_indo_users$segment <- "Users - HDILA Screen"
  
  ga_data_temp9_indo_excludeusers <- 
    google_analytics_4(id_app_indo, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_non_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_indo_excludeusers$id_combined <- id_app_indo
  ga_data_temp9_indo_excludeusers$segment <- "Users - exclude HDILA screen"
  
  ga_data_temp9_ph_users <- 
    google_analytics_4(id_app_ph, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_ph_users$id_combined <- id_app_ph
  ga_data_temp9_ph_users$segment <- "Users - HDILA Screen"
  
  ga_data_temp9_ph_excludeusers <- 
    google_analytics_4(id_app_ph, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_non_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_ph_excludeusers$id_combined <- id_app_ph
  ga_data_temp9_ph_excludeusers$segment <- "Users - exclude HDILA screen"
  
  
  ga_data_temp9_hk_users <- 
    google_analytics_4(id_app_hk, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_hk_users$id_combined <- id_app_hk
  ga_data_temp9_hk_users$segment <- "Users - HDILA Screen"
  
  ga_data_temp9_hk_excludeusers <- 
    google_analytics_4(id_app_hk, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_non_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_hk_excludeusers$id_combined <- id_app_hk
  ga_data_temp9_hk_excludeusers$segment <- "Users - exclude HDILA screen"
  
  ga_data_temp9_tw_users <- 
    google_analytics_4(id_app_tw, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_tw_users$id_combined <- id_app_tw
  ga_data_temp9_tw_users$segment <- "Users - HDILA Screen"
  
  ga_data_temp9_tw_excludeusers <- 
    google_analytics_4(id_app_tw, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("screenviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_app_users_non_screensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp9_tw_excludeusers$id_combined <- id_app_tw
  ga_data_temp9_tw_excludeusers$segment <- "Users - exclude HDILA screen"
  
  
  ga_data_completedpurchase_app_engagement <- rbind(ga_data_temp9_sg_users,
                                                ga_data_temp9_sg_excludeusers,
                                                ga_data_temp9_my_users,
                                                ga_data_temp9_my_excludeusers,
                                                ga_data_temp9_indo_users,
                                                ga_data_temp9_indo_excludeusers,
                                                ga_data_temp9_ph_users,
                                                ga_data_temp9_ph_excludeusers,
                                                ga_data_temp9_hk_users,
                                                ga_data_temp9_hk_excludeusers,
                                                ga_data_temp9_tw_users,
                                                ga_data_temp9_tw_excludeusers)


  ga_data_completedpurchase_app_engagement <- ga_data_completedpurchase_app_engagement %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(Country = case_when(grepl("HK", viewName, ignore.case = TRUE) ~"HK",
                             grepl("ID", viewName, ignore.case = TRUE) ~"ID",
                             grepl("SG", viewName, ignore.case = TRUE) ~"SG",
                             grepl("MY", viewName, ignore.case = TRUE) ~"MY",
                             grepl("PH", viewName, ignore.case = TRUE) ~"PH",
                             grepl("TW", viewName, ignore.case = TRUE) ~"TW"),
         date = ymd(date)) %>%
  mutate(Week = case_when(date >= '2017-09-08' & date <= '2017-09-10' ~ "1",
                          date >= '2017-09-11' & date <= '2017-09-17' ~ "2",
                          date >= '2017-09-18' & date <= '2017-09-24' ~ "3",
                          date >= '2017-09-25' & date <= '2017-10-01' ~ "4",
                          date >= '2017-10-02' & date <= '2017-10-08' ~ "5",
                          date >= '2017-10-09' & date <= '2017-10-15' ~ "6",
                          date >= '2017-10-16' & date <= '2017-10-22' ~ "7",
                          date >= '2017-10-23' & date <= '2017-10-29' ~ "8",
                          date >= '2017-10-30' & date <= '2017-11-05' ~ "9",
                          date >= '2017-11-06' & date <= '2017-11-12' ~ "10",
                          date >= '2017-11-13' & date <= '2017-11-19' ~ "11",
                          date >= '2017-11-20' & date <= '2017-11-26' ~ "12",
                          date >= '2017-11-27' & date <= '2017-12-03' ~ "13",
                          date >= '2017-12-04' & date <= '2017-12-10' ~ "14",
                          date >= '2017-12-11' & date <= '2017-12-17' ~ "15"
  ))

# export dataframe as csv to your working directory
write_csv(ga_data_completedpurchase_app_engagement, "week7_app_purchase_users_engagement.csv")


# Completed purchase report - landing screen app - place data in "App Purchase Metrics" worksheet

ga_data_temp3_sg <- 
  google_analytics_4(id_app_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU", 'landingScreenName'),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp3_sg$id_combined <- id_app_sg
ga_data_temp3_sg$segment <- "HDILA Landing Screen"


ga_data_temp3_my <- 
  google_analytics_4(id_app_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU", 'landingScreenName'),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp3_my$id_combined <- id_app_my
ga_data_temp3_my$segment <- "HDILA Landing Screen"

ga_data_temp3_indo <- 
  google_analytics_4(id_app_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU", 'landingScreenName'),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp3_indo$id_combined <- id_app_indo
ga_data_temp3_indo$segment <- "HDILA Landing Screen"

ga_data_temp3_ph <- 
  google_analytics_4(id_app_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU", 'landingScreenName'),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp3_ph$id_combined <- id_app_ph
ga_data_temp3_ph$segment <- "HDILA Landing Screen"

ga_data_temp3_hk <- 
  google_analytics_4(id_app_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU", 'landingScreenName'),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp3_hk$id_combined <- id_app_hk
ga_data_temp3_hk$segment <- "HDILA Landing Screen"

ga_data_temp3_tw <- 
  google_analytics_4(id_app_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU", 'landingScreenName'),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp3_tw$id_combined <- id_app_tw
ga_data_temp3_tw$segment <- "HDILA Landing Screen"

ga_data_app_landingscreen_completedpurchase <- rbind(ga_data_temp3_hk, 
                                                    ga_data_temp3_indo,
                                                    ga_data_temp3_sg,
                                                    ga_data_temp3_my,
                                                    ga_data_temp3_ph,
                                                    ga_data_temp3_tw)

ga_data_app_landingscreen_completedpurchase <- ga_data_app_landingscreen_completedpurchase %>%
  filter(grepl("HDILA", landingScreenName, ignore.case = TRUE)) %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(Country = case_when(grepl("HK", viewName, ignore.case = TRUE) ~"HK",
                             grepl("ID", viewName, ignore.case = TRUE) ~"ID",
                             grepl("SG", viewName, ignore.case = TRUE) ~"SG",
                             grepl("MY", viewName, ignore.case = TRUE) ~"MY",
                             grepl("PH", viewName, ignore.case = TRUE) ~"PH",
                             grepl("TW", viewName, ignore.case = TRUE) ~"TW"),
         date = ymd(date)) %>%
  mutate(Week = case_when(date >= '2017-09-08' & date <= '2017-09-10' ~ "1",
                          date >= '2017-09-11' & date <= '2017-09-17' ~ "2",
                          date >= '2017-09-18' & date <= '2017-09-24' ~ "3",
                          date >= '2017-09-25' & date <= '2017-10-01' ~ "4",
                          date >= '2017-10-02' & date <= '2017-10-08' ~ "5",
                          date >= '2017-10-09' & date <= '2017-10-15' ~ "6",
                          date >= '2017-10-16' & date <= '2017-10-22' ~ "7",
                          date >= '2017-10-23' & date <= '2017-10-29' ~ "8",
                          date >= '2017-10-30' & date <= '2017-11-05' ~ "9",
                          date >= '2017-11-06' & date <= '2017-11-12' ~ "10",
                          date >= '2017-11-13' & date <= '2017-11-19' ~ "11",
                          date >= '2017-11-20' & date <= '2017-11-26' ~ "12",
                          date >= '2017-11-27' & date <= '2017-12-03' ~ "13",
                          date >= '2017-12-04' & date <= '2017-12-10' ~ "14",
                          date >= '2017-12-11' & date <= '2017-12-17' ~ "15"
  ))

write_csv(ga_data_app_landingscreen_completedpurchase, "week7_app_landingscreen_purchase.csv")

#Completed purchase report - APP - place data in "App Purchase Data HDILA" worksheet

ga_data_temp_hk <- 
  google_analytics_4(id_app_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_hk$id_combined <- id_app_hk
ga_data_temp_hk$segment <- "Users - HDILA screen"

ga_data_temp_indo <- 
  google_analytics_4(id_app_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_indo$id_combined <- id_app_indo
ga_data_temp_indo$segment <- "Users - HDILA screen"

ga_data_temp_sg <- 
  google_analytics_4(id_app_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_sg$id_combined <- id_app_sg
ga_data_temp_sg$segment <- "Users - HDILA screen"

ga_data_temp_my <- 
  google_analytics_4(id_app_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_my$id_combined <- id_app_my
ga_data_temp_my$segment <- "Users - HDILA screen"

ga_data_temp_ph <- 
  google_analytics_4(id_app_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_ph$id_combined <- id_app_ph
ga_data_temp_ph$segment <- "Users - HDILA screen"

ga_data_temp_tw <- 
  google_analytics_4(id_app_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_tw$id_combined <- id_app_tw
ga_data_temp_tw$segment <- "Users - HDILA screen"

ga_data_app_completedpurchase <- rbind(ga_data_temp_hk, 
                                   ga_data_temp_indo,
                                   ga_data_temp_sg,
                                   ga_data_temp_my,
                                   ga_data_temp_ph,
                                   ga_data_temp_tw)

ga_data_app_completedpurchase <- ga_data_app_completedpurchase %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(Country = case_when(grepl("HK", viewName, ignore.case = TRUE) ~"HK",
                             grepl("ID", viewName, ignore.case = TRUE) ~"ID",
                             grepl("SG", viewName, ignore.case = TRUE) ~"SG",
                             grepl("MY", viewName, ignore.case = TRUE) ~"MY",
                             grepl("PH", viewName, ignore.case = TRUE) ~"PH",
                             grepl("TW", viewName, ignore.case = TRUE) ~"TW"),
         date = ymd(date)) %>%
  mutate(Week = case_when(date >= '2017-09-08' & date <= '2017-09-10' ~ "1",
                          date >= '2017-09-11' & date <= '2017-09-17' ~ "2",
                          date >= '2017-09-18' & date <= '2017-09-24' ~ "3",
                          date >= '2017-09-25' & date <= '2017-10-01' ~ "4",
                          date >= '2017-10-02' & date <= '2017-10-08' ~ "5",
                          date >= '2017-10-09' & date <= '2017-10-15' ~ "6",
                          date >= '2017-10-16' & date <= '2017-10-22' ~ "7",
                          date >= '2017-10-23' & date <= '2017-10-29' ~ "8",
                          date >= '2017-10-30' & date <= '2017-11-05' ~ "9",
                          date >= '2017-11-06' & date <= '2017-11-12' ~ "10",
                          date >= '2017-11-13' & date <= '2017-11-19' ~ "11",
                          date >= '2017-11-20' & date <= '2017-11-26' ~ "12",
                          date >= '2017-11-27' & date <= '2017-12-03' ~ "13",
                          date >= '2017-12-04' & date <= '2017-12-10' ~ "14",
                          date >= '2017-12-11' & date <= '2017-12-17' ~ "15"
  ))


#Completed purchase report exclude users - APP - place data in "App Purchase Data" worksheet

ga_data_temp2_sg <- 
  google_analytics_4(id_app_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_non_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp2_sg$id_combined <- id_app_sg
ga_data_temp2_sg$segment <- "Users - exclude HDILA screen"

ga_data_temp2_my <- 
  google_analytics_4(id_app_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_non_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp2_my$id_combined <- id_app_my
ga_data_temp2_my$segment <- "Users - exclude HDILA screen"

ga_data_temp2_indo <- 
  google_analytics_4(id_app_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_non_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp2_indo$id_combined <- id_app_indo
ga_data_temp2_indo$segment <- "Users - exclude HDILA screen"

ga_data_temp2_ph <- 
  google_analytics_4(id_app_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_non_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp2_ph$id_combined <- id_app_ph
ga_data_temp2_ph$segment <- "Users - exclude HDILA screen"

ga_data_temp2_hk <- 
  google_analytics_4(id_app_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_non_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp2_hk$id_combined <- id_app_hk
ga_data_temp2_hk$segment <- "Users - exclude HDILA screen"

ga_data_temp2_tw <- 
  google_analytics_4(id_app_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("itemRevenue"), 
                     dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                     segments = c(seg_HDILA_app_users_non_screensegment),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp2_tw$id_combined <- id_app_tw
ga_data_temp2_tw$segment <- "Users - exclude HDILA screen"

ga_data_app_excludeusers_completedpurchase <- rbind(ga_data_temp2_hk, 
                                       ga_data_temp2_indo,
                                       ga_data_temp2_sg,
                                       ga_data_temp2_my,
                                       ga_data_temp2_ph,
                                       ga_data_temp2_tw)

ga_data_app_excludeusers_completedpurchase <- ga_data_app_excludeusers_completedpurchase %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(Country = case_when(grepl("HK", viewName, ignore.case = TRUE) ~"HK",
                             grepl("ID", viewName, ignore.case = TRUE) ~"ID",
                             grepl("SG", viewName, ignore.case = TRUE) ~"SG",
                             grepl("MY", viewName, ignore.case = TRUE) ~"MY",
                             grepl("PH", viewName, ignore.case = TRUE) ~"PH",
                             grepl("TW", viewName, ignore.case = TRUE) ~"TW"),
         date = ymd(date)) %>%
  mutate(Week = case_when(date >= '2017-09-08' & date <= '2017-09-10' ~ "1",
                          date >= '2017-09-11' & date <= '2017-09-17' ~ "2",
                          date >= '2017-09-18' & date <= '2017-09-24' ~ "3",
                          date >= '2017-09-25' & date <= '2017-10-01' ~ "4",
                          date >= '2017-10-02' & date <= '2017-10-08' ~ "5",
                          date >= '2017-10-09' & date <= '2017-10-15' ~ "6",
                          date >= '2017-10-16' & date <= '2017-10-22' ~ "7",
                          date >= '2017-10-23' & date <= '2017-10-29' ~ "8",
                          date >= '2017-10-30' & date <= '2017-11-05' ~ "9",
                          date >= '2017-11-06' & date <= '2017-11-12' ~ "10",
                          date >= '2017-11-13' & date <= '2017-11-19' ~ "11",
                          date >= '2017-11-20' & date <= '2017-11-26' ~ "12",
                          date >= '2017-11-27' & date <= '2017-12-03' ~ "13",
                          date >= '2017-12-04' & date <= '2017-12-10' ~ "14",
                          date >= '2017-12-11' & date <= '2017-12-17' ~ "15"
  ))



ga_data_app_allcompletedpurchase <- ga_data_app_completedpurchase %>%
  select(Country, Week, deviceCategory, transactionId, productSKU, itemRevenue) %>%
  mutate(productSKUCount = ifelse(productSKU!="", 1, 0)) %>%
  group_by(Country, Week, deviceCategory) %>%
  summarise(revenueSum = sum(itemRevenue),
            productSKUCount = sum(productSKUCount),
            transactionIdcount = n_distinct(transactionId))

ga_data_app_excludeusers_completedpurchase1 <- ga_data_app_excludeusers_completedpurchase %>%
  select(Country, Week, deviceCategory, transactionId, productSKU, itemRevenue) %>%
  mutate(productSKUCount = ifelse(productSKU!="", 1, 0)) %>%
  group_by(Country, Week, deviceCategory) %>%
  summarise(revenueSum = sum(itemRevenue),
            productSKUCount = sum(productSKUCount),
            transactionIdcount = n_distinct(transactionId))

#group_by(Country, Week, deviceCategory) %>%
#summarise(revenueSum = sum(itemRevenue),
#          productSKUCount = sum(productSKUCount),
#          transactionIdcount = n_distinct(transactionId))

# export dataframe as csv to your working directory
write_csv(ga_data_app_allcompletedpurchase, "week7_appall_purchase.csv")
write_csv(ga_data_app_excludeusers_completedpurchase1, "week7_appexcludeusers_purchase.csv")

