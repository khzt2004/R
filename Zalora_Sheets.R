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

id_hk <- account_list$viewId[account_list$viewName=='HK Rollup ZALORA | The Take (hkrt)']
id_indo <- account_list$viewId[account_list$viewName=='ID Rollup ZALORA | The Take (idrt)']
id_my <- account_list$viewId[account_list$viewName=='MY Rollup ZALORA | The Take (myrt)']
id_ph <- account_list$viewId[account_list$viewName=='PH Rollup ZALORA | The Take (phrt)']
id_sg <- account_list$viewId[account_list$viewName=='SG Rollup ZALORA | The Take (sgrt)']
id_tw <- account_list$viewId[account_list$viewName=='TW Rollup ZALORA | The Take (twrt)']

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

usersvisitedHDILA_Segment <- "gaid::1DPCmoswQbGKIbx7v99XOw"
seg_HDILA_users <- segment_ga4("usersvisitedHDILA_Segment", segment_id = usersvisitedHDILA_Segment)

usersdidntvisitHDILA_Segment <- "gaid::RHho8mgVSV-kwSvCzcpQDA"
seg_nonHDILA_users <- segment_ga4("usersdidntvisitHDILA_Segment", segment_id = usersdidntvisitHDILA_Segment)


HDILA_LP <- "gaid::2GZ0l6p8Rf62KRGjWCu-ZQ"
seg_HDILA_LP <- segment_ga4("HDILA_LP", segment_id = HDILA_LP)

HDILA_appSegment <- "gaid::QAn5V6e-TaO4NUwtSnsdQg"
seg_HDILA_appSegment <- segment_ga4("HDILA_appSegment", segment_id = HDILA_appSegment)

sessionsvisitedHDILA_Segment <- "gaid::l7_7cZprTVe35U9LE2E_3Q"
seg_HDILA_sessions <- segment_ga4("sessionsvisitedHDILA_Segment", segment_id = sessionsvisitedHDILA_Segment)

sessions_exclude_visitedHDILA_Segment <- "gaid::ESjhzsAKRwqbe6G1NL10xg"
seg_exclude_HDILA_sessions <- segment_ga4("sessions_exclude_visitedHDILA_Segment", segment_id = sessions_exclude_visitedHDILA_Segment)

HDILA_app_landingscreensegment <- "gaid::2s1LpBdnQkuBsnS0uBdMeQ"
seg_HDILA_app_landingscreensegment <- segment_ga4("HDILA_app_landingscreensegment", segment_id = HDILA_app_landingscreensegment)

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-09-08"
endDate <- "2017-10-22"

# Traffic Report
ga_traffic_data_merged <- data.frame()

for (i in id_combined) {
  ga_data_temp <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions", "users"), 
                       dimensions = c("deviceCategory", "sourceMedium", "date", "campaign"),
                       segments = c(seg_HDILA_sessions),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp$id_combined <- i
  ga_traffic_data_merged <- rbind(ga_traffic_data_merged, ga_data_temp)
}

ga_traffic_data_merged <- ga_traffic_data_merged %>%
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
write_csv(ga_traffic_data_merged, "traffic_wk7.csv")


# App Traffic HDILA Report
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


# App Traffic landing screen Report
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


# Clicked Add to Cart report
ga_addCart_data_merged <- data.frame()

for (i in id_combined) {
  ga_data_temp1 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions"), 
                       dimensions = c("deviceCategory", "sourceMedium", "date", "adContent", "campaign", "shoppingStage"),
                       segments = c(seg_HDILA_sessions),
                       filtersExpression = "ga:shoppingStage==ADD_TO_CART",
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp1$id_combined <- i
  ga_addCart_data_merged <- rbind(ga_addCart_data_merged, ga_data_temp1)
}

ga_addCart_data_merged <- ga_addCart_data_merged %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  filter(campaign == 'TheTake') %>%
  mutate(adContent2 = adContent) %>%
  separate(adContent2, c("ad_product", "ad_SKU", "video_title"), "_", extra = "merge") %>%
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
write_csv(ga_addCart_data_merged, "addcart_wk7.csv")


# Clicked Add to Cart report - APP
ga_addCart_app_data_merged <- data.frame()

for (i in id_app_combined) {
  ga_data_temp6 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions"), 
                       dimensions = c("deviceCategory", "sourceMedium", "date", "shoppingStage", "landingScreenName"),
                       segments = c(seg_HDILA_app_landingscreensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp6$id_combined <- i
  ga_addCart_app_data_merged <- rbind(ga_addCart_app_data_merged, ga_data_temp6)
}

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

#Completed purchase report - sessions
ga_data_completedpurchase <- data.frame()

for (i in id_combined) {
  ga_data_temp2 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("transactionId", "deviceCategory", "sourceMedium", "date", "adContent", "campaign", "productSKU", "userType"),
                       segments = c(seg_HDILA_sessions),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp2$id_combined <- i
  ga_data_completedpurchase <- rbind(ga_data_completedpurchase, ga_data_temp2)
}

ga_data_completedpurchase <- ga_data_completedpurchase %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(adContent2 = adContent) %>%
  separate(adContent2, c("ad_product", "ad_SKU", "video_title"), "_", extra = "merge") %>%
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
write_csv(ga_data_completedpurchase, "week7_purchase_sessions.csv")


#Completed purchase report - users
ga_data_completedpurchase <- data.frame()

for (i in id_combined) {
  ga_data_temp2 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("transactionId", "deviceCategory", "sourceMedium", "date", "adContent", "campaign", "productSKU", "userType"),
                       segments = c(seg_HDILA_users,seg_nonHDILA_users),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp2$id_combined <- i
  ga_data_completedpurchase <- rbind(ga_data_completedpurchase, ga_data_temp2)
}

ga_data_completedpurchase <- ga_data_completedpurchase %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(adContent2 = adContent) %>%
  separate(adContent2, c("ad_product", "ad_SKU", "video_title"), "_", extra = "merge") %>%
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
write_csv(ga_data_completedpurchase, "week7_purchase_users.csv")




