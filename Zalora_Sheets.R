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

usersvisitedHDILA_Segment <- "gaid::1DPCmoswQbGKIbx7v99XOw"
seg_HDILA_users <- segment_ga4("usersvisitedHDILA_Segment", segment_id = usersvisitedHDILA_Segment)

usersdidntvisitHDILA_Segment <- "gaid::RHho8mgVSV-kwSvCzcpQDA"
seg_nonHDILA_users <- segment_ga4("usersdidntvisitHDILA_Segment", segment_id = usersdidntvisitHDILA_Segment)

sessionsvisitedHDILA_Segment <- "gaid::l7_7cZprTVe35U9LE2E_3Q"
seg_HDILA_sessions <- segment_ga4("sessionsvisitedHDILA_Segment", segment_id = sessionsvisitedHDILA_Segment)

sessions_addCart_visitedHDILA_Segment <- "gaid::a_C43yiuSAqTE4Ma9NWqFw"
seg_addCart_HDILA_sessions <- segment_ga4("sessions_addCart_visitedHDILA_Segment", segment_id = sessions_addCart_visitedHDILA_Segment)

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-09-08"
endDate <- "2017-10-29"

# Traffic Report - paste data into "Web Traffic Sources" worksheet
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


# Clicked Add to Cart report - paste data into "Add_Cart" worksheet
ga_addCart_data_merged <- data.frame()

for (i in id_combined) {
  ga_data_temp1 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions"), 
                       dimensions = c("deviceCategory", "sourceMedium", "date", "adContent", "campaign", "shoppingStage"),
                       segments = c(seg_addCart_HDILA_sessions),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp1$id_combined <- i
  ga_addCart_data_merged <- rbind(ga_addCart_data_merged, ga_data_temp1)
}

ga_addCart_data_merged <- ga_addCart_data_merged %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  filter(campaign == 'TheTake' & shoppingStage == 'ADD_TO_CART') %>%
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



#Completed purchase report - sessions - paste data into "Purchase Data" worksheet
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


#Completed purchase report - user engagement - paste data into "Purchase_Data_Engagement" worksheet
ga_data_completedpurchase_engagement <- data.frame()

for (i in id_combined) {
  ga_data_temp7 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("pageviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_HDILA_users),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp7$id_combined <- i
  ga_data_temp7$segment <- "Users visited HDILA page"
  ga_data_completedpurchase_engagement <- rbind(ga_data_completedpurchase_engagement, ga_data_temp7)
}

ga_data_completedpurchase_engagement <- ga_data_completedpurchase_engagement %>%
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

ga_data_completedpurchase_nonHDILA_engagement <- data.frame()

for (i in id_combined) {
  ga_data_temp_b <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("pageviews", "sessions", "sessionDuration"), 
                       dimensions = c("deviceCategory", "date"),
                       segments = c(seg_nonHDILA_users),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp_b$id_combined <- i
  ga_data_temp_b$segment <- "Users did not visit HDILA page"
  ga_data_completedpurchase_nonHDILA_engagement <- rbind(ga_data_completedpurchase_nonHDILA_engagement, ga_data_temp_b)
}

ga_data_completedpurchase_nonHDILA_engagement <- ga_data_completedpurchase_nonHDILA_engagement %>%
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

ga_data_completedpurchase_all_engagement <- rbind(ga_data_completedpurchase_engagement, ga_data_completedpurchase_nonHDILA_engagement)

# export dataframe as csv to your working directory
write_csv(ga_data_completedpurchase_all_engagement, "week7_purchase_users_engagement.csv")

#Completed purchase report - users - paste data into "Purchase_Data_User" worksheet
ga_data_completedpurchase <- data.frame()

for (i in id_combined) {
  ga_data_temp2 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("transactionId", "deviceCategory", "date", "adContent", "productSKU"),
                       segments = c(seg_HDILA_users),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp2$id_combined <- i
  ga_data_temp2$segment <- "Users visited HDILA page"
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


ga_data_nonHDILA_completedpurchase <- data.frame()

for (i in id_combined) {
  ga_data_temp_a <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("transactionId", "deviceCategory", "date", "adContent", "productSKU"),
                       segments = c(seg_nonHDILA_users),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp_a$id_combined <- i
  ga_data_temp_a$segment <- "Users did not visit HDILA page"
  ga_data_nonHDILA_completedpurchase <- rbind(ga_data_nonHDILA_completedpurchase, ga_data_temp_a)
}

ga_data_nonHDILA_completedpurchase <- ga_data_nonHDILA_completedpurchase %>%
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

ga_data_total_completedpurchase <- rbind(ga_data_nonHDILA_completedpurchase, ga_data_completedpurchase)

ga_data_allcompletedpurchase1 <- ga_data_total_completedpurchase %>%
  select(Country, Week, segment, deviceCategory, transactionId, productSKU, itemRevenue) %>%
  mutate(productSKUCount = ifelse(productSKU!="", 1, 0)) %>%
  group_by(Country, Week, segment, deviceCategory) %>%
  summarise(revenueSum = sum(itemRevenue),
            productSKUCount = sum(productSKUCount),
            transactionIdcount = n_distinct(transactionId))


# export dataframe as csv to your working directory
write_csv(ga_data_allcompletedpurchase1, "week7_purchase_users.csv")


