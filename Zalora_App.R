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
HDILASegment <- "gaid::2GZ0l6p8Rf62KRGjWCu-ZQ"
seg_HDILA <- segment_ga4("HDILA", segment_id = HDILASegment)

TheTakeSegment <- "gaid::YLukq_szSomvU98L557Esw"
seg_TheTake <- segment_ga4("TheTake", segment_id = TheTakeSegment)

HDILA_appSegment <- "gaid::QAn5V6e-TaO4NUwtSnsdQg"
seg_HDILA_appSegment <- segment_ga4("HDILA_appSegment", segment_id = HDILA_appSegment)

HDILA_sessionssegment <- "gaid::9EIdwigvR8elGVvGZYWjmw"
seg_HDILA_sessionssegment <- segment_ga4("HDILA_sessions_segment", segment_id = HDILA_sessionssegment)

HDILA_app_landingscreensegment <- "gaid::2s1LpBdnQkuBsnS0uBdMeQ"
seg_HDILA_app_landingscreensegment <- segment_ga4("HDILA_app_landingscreensegment", segment_id = HDILA_app_landingscreensegment)

HDILA_excludeapp_screensegment <- "gaid::PNtM4s1tQ-OVfdNzmSu62A"
HDILA_excludeapp_screensegment <- segment_ga4("HDILA_excludeapp_screensegment", segment_id = HDILA_excludeapp_screensegment)

HDILA_excludeapp_landingscreensegment <- "gaid::xwG6eGScR-G4TzzzdBMwqA"
HDILA_excludeapp_landingscreensegment <- segment_ga4("HDILA_excludeapp_landingscreensegment", segment_id = HDILA_excludeapp_landingscreensegment)

HDILA_app_users_landingscreensegment <- "gaid::6zzXz7Z2QrO4XeACE5TX_A"
seg_HDILA_app_users_landingscreensegment <- segment_ga4("HDILA_app_users_landingscreensegment", segment_id = HDILA_app_users_landingscreensegment)

HDILA_app_users_non_landingscreensegment <- "gaid::TJeP0oWfQeylO94YpZbO3Q"
seg_HDILA_app_users_non_landingscreensegment <- segment_ga4("HDILA_app_users_non_landingscreensegment", segment_id = HDILA_app_users_non_landingscreensegment)


segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-09-08"
endDate <- "2017-10-22"

#Completed purchase report - APP

ga_data_app_completedpurchase <- data.frame()

for (i in id_app_combined) {
  ga_data_temp6 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                       segments = c(seg_HDILA_sessionssegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp6$id_app_combined <- i
  ga_data_app_completedpurchase <- rbind(ga_data_app_completedpurchase, ga_data_temp6)
}

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

ga_data_app_excludeusers_completedpurchase <- data.frame()

for (i in id_app_combined) {
  ga_data_temp7 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("transactionId", "deviceCategory", "date", "productSKU"),
                       segments = c(seg_HDILA_app_users_non_landingscreensegment),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp7$id_app_combined <- i
  ga_data_app_excludeusers_completedpurchase <- rbind(ga_data_app_excludeusers_completedpurchase, ga_data_temp7)
}

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
  filter(segment = )
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

