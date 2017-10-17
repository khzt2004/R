library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()
id_hk <- account_list[64,'viewId']
id_indo <- account_list[65,'viewId']
id_my <- account_list[66,'viewId']
id_ph <- account_list[67,'viewId']
id_sg <- account_list[68,'viewId']
id_tw <- account_list[69,'viewId']

id_combined <- c(id_hk,id_indo, id_my, id_ph, id_sg, id_tw)

#my_segments <- ga_segment_list()
#segs <- my_segments$items
HDILASegment <- "gaid::2GZ0l6p8Rf62KRGjWCu-ZQ"
seg_HDILA <- segment_ga4("HDILA", segment_id = HDILASegment)

TheTakeSegment <- "gaid::YLukq_szSomvU98L557Esw"
seg_TheTake <- segment_ga4("TheTake", segment_id = TheTakeSegment)

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

startDate <- "2017-09-08"
endDate <- "2017-10-08"

# Traffic Report
ga_data_merged <- data.frame()

for (i in id_combined) {
  ga_data_temp <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions", "users"), 
                       dimensions = c("deviceCategory", "sourceMedium", "date", "campaign"),
                       segments = c(seg_allUsers),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp$id_combined <- i
  ga_data_merged <- rbind(ga_data_merged, ga_data_temp)
}

ga_data_merged <- ga_data_merged %>%
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


# Clicked Add to Cart report
ga_addCart_data_merged <- data.frame()

for (i in id_combined) {
  ga_data_temp1 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions"), 
                       dimensions = c("deviceCategory", "sourceMedium", "date", "adContent", "campaign", "shoppingStage"),
                       segments = c(seg_allUsers),
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

#Completed purchase report
ga_data_completedpurchase <- data.frame()

for (i in id_combined) {
  ga_data_temp1 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("transactionId", "deviceCategory", "sourceMedium", "date", "adContent", "campaign", "productSKU", "userType"),
                       segments = c(seg_allUsers),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp1$id_combined <- i
  ga_data_completedpurchase <- rbind(ga_data_completedpurchase, ga_data_temp1)
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

write_csv(ga_data_completedpurchase, "allweeks_purchase.csv")

# top of funnel report
ga_data_merged_topFunnel <- data.frame()

for (i in id_combined) {
  ga_data_temp2 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions", "users"), 
                       dimensions = c("deviceCategory", "source","medium", "channelGrouping"),
                       segments = c(seg_allUsers),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp2$id_combined <- i
  ga_data_merged_topFunnel <- rbind(ga_data_merged_topFunnel, ga_data_temp2)
}

patterns <- c("Email", "Organic", 
  "Retargeting", "Affiliate", 
  "cpc", "Onsite", "Referring Source - Diva")

ga_data_merged_topFunnel <- ga_data_merged_topFunnel %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  select(-id_combined, -segment) %>%
  mutate(clean1 = case_when(source == '(direct)' ~ "Onsite",
                            source == 'Affiliate' ~ "Affiliate",
                            grepl('diva, ', source, ignore.case = TRUE) ~"Referring Source - Diva",
                            TRUE ~ medium)) %>%
  mutate(clean2 = case_when(channelGrouping == 'Social' ~ "Social",
                            TRUE ~ clean1)) %>%
  filter(grepl(paste(patterns, collapse = "|"),clean2, ignore.case = TRUE))
 

ga_data_merged_topFunnel_sessions <- ga_data_merged_topFunnel %>%
  select(deviceCategory, sessions, clean2)

ga_data_merged_topFunnel_sessions <- ga_data_merged_topFunnel_sessions %>%
  mutate(row = 1:nrow(ga_data_merged_topFunnel_sessions)) %>%
  spread(clean2, sessions) %>%
  group_by(deviceCategory) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  mutate(type = "sessions") %>%
  rowwise() %>%
  mutate(Email = sum(email, Email),
         Retargeting = sum(retargeting, Retargeting)) %>%
  select(1, 12,6,8,11,3,4,7,9)

ga_data_merged_topFunnel_users <- ga_data_merged_topFunnel %>%
  select(deviceCategory, users, clean2)

ga_data_merged_topFunnel_users <- ga_data_merged_topFunnel_users %>%
  mutate(row = 1:nrow(ga_data_merged_topFunnel_users)) %>%
  spread(clean2, users) %>%
  group_by(deviceCategory) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  mutate(type = "users") %>%
  rowwise() %>%
  mutate(Email = sum(email, Email),
         Retargeting = sum(retargeting, Retargeting)) %>%
  select(1, 12,6,8,11,3,4,7,9)

ga_data_merged_topFunnel_users_Sessions <- rbind(ga_data_merged_topFunnel_users, ga_data_merged_topFunnel_sessions)
write_csv(ga_data_merged_topFunnel, "funnel_wkX.csv")

# purchase metrics report
product_sg <- google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                                 date_range = c(startDate, endDate), 
                                 metrics = c("itemRevenue"), 
                                 dimensions = c("deviceCategory", "userType", "adContent", "productSku", "transactionId"),
                                 segments = c(seg_allUsers), # change segment as needed
                                 anti_sample = TRUE,
                                 max = -1)

pdt_sg_desktop <- filter(product_sg, deviceCategory == "desktop") 
sum(pdt_sg_desktop$itemRevenue)
filter(product_sg, deviceCategory == "desktop") %>% distinct(transactionId)
filter(product_sg, deviceCategory == "desktop") %>% distinct(productSku)
desktop_count <- filter(product_sg, deviceCategory == "desktop") %>% count(productSku)
filter(product_sg, deviceCategory == "desktop") %>% sum(itemRevenue)

pdt_sg_tablet <- filter(product_sg, deviceCategory == "tablet") 
sum(pdt_sg_tablet$itemRevenue)
filter(product_sg, deviceCategory == "tablet") %>% distinct(transactionId)
filter(product_sg, deviceCategory == "tablet") %>% distinct(productSku)
tablet_count <- filter(product_sg, deviceCategory == "tablet") %>% count(productSku)
filter(product_sg, deviceCategory == "tablet") %>% sum(itemRevenue)

# get video/product dict
zalora_Product_worksheet <- gs_title("zalora products_till_week5.csv")
gs_ws_ls(zalora_Product_worksheet)
video_dict <- gs_read(ss=zalora_Product_worksheet, ws = "Videoname_type_dictionary")
video_dict <- as.data.frame(video_dict)
gs_ws <- gs_ws_new(zalora_Product_worksheet, ws_title = "adContent_product_sku separated")
gs_edit_cells(zalora_Product_worksheet, ws="adContent_product_sku separated", input = ga_SKU_data_merged, trim = TRUE)
