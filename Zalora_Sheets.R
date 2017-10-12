library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()
id_hk <- account_list[218,'viewId']
id_indo <- account_list[234,'viewId']
id_my <- account_list[249,'viewId']
id_ph <- account_list[265,'viewId']
id_sg <- account_list[280,'viewId']
id_tw <- account_list[299,'viewId']

id_combined <- c(id_hk,id_indo, id_my, id_ph, id_sg, id_tw)

#my_segments <- ga_segment_list()
#segs <- my_segments$items
HDILASegment <- "gaid::1NZ_PZZyRnmbDSnrQ6-dgA"
seg_HDILA <- segment_ga4("HDILA", segment_id = HDILASegment)

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

startDate <- "2017-10-02"
endDate <- "2017-10-08"

ga_data_merged <- data.frame()

for (i in id_combined) {
  ga_data_temp <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("sessions", "transactions", "itemRevenue"), 
                       dimensions = c("deviceCategory"),
                       segments = c(seg_HDILA, seg_allUsers),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp$id_combined <- i
  ga_data_merged <- rbind(ga_data_merged, ga_data_temp)
}

ga_data_merged <- ga_data_merged %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  group_by(viewName, segment) %>%
  summarize(Sessions = sum(sessions), 
            Transactions = sum(transactions),
            Revenue = sum(itemRevenue)) %>%
  arrange(desc(viewName, segment))


# SKU/product report
ga_SKU_data_merged <- data.frame()

for (i in id_combined) {
  ga_data_temp1 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("deviceCategory"),
                       segments = c(seg_allUsers),
                       filtersExpression = c("ga:campaign=~TheTake"),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp1$id_combined <- i
  ga_SKU_data_merged <- rbind(ga_SKU_data_merged, ga_data_temp1)
}

ga_SKU_data_merged <- ga_SKU_data_merged %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(adContent2 = adContent) %>%
  separate(adContent2, c("ad_product", "ad_SKU", "video_title"), "_", extra = "merge")

#sales report
ga_data_sales_merged <- data.frame()

for (i in id_combined) {
  ga_data_temp1 <- 
    google_analytics_4(i, #=This is a (dynamic) ViewID parameter
                       date_range = c(startDate, endDate), 
                       metrics = c("itemRevenue"), 
                       dimensions = c("deviceCategory"),
                       segments = c(seg_HDILA),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp1$id_combined <- i
  ga_data_sales_merged <- rbind(ga_data_sales_merged, ga_data_temp1)
}

ga_data_sales_merged <- ga_data_sales_merged %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  group_by(viewName, segment) %>%
  summarize(Revenue = sum(itemRevenue)) %>%
  arrange(desc(viewName, segment))

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
