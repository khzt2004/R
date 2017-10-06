library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()
id_hk <- account_list[210,'viewId']
id_indo <- account_list[211,'viewId']
id_my <- account_list[212,'viewId']
id_ph <- account_list[213,'viewId']
id_sg <- account_list[214,'viewId']
id_tw <- account_list[215,'viewId']

id_combined <- c(id_hk,id_indo, id_my, id_ph, id_sg, id_tw)

#my_segments <- ga_segment_list()
#segs <- my_segments$items
HDILASegment <- "gaid::1NZ_PZZyRnmbDSnrQ6-dgA"
seg_HDILA <- segment_ga4("HDILA", segment_id = HDILASegment)

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

startDate <- "2017-09-18"
endDate <- "2017-09-24"

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
                       metrics = c("sessions", "pageViews"), 
                       dimensions = c("deviceCategory", "source","medium", "channelGrouping"),
                       segments = c(seg_allUsers),
                       anti_sample = TRUE,
                       max = -1)
  ga_data_temp2$id_combined <- i
  ga_data_merged_topFunnel <- rbind(ga_data_merged_topFunnel, ga_data_temp2)
}

ga_data_merged_topFunnel <- ga_data_merged_topFunnel %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  group_by(viewName, source, medium, channelGrouping, segment) %>%
  summarize(Sessions = sum(sessions), 
            Pageviews = sum(pageViews)) %>%
  arrange(desc(viewName, segment)) %>%
  mutate(clean1 = case_when(source == '(direct)' ~ "Onsite",
                            source == 'Affiliate' ~ "Affiliate",
                            TRUE ~ medium)) %>%
  mutate(clean2 = case_when(channelGrouping == 'Social' ~ "Social",
                            TRUE ~ clean1))
write_csv(ga_data_merged_topFunnel, "funnel_wkX.csv")

ga_data_merged_topFunnel_sum <- ga_data_merged_topFunnel %>%
  filter(clean1 == 'cpc' && viewName == 'SG Rollup ZALORA | The Take (sgrt)') 
sum(ga_data_merged_topFunnel_sum$Sessions)

# get video/product dict
zalora_Product_worksheet <- gs_title("zalora products_till_week5.csv")
gs_ws_ls(zalora_Product_worksheet)
video_dict <- gs_read(ss=zalora_Product_worksheet, ws = "Videoname_type_dictionary")
video_dict <- as.data.frame(video_dict)
gs_ws <- gs_ws_new(zalora_Product_worksheet, ws_title = "adContent_product_sku separated")
gs_edit_cells(zalora_Product_worksheet, ws="adContent_product_sku separated", input = ga_SKU_data_merged, trim = TRUE)
