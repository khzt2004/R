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
#my_segments <- ga_segment_list()
#segs <- my_segments$items
HDILASegment <- "gaid::1NZ_PZZyRnmbDSnrQ6-dgA"
seg_HDILA <- segment_ga4("HDILA", segment_id = HDILASegment)

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

startDate <- "2017-09-10"
endDate <- "2017-09-30"

HDILA_Table_HK <- google_analytics_4(id_hk, date_range = c(startDate, endDate), 
                                         metrics = c("sessions", "transactions", "itemRevenue"), 
                                         dimensions = c("deviceCategory"),
                                         segments = c(seg_HDILA, seg_allUsers),
                                         anti_sample = TRUE)

HDILA_Table_HK <- HDILA_Table_HK %>%
  mutate(Property = "HK") %>%
  select(6, 2:5) %>%
  group_by(Property, segment) %>%
  summarize(Sessions = sum(sessions), 
            Transactions = sum(transactions),
            Revenue = sum(itemRevenue))


df <- rbind(HDILA_Table_HK, df2, df3, df4, df5)








