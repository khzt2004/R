library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)

ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()

id_rollup <- account_list$viewId[account_list$viewName=='Roll-up All (filtered)']

id_combined <- c(id_rollup)

my_segments <- ga_segment_list()
segs <- my_segments$items

sgusers_Segment <- "gaid::sas0HinySmy6nw7f844mLg"
seg_sg_users <- segment_ga4("usersfromSG_Segment", segment_id = sgusers_Segment)

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-11-01"
endDate <- "2018-01-31"

ga_data_temp_sg <- 
  google_analytics(id_combined,
                   start=startDate, end=endDate, 
                   metrics = c("totalConversions", "totalConversionValue"), 
                   dimensions = c("timeLagInDaysHistogram", "conversionType"),
                   type = "mcf",
                   segment = c(seg_sg_users),
                   samplingLevel = "WALK",
                   max = 99999999)

ga_data_temp_sg$totalConversions <- as.numeric(ga_data_temp_sg$totalConversions)
ga_data_temp_sg$totalConversionValue <- as.numeric(ga_data_temp_sg$totalConversionValue)

ga_data_temp_sg1 <- ga_data_temp_sg %>%
  filter(conversionType == 'Transaction') %>%
  group_by(timeLagInDaysHistogram) %>%
  summarise(totalConversions = sum(totalConversions),
            totalConversionValue = sum(totalConversionValue)) %>%
  mutate(pctTotalConversions = 100*(totalConversions/sum(totalConversions)),
         pctTotalConversionValue = 100*(totalConversionValue/sum(totalConversionValue)))

ga_data_temp_sg2 <- ga_data_temp_sg1 %>%
  filter(timeLagInDaysHistogram %in% c("001", "002", "003"
                                       , "004", "005", "006", "007"))

sum(ga_data_temp_sg2$pctTotalConversions)
sum(ga_data_temp_sg2$pctTotalConversionValue)

