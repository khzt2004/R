library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(ggthemes)
library(ggalt)
library(googlesheets)
library(reshape2)
library(zoo)
library(easyRFM)

ga_auth(new_user = TRUE)
## get your accounts and view ID
account_list <- ga_account_list()

# change the view name to the view that you wish to conduct analysis on
# view_id <- account_list$viewId[account_list$viewName=='Lazada SG - SGD']
view_id <- account_list$viewId[account_list$viewName=='Roll-up All (filtered)']
id_combined <- c(view_id)

# selecting segments
my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

se_trans <- segment_element("transactions", 
                            operator = "GREATER_THAN", 
                            type = "METRIC", 
                            comparisonValue = 0, 
                            scope = "USER")

se_usertype <- segment_element("userType", 
                               operator = "EXACT", 
                               type = "DIMENSION", 
                               expressions = "New Visitor",
                               scope = "USER")

sv_simple <- segment_vector_simple(list(list(se_trans)))
sv_simple2 <- segment_vector_simple(list(list(se_usertype)))
seg_defined <- segment_define(list(sv_simple))
segment_trans_usertype <- segment_ga4("simple", user_segment = seg_defined)

segment_for_usertype <- "gaid::wZRM4rlFQvqpC-mzu6frBg"
seg_usertype <- segment_ga4("usertype", segment_id = segment_for_usertype)

segment_def_for_call <- "sessions::condition::ga:deviceCategory==desktop"

## make the v3 segment object in the v4 segment object:
seg_obj_desktop <- segment_ga4("Desktop", segment_id = segment_def_for_call)


# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-05-01"
endDate <- "2017-05-01"

startDate2 <- "2018-03-31"
endDate2 <- "2018-03-31"

ga_data_convRate <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate2), 
                   metrics = c("transactionsPerSession"), 
                   dimensions = c("date"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_convRate %>%
  ggplot(aes(x = date, y = transactionsPerSession, color=transactionsPerSession)) +
  geom_line() +
  labs(title = "Conversion Rate Trends", y = "Conversion Rate", x = "") +
  theme_minimal()

ga_data_convRatechg <- ga_data_convRate[,"transactionsPerSession"][ga_data_convRate$date == max(ga_data_convRate$date)]/
  ga_data_convRate[,"transactionsPerSession"][ga_data_convRate$date == min(ga_data_convRate$date)]

# find channelgrouping outliers and compare % change against outlier

ga_data_channelgrouping_baseline_start <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate),
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("channelGrouping"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_channelgrouping_baseline_end <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate2, endDate2),
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("channelGrouping"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_channelgrouping_baselinegroup <- ga_data_channelgrouping_baseline_start %>%
  select(channelGrouping, session_start = "sessions", transactions_start = 'transactions') %>%
  left_join(ga_data_channelgrouping_baseline_end, by="channelGrouping") %>%
  mutate(sessionstotal = sum(sessions, na.rm=TRUE)) %>%
  group_by(channelGrouping) %>%
  summarise(transactionsPerSessionchg = round(((sum(transactions)/sum(sessions))/(sum(transactions_start)/sum(session_start))-1)*100, 2),
            shareofSessions = round((sum(sessions)/sum(sessionstotal))*100,2)) %>%
  top_n(3, shareofSessions) %>%
  select(`Channel Grouping` = 'channelGrouping',
         `% change in CR` = 'transactionsPerSessionchg',
          `Share of Sessions` = 'shareofSessions')

# create segment from channelGrouping
se_channelgroup <- segment_element("channelGrouping", 
                               operator = "REGEXP", 
                               type = "DIMENSION", 
                               expressions = paste0(ga_data_channelgrouping_baselinegroup$`Channel Grouping`, collapse ="|"),
                               scope = "SESSION")

sv_channel <- segment_vector_simple(list(list(se_channelgroup)))
seg_defined <- segment_define(list(sv_channel))
segment_trans_channelgroup <- segment_ga4("top3 channelgrouping", user_segment = seg_defined)

# apply segment to sourcemedium - find top contributing source/mediums
ga_data_sourceMedium_start <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate),
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("sourceMedium"),
                   segments = c(segment_trans_channelgroup),
                   anti_sample = TRUE,
                   max = -1)

ga_data_sourceMedium_end <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate2, endDate2),
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("sourceMedium"),
                   segments = c(segment_trans_channelgroup),
                   anti_sample = TRUE,
                   max = -1)

ga_data_sourceMedium_baselinegroup <- ga_data_sourceMedium_start %>%
  select(sourceMedium, session_start = "sessions", transactions_start = 'transactions') %>%
  left_join(ga_data_sourceMedium_end, by="sourceMedium") %>%
  mutate(sessionstotal = sum(sessions, na.rm=TRUE)) %>%
  group_by(sourceMedium) %>%
  summarise(transactionsPerSessionchg = round(((sum(transactions)/sum(sessions))/(sum(transactions_start)/sum(session_start))-1)*100, 2),
            shareofSessions = round((sum(sessions)/sum(sessionstotal))*100,2)) %>%
  top_n(5, shareofSessions) %>%
  select(`Source / Medium` = 'sourceMedium',
         `% change in CR` = 'transactionsPerSessionchg',
         `Share of Sessions` = 'shareofSessions')


# apply segment to country - find top 5 contributing countries
ga_data_country_start <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate),
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("country"),
                   segments = c(segment_trans_channelgroup),
                   anti_sample = TRUE,
                   max = -1)

ga_data_country_end <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate2, endDate2),
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("country"),
                   segments = c(segment_trans_channelgroup),
                   anti_sample = TRUE,
                   max = -1)

ga_data_country_baselinegroup <- ga_data_country_start %>%
  select(country, session_start = "sessions", transactions_start = 'transactions') %>%
  left_join(ga_data_country_end, by="country") %>%
  mutate(sessionstotal = sum(sessions, na.rm=TRUE)) %>%
  group_by(country) %>%
  summarise(transactionsPerSessionchg = round(((sum(transactions)/sum(sessions))/(sum(transactions_start)/sum(session_start))-1)*100, 2),
            shareofSessions = round((sum(sessions)/sum(sessionstotal))*100,2)) %>%
  top_n(5, shareofSessions) %>%
  select(`Country` = 'country',
         `% change in CR` = 'transactionsPerSessionchg',
         `Share of Sessions` = 'shareofSessions')


# needs refining
# apply segment to campaigns - find top 5 contributing campaigns 
ga_data_campaigns_start <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate),
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("campaign"),
                   segments = c(segment_trans_channelgroup),
                   anti_sample = TRUE,
                   max = -1)

ga_data_campaigns_end <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate2, endDate2),
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("campaign"),
                   segments = c(segment_trans_channelgroup),
                   anti_sample = TRUE,
                   max = -1)

ga_data_campaigns_baselinegroup <- ga_data_campaigns_start %>%
  select(campaign, session_start = "sessions", transactions_start = 'transactions') %>%
  left_join(ga_data_campaigns_end, by="campaign") %>%
  mutate(sessionstotal = sum(sessions, na.rm=TRUE)) %>%
  group_by(campaign) %>%
  summarise(transactionsPerSessionchg = round(((sum(transactions)/sum(sessions))/(sum(transactions_start)/sum(session_start))-1)*100, 2),
            shareofSessions = round((sum(sessions)/sum(sessionstotal))*100,2)) %>%
  filter(campaign != '(not set)') %>%
  top_n(3, shareofSessions)

