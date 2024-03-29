library(RColorBrewer)
library(tidyverse)
library(viridis)
library(ggthemes)
library(googleAnalyticsR)
library(lubridate)
library(rpivotTable)
library(TTR)
library(googlesheets)
library(quantmod)
library(grid)
library(gridExtra)
library(reshape2)



ga_auth()
## get your accounts
account_list <- ga_account_list()
id <- account_list[13,'viewId']
my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_call_FB <- "gaid::oskyC39UQi6wpgYOLCLMww"
seg_obj_FB <- segment_ga4("Training Campaign_FB", segment_id = segment_for_call_FB)

segment_for_call_Google <- "gaid::rj-Q3pVwTtyja1bubZPp8w"
seg_obj_Google <- segment_ga4("Training Campaign_Google", segment_id = segment_for_call_Google)

segment_for_call_SpamFiltered <- "gaid::1PVeiyEpTWW8guG2ff3rjg"
seg_obj_SpamFiltered <- segment_ga4("googleusercontent network domain", segment_id = segment_for_call_SpamFiltered)

startDate <- "2017-09-02"
endDate <- "2017-09-08"
startDate2 <- "2017-08-26"
endDate2 <- "2017-09-01"

unsampled_data_fetch_FB <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                              metrics = c("users", "sessions", "pageViews", "entrances", "bounceRate", "avgTimeOnPage", "exitRate"), 
                                              dimensions = c("dayOfWeekName", "hour"),
                                              segments = seg_obj_FB)

unsampled_data_fetch_Google <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                                  metrics = c("users", "sessions", "pageViews", "entrances", "bounceRate", "avgTimeOnPage", "exitRate"), 
                                                  dimensions = c("dayOfWeekName", "hour"),
                                                  segments = seg_obj_Google,
                                                  anti_sample = TRUE)

unsampled_data_fetch_campaigns <- google_analytics_4(id, date_range = c(startDate, endDate, startDate2, endDate2), 
                                                     metrics = c("users", "newUsers", "sessions", "bounceRate", "pageviewsPerSession", "entrances", "avgTimeOnPage", "exitRate", "avgSessionDuration", "transactionsPerSession"), 
                                                     dimensions = c("Campaign", "adGroup"),
                                                     segments = c(seg_obj_Google, seg_obj_FB))

unsampled_data_fetch_adwordcampaigns <- google_analytics_4(id, date_range = c(startDate, endDate, startDate2, endDate2), 
                                                     metrics = c("impressions", "adClicks", "adCost", "CTR", "CPC"), 
                                                     dimensions = c("Campaign", "adGroup"))

adwordsCombined <- unsampled_data_fetch_campaigns %>%
  left_join(unsampled_data_fetch_adwordcampaigns, by = "adGroup") %>%
  select(-c(Campaign.x, Campaign.y)) %>%
  filter(segment == "Training Campaign_Google") %>%
  select(-segment) %>%
  gather(metrics, values, 2:31) %>% 
  spread(adGroup, values) %>%
  gather(Campaign, Values, 2:5) %>%
  separate(metrics, c("metric", "dateRange")) %>%
  spread(dateRange, Values)

adwordsCombined[, 3:4] <- round(adwordsCombined[, 3:4], 2)



unsampled_data_fetch_deviceLP <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                                    metrics = c("users", "newUsers", "sessions", "bounceRate", "pageviewsPerSession", "entrances", "avgTimeOnPage", "exitRate", "avgSessionDuration", "transactionsPerSession"), 
                                                    dimensions = c("adGroup", "deviceCategory", "landingPagePath"),
                                                    segments = c(seg_obj_Google, seg_obj_FB))

unsampled_data_fetch_deviceLP <- unsampled_data_fetch_deviceLP %>%
  filter(!grepl("(not set)", adGroup, ignore.case = TRUE))

unsampled_data_fetch_deviceLP$sessionsDelt <- round(Delt(unsampled_data_fetch_deviceLP$sessions.d2, unsampled_data_fetch_deviceLP$sessions.d1), 2)
unsampled_data_fetch_deviceLP$usersDelt <- round(Delt(unsampled_data_fetch_deviceLP$users.d2, unsampled_data_fetch_deviceLP$users.d1), 2)

unsampled_data_fetch_device_AdKeyword <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                                            metrics = c("users", "newUsers", "sessions", "avgTimeOnPage", "bounceRate", "pageviewsPerSession", "exitRate", "goal4Completions", "goal5Completions", "transactionsPerSession"), 
                                                            dimensions = c("deviceCategory", "adContent", "keyword"),
                                                            segments = c(seg_obj_Google, seg_obj_FB),
                                                            anti_sample = TRUE)
# demographics may not be accurate
# unsampled_data_fetch_demographics <- google_analytics_4(id, date_range = c(startDate, endDate), 
#                                                            metrics = c("users", "newUsers", "sessions", "avgTimeOnPage", "bounceRate", "pageviewsPerSession", "avgSessionDuration", "goal4Completions", "goal5Completions", "transactionsPerSession"), 
#                                                            dimensions = c("userAgeBracket", "userGender"),
#                                                            segments = c(seg_obj_Google, seg_obj_FB),
#                                                            anti_sample = TRUE)

# Google heatmap
# google_heatmap <- read.csv("google_heatmap.csv", sep=",")
# google_heatmap1 <- spread(unsampled_data_fetch_Google, dayOfWeekName, Sessions, fill = 0)
google_heatmap <- unsampled_data_fetch_Google
google_heatmap$dayOfWeekName <- factor(google_heatmap$dayOfWeekName, c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
google_heatmap <- rename(google_heatmap, Hour = hour)
google_heatmap$Hour <- as.numeric(google_heatmap$Hour)
rename(df,c('foo'='samples'))
google_heatmap$wDay <- ifelse(google_heatmap$dayOfWeekName %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# use this if summarising by weekend vs weekday
#google_heatmap <- google_heatmap %>%
#  group_by(wDay, Hour) %>%
#  summarise(Sessions = sum(Sessions))

google_heatmap %>%
  ggplot(aes(x=Hour, y=dayOfWeekName, fill=sessions)) +  # change y-axis if summary by weekend vs weekday
  geom_tile(color="white", size=0.1) +
  scale_fill_gradient(low = "#f6f6f6", high = "#3367d6") + 
  # scale_fill_viridis(name="Sessions", option = "C") +
  scale_x_continuous(breaks = c(0:23), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(y = "Day of Week", title="Adwords - Sessions by Hour and Day of Week") +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_text(size=9)) + 
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=6))
# annotate("rect", xmin=-0.5, xmax=8, ymin=-Inf, ymax=Inf, alpha=.2, fill="blue")


# FB heatmap
# fb_heatmap <- read.csv("fb_heatmap.csv", sep=",")
# fb_heatmap1 <- spread(fb_heatmap, Day.of.Week.Name, Sessions, fill = 0)
fb_heatmap <- unsampled_data_fetch_FB
fb_heatmap$dayofWeekName <- factor(fb_heatmap$dayOfWeekName, c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
fb_heatmap <- rename(fb_heatmap, Hour = hour)
fb_heatmap$Hour <- as.numeric(fb_heatmap$Hour)
fb_heatmap$wDay <- ifelse(fb_heatmap$dayOfWeekName %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# use this if summarising by weekend vs weekday
#fb_heatmap <- fb_heatmap %>%
#  group_by(wDay, Hour) %>%
#  summarise(Sessions = sum(Sessions))

fb_heatmap %>%
  ggplot(aes(x=Hour, y=dayOfWeekName, fill=sessions)) +  # change y-axis if summary by weekend vs weekday
  geom_tile(color="white", size=0.1) +
  scale_fill_gradient(low = "#f6f6f6", high = "#3367d6") +
  # scale_fill_viridis(name="Sessions") +
  scale_x_continuous(breaks = c(0:23), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(y = "Day of Week", title="Facebook - Sessions by Hour and Day of Week") +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_text(size=9)) + 
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=6))
# annotate("rect", xmin=-0.5, xmax=8, ymin=-Inf, ymax=Inf, alpha=.2, fill="blue")

# create google sheets

write_csv(unsampled_data_fetch_device_AdKeyword, "unsampled_data_fetch_device_AdKeyword.csv")
keyword_ss <- gs_upload("unsampled_data_fetch_device_AdKeyword.csv")
keyword_ss %>% gs_read()
file.remove("unsampled_data_fetch_device_AdKeyword.csv")

write_csv(unsampled_data_fetch_deviceLP, "unsampled_data_fetch_deviceLP.csv")
deviceLP_ss <- gs_upload("unsampled_data_fetch_deviceLP.csv")
deviceLP_ss %>% gs_read()
file.remove("unsampled_data_fetch_deviceLP.csv")

write_csv(adwordsCombined, "adwordsCombined.csv")
adwordsCombined_ss <- gs_upload("adwordsCombined.csv")
adwordsCombined_ss %>% gs_read()
file.remove("adwordsCombined.csv")