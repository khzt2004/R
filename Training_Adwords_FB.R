library(RColorBrewer)
library(tidyverse)
library(viridis)
library(ggthemes)
library(googleAnalyticsR)
library(lubridate)
library(rpivotTable)
library(TTR)
library(googlesheets)


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

startDate <- "2017-08-19"
endDate <- "2017-08-25"

unsampled_data_fetch_FB <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                           metrics = c("sessions", "pageViews", "entrances", "bounceRate", "avgTimeOnPage", "exitRate"), 
                                           dimensions = c("dayOfWeekName", "hour"),
                                           segments = seg_obj_FB,
                                           anti_sample = TRUE)

unsampled_data_fetch_Google <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                              metrics = c("sessions", "pageViews", "entrances", "bounceRate", "avgTimeOnPage", "exitRate"), 
                                              dimensions = c("dayOfWeekName", "hour"),
                                              segments = seg_obj_Google,
                                              anti_sample = TRUE)

unsampled_data_fetch_campaigns <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                                  metrics = c("users", "newUsers", "sessions", "bounceRate", "pageviewsPerSession", "entrances", "avgTimeOnPage", "exitRate", "avgSessionDuration", "transactionsPerSession"), 
                                                  dimensions = c("Campaign"),
                                                  segments = c(seg_obj_SpamFiltered, seg_obj_Google, seg_obj_FB),
                                                  anti_sample = TRUE)

unsampled_data_fetch_deviceLP <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                                     metrics = c("users", "newUsers", "sessions", "avgTimeOnPage", "bounceRate", "pageviewsPerSession", "exitRate", "goal4Completions", "goal5Completions", "transactionsPerSession"), 
                                                     dimensions = c("deviceCategory", "landingPagePath"),
                                                     segments = c(seg_obj_Google, seg_obj_FB),
                                                     anti_sample = TRUE)

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

write_csv(fb_heatmap, "fb_heatmap.csv")
training_ss <- gs_upload("fb_heatmap.csv")
training_ss %>% gs_read()
file.remove("fb_heatmap.csv")