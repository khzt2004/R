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
library(corrplot)



ga_auth()
## get your accounts
account_list <- ga_account_list()
id <- account_list[13,'viewId']
my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_call_SpamFiltered <- "gaid::1PVeiyEpTWW8guG2ff3rjg"
seg_obj_SpamFiltered <- segment_ga4("googleusercontent network domain", segment_id = segment_for_call_SpamFiltered)

startDate <- "2016-01-01"
endDate <- "2017-09-01"
startDate2 <- "2017-08-19"
endDate2 <- "2017-08-25"

web_data <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                              metrics = c("users", "sessions", "pageViews", "entrances", "bounces", "avgTimeOnPage", "exitRate"), 
                                              dimensions = c("date", "deviceCategory", "channelGrouping"),
                                              segments = seg_obj_SpamFiltered,
                                           anti_sample = TRUE)

web_data_metrics <- web_data[,c("sessions","pageviews","entrances","bounces")]
cor(web_data_metrics)
pairs(web_data_metrics)

## Get only desktop rows, and the date, channelGrouping and sessions columns
pivoted <- web_data %>% 
  filter(deviceCategory == "desktop") %>% 
  select(date, channelGrouping, sessions) %>%
  spread(channelGrouping, sessions)

## Get rid of any NA's and replace with 0
pivoted[is.na(pivoted)] <- 0

## can't include the date as its not numeric, so remove first column. -1 subset
cor_data <- pivoted[, -1]
## round the correlation values to 2 dp
cor_table <- round(cor(cor_data),2)


cor_table %>%
  corrplot(mar = c(2, 0, 1, 0), method = "color", order = "hclust", tl.srt=45, tl.cex=0.75)

# correlation plots to find interesting relationships
# paid vs organic search
gg <- ggplot(data = pivoted) + 
  theme_minimal() + 
  ggtitle("Paid (blue) vs Organic (green) search")
gg <- gg + 
  geom_line(aes(x = as.Date(date), y = `Generic Paid Search`), col = "blue")

gg + geom_line(aes(x = as.Date(date), y = `Organic Search`), col = "green")

# social vs referral 

gg <- ggplot(data = pivoted) + 
  theme_minimal() + 
  ggtitle("Social (red) vs Referral (orange)")
gg <- gg + 
  geom_line(aes(x = as.Date(date), y = Referral), col = "red")
gg + geom_line(aes(x = as.Date(date), y = Email), col = "orange")

# cross correlation - social vs email
ccf(pivoted$Social, pivoted$Email)

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