library(googleAnalyticsR)
library(tidyverse)
library(corrplot)
ga_auth()

# Pull a full list of the views that you have access to
my_accounts <- ga_account_list()

# Change this to your own view ID
my_id <- 122507877 

# Now, query for some basic data, assigning the data to a 'data frame' object 
# called 'web_data'
web_data <- google_analytics_4(my_id, 
                               date_range = c("2017-01-01", "2017-07-01"),
                               metrics = c("sessions","pageviews",
                                           "entrances","bounces"),
                               dimensions = c("date","deviceCategory",
                                              "channelGrouping"),
                               anti_sample = TRUE)

# use this to export data from GA to csv or read csv data

# write.csv(web_data, file = "APS2017_GA_Data.csv")
# web_data_test <- read.csv("APS2017_GA_Data.csv")
# web_data_test <- web_data_test %>%
#   select(2:8)

## use tidyverse to pivot the data
library(dplyr)
library(tidyr)

## get only desktop rows, and the date, channelGrouping and sessions columns
pivoted <- web_data %>% 
  filter(deviceCategory == "desktop") %>% 
  select(date, channelGrouping, sessions) %>%
  spread(channelGrouping, sessions)

## get rid of any NA's and replace with 0
pivoted[is.na(pivoted)] <- 0

## make a time-series object
web_data_ts <- ts(pivoted[-1], frequency = 7)

## time-series are set up to have useful plots
plot(web_data_ts, axes = FALSE)

decomp <- decompose(web_data_ts[, "Organic Search"])
plot(decomp)

library(forecast)
## performs decomposition and smoothing
fit <- ets(web_data_ts[, "Organic Search"])
## makes the forecast
fc <- forecast(fit)
plot(fc)

fit2 <- HoltWinters(web_data_ts[, "Organic Search"])
## makes the forecast
fc2 <- forecast(fit2, h = 25)
plot(fc2)

library(xts)

## create a time-series zoo object
web_data_xts <- xts(pivoted[-1], order.by = as.Date(pivoted$date), frequency = 7)

library(CausalImpact)
pre.period <- as.Date(c("2016-02-01","2016-05-14"))
post.period <- as.Date(c("2016-05-15","2016-07-15"))

## data in order of response, predictor1, predictor2, etc.
model_data <- web_data_xts[,c("Video","Social","Direct")]


impact <- CausalImpact(model_data,  pre.period, post.period)
plot(impact)
