library(googleAnalyticsR)
library(tidyverse)
library(corrplot)
library(forecast)
library(xts)
library(CausalImpact)

#specify the working directory if unable to authenticate
setwd("D:/ToBeSaved/K6O Documents")
ga_auth(new_user = TRUE)

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


## performs decomposition and smoothing
fit <- ets(web_data_ts[, "Organic Search"])
## makes the forecast
fc <- forecast(fit)
plot(fc)

fit2 <- HoltWinters(web_data_ts[, "Organic Search"])
## makes the forecast
fc2 <- forecast(fit2, h = 25)
plot(fc2)


## create a time-series zoo object
web_data_xts <- xts(pivoted[-1], order.by = as.Date(pivoted$date), frequency = 7)


pre.period <- as.Date(c("2017-01-01","2017-03-14"))
post.period <- as.Date(c("2017-05-15","2017-07-15"))

## data in order of response, predictor1, predictor2, etc.
model_data <- web_data_xts[,c("Email","Social","Direct")]


impact <- CausalImpact(model_data,  pre.period, post.period)
plot(impact)
