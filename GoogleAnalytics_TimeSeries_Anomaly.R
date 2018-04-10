library(googleAnalyticsR)
library(anomalize)
library(tidyverse)
library(lubridate)

#  http://www.business-science.io/code-tools/2018/04/08/introducing-anomalize.html

ga_auth(new_user = TRUE)

account_list <- ga_account_list()

## the ViewIds to fetch all at once
gaids <- c(account_list[1505,'viewId'])

# selecting segments
my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

my_fetch <- google_analytics(gaids,
                             date_range = c("2017-12-01","yesterday"), 
                             metrics = c("transactions"), 
                             dimensions = c("date", "deviceCategory"),
                             segments = c(seg_allUsers),
                             anti_sample = TRUE,
                             max = -1)

my_data <- as.tibble(my_fetch)

my_data <- my_data %>%
  select(date, transactions, deviceCategory) %>%
  mutate(castedDate = as.POSIXct(date, format="%Y-%m-%d", tz="SGT")) %>%
  select(castedDate, transactions, deviceCategory) %>%
  group_by(deviceCategory)


my_data %>%  time_decompose(transactions) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

# Time Series Decomposition
my_data %>%
  time_decompose(transactions, method = "stl", frequency = "auto", trend = "auto")

# Anomaly Detection Of Remainder
my_data %>%
  time_decompose(transactions, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2)

my_data %>%
  
  # Select a single time series
  filter(deviceCategory == "desktop") %>%
  ungroup() %>%
  
  # Anomalize
  time_decompose(transactions, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  
  # Plot Anomaly Decomposition
  plot_anomaly_decomposition() +
  ggtitle("Lubridate Downloads: Anomaly Decomposition")

# Anomaly Lower and Upper Bounds
my_data %>%
  time_decompose(transactions, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose()

my_data %>%
  # Select single time series
  filter(deviceCategory == "mobile") %>%
  ungroup() %>%
  # Anomalize
  time_decompose(transactions, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  # Plot Anomaly Decomposition
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("Data: Anomalies Detected")





