library(sqldf)
library(tidyverse)
library(lubridate)
library(reshape2)

raw_data1 <- read.csv(file="CD_table_sample.csv",head=TRUE,sep=",")

raw_data1$flightSearches <- as.numeric(as.character(raw_data1$flightSearches))
raw_data1$flightbookings <- as.numeric(as.character(raw_data1$flightbookings))

raw_data <- raw_data1 %>%
  select(-X) %>%
  mutate(Date = ymd(yyyymmdd)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  select(-Date) %>%
  mutate(yearMonth = paste(Year,Month, sep = "-")) %>%
  mutate(flightSearches = ifelse(is.na(flightSearches),0,flightSearches)) %>%
  mutate(flightbookings = ifelse(is.na(flightbookings),0,flightbookings))
  

raw_data[,5:32] <- lapply(raw_data[, 5:32], gsub, pattern = "null", replacement = NA, fixed = TRUE)

raw_data <- raw_data %>%
  replace_na(list(flightSearches = 0, flightbookings = 0)) %>%
  select(1:4, 33:37, 5:32) %>%
  gather(eventCategory, eventAction, 8:37) %>%
  separate()

raw_data <- raw_data %>%
  filter(grepl("2016", yearMonth, ignore.case = TRUE)) %>%
  select(yearMonth, 5:32) %>%
  gather("EventCategory", "EventAction", -yearMonth) %>%
  group_by(yearMonth, EventCategory, EventAction) %>%
  summarize(count = n())



raw_data <- raw_data %>%
  select(storefront, SearchedOriginCity, SearchedJourneyStartDate) %>%
  group_by(storefront) %>%
  summarise(count = n(), 
            flightSearches = sum(flightSearches),
            flightbookings = sum(flightbookings)) %>%
  spread(storefront, count)
  gather(EventCategory, EventAction, 1:3, -flightSearches, -flightbookings, factor_key= TRUE)
raw_data <- raw_data %>%
  select(1:4, 35:37, 5:34)