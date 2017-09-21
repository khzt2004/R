library(sqldf)
library(tidyverse)
library(lubridate)
library(reshape2)

raw_data1 <- read.csv(file="CD_table_sample.csv",head=TRUE,sep=",")


raw_data1$flightSearches <- as.numeric(as.character(raw_data1$flightSearches))
raw_data1$flightbookings <- as.numeric(as.character(raw_data1$flightbookings))

# performs count of eventCategory and flightsearch/flightbooking grouped by month
raw_data <- raw_data1 %>%
  select(-X) %>%
  mutate(Date = ymd(yyyymmdd)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  select(-Date) %>%
  mutate(yearMonth = paste(Year,Month, sep = "-")) # %>%
  # don't substitute with zero if checking for correlation
  # at event category level only
  #mutate(flightSearches = ifelse(is.na(flightSearches),0,flightSearches)) %>%
  #mutate(flightbookings = ifelse(is.na(flightbookings),0,flightbookings))

raw_data[,5:32] <- lapply(raw_data[, 5:32], gsub, pattern = "null", replacement = NA, fixed = TRUE)
raw_data[,5:32][!is.na(raw_data[,5:32])] <- 1
raw_data[,5:34][is.na(raw_data[,5:34])] <- 0
raw_data[,5:34] <- sapply( raw_data[,5:34], as.numeric )

# group by month, summary count of event category
raw_data <- raw_data %>%
  filter(grepl("2016|2017", yearMonth, ignore.case = TRUE)) %>%
  select(yearMonth, 5:34) %>%
  group_by(yearMonth) %>%
  summarise_all(function(x) sum(!is.na(x)))

# generate dataframe with count of event category/event action, flightsearch/bookings
# group by month
raw_data <- raw_data1 %>%
  select(-X) %>%
  mutate(Date = ymd(yyyymmdd)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  select(-Date) %>%
  mutate(yearMonth = paste(Year,Month, sep = "-"))

raw_data[,5:32] <- lapply(raw_data[, 5:32], gsub, pattern = "null", replacement = NA, fixed = TRUE)

raw_data <- raw_data %>%
  filter(grepl("2016|2017", yearMonth, ignore.case = TRUE)) %>%
  select(yearMonth, 5:34) %>%
  # or gather flight first
  #gather()
  group_by(yearMonth) %>%
  gather("EventCategory", "EventAction", -yearMonth) %>%
  # must summarize flight events 
  group_by(yearMonth, EventCategory, EventAction) %>%
  summarize(EventCategory_count = sum(!is.na(EventCategory)),
            EventAction_count = sum(!is.na(EventAction)))
  
# spread an event category out for deep-dive analysis
raw_data <- raw_data1 %>%
  select(-X) %>%
  mutate(Date = ymd(yyyymmdd)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  select(-Date) %>%
  mutate(yearMonth = paste(Year,Month, sep = "-"))

raw_data[,5:32] <- lapply(raw_data[, 5:32], gsub, pattern = "null", replacement = NA, fixed = TRUE)

raw_data <- raw_data %>%
  filter(grepl("2016|2017", yearMonth, ignore.case = TRUE)) %>%
  select(yearMonth, SearchedJourneyLeadDays, flightSearches, flightbookings) %>%
  group_by(yearMonth, SearchedJourneyLeadDays) %>%
  summarize(SearchedJourneyLeadDays_count = sum(!is.na(SearchedJourneyLeadDays)),
            flightSearches = sum(!is.na(flightSearches)),
            flightbookings = sum(!is.na(flightbookings))) %>%
  spread(SearchedJourneyLeadDays, SearchedJourneyLeadDays_count)
# convert NA to zero for running correlations
raw_data[,4:ncol(raw_data)][is.na(raw_data[,4:ncol(raw_data)])] <- 0





summarize_if(is.na(), function(x) sum(!is.na(x)) )
cor(raw_data$`126`, raw_data$`139`, use = "complete.obs")
write_csv(raw_data, "HVA_raw.csv")

raw_data <- raw_data %>%
  select(storefront, SearchedOriginCity, SearchedJourneyStartDate) %>%
  group_by(storefront) %>%
  summarise(count = n()) %>%
  flightSearches = sum(flightSearches) %>%
  flightbookings = sum(flightbookings) %>%
  spread(storefront, count) %>%
  gather(EventCategory, EventAction, 1:3, -flightSearches, -flightbookings, factor_key= TRUE)
