library(sqldf)
library(tidyverse)
library(lubridate)
library(reshape2)
library(bigrquery)

# raw_data1 <- read.csv(file="CD_table_sample.csv",head=TRUE,sep=",")
# raw_data1 <- read.csv(file="HVA_raw.csv",head=TRUE,sep=",")
sourcefile <- load("HVA_raw_rdata.RData")
raw_data1$flightSearches <- as.numeric(as.character(raw_data1$flightSearches))
raw_data1$flightbookings <- as.numeric(as.character(raw_data1$flightbookings))

# Big Query Setting
project <- "airnz-ga-bigquery"
dataset <- "hva_analysis"

sql <- paste0("SELECT
sid,
yyyymmdd,
ecommerceevents,
chatbotEvents,
mastheadEvents,
CTAEvents,
scrolldepthEvents,
footermenuEvents,
youtubeEvents,
loyalty,
Loyalty_new,
videoEvents,
NebulaCXEvents,
flightbookingEvents,
socialmediashares,
airnzhotels,
farefinder,
signin,
homepagetabpanel,
appsdownload,
dealpages,
dealNavigation,
formfield,
flightSearches,
flightbookings
FROM
[airnz-ga-bigquery:hva_analysis.event_base_table]
WHERE yyyymmdd = '2017-01-25'
")

raw_data1 <- query_exec(sql, project = project, destination_table = NULL, max_pages = Inf)

# performs count of eventCategory and flightsearch/flightbooking grouped by month
raw_data <- raw_data1 %>%
  #select(-X) %>%
  mutate(Date = ymd(yyyymmdd)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  select(-Date) %>%
  mutate(yearMonth = paste(Year,Month, sep = "-")) # %>%
  # don't substitute with zero if checking for correlation
  # at event category level only
  #mutate(flightSearches = ifelse(is.na(flightSearches),0,flightSearches)) %>%
  #mutate(flightbookings = ifelse(is.na(flightbookings),0,flightbookings))

raw_data[,3:23] <- lapply(raw_data[, 3:23], gsub, pattern = "null", replacement = NA, fixed = TRUE)
raw_data[,3:23][!is.na(raw_data[,3:23])] <- 1
raw_data[,3:25][is.na(raw_data[,3:25])] <- 0
raw_data[,3:25] <- sapply( raw_data[,3:25], as.numeric )

# compute correlation: event category vs flightsearches/bookings
coff_df <- data.frame(cor(raw_data[,5:6], raw_data[c("flightSearches", "flightbookings")], use = "everything"))

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
coff_df_deepdive <- data.frame(cor(raw_data[,4:ncol(raw_data)], raw_data[c("flightSearches", "flightbookings")], use = "complete.obs"))



# export dataframes to csv
write_csv(raw_data1, "HVA_raw.csv")
save(raw_data1, file = "HVA_raw_rdata.RData")

summarize_if(is.na(), function(x) sum(!is.na(x)) )
raw_data <- raw_data %>%
  select(storefront, SearchedOriginCity, SearchedJourneyStartDate) %>%
  group_by(storefront) %>%
  summarise(count = n()) %>%
  flightSearches = sum(flightSearches) %>%
  flightbookings = sum(flightbookings) %>%
  spread(storefront, count) %>%
  gather(EventCategory, EventAction, 1:3, -flightSearches, -flightbookings, factor_key= TRUE)
