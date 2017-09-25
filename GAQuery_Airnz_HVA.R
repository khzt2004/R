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


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()
id <- account_list[172,'viewId']
#my_segments <- ga_segment_list()
#segs <- my_segments$items


startDate <- "2016-08-01"
endDate <- "2017-08-31"
startDate2 <- "2017-08-26"
endDate2 <- "2017-09-01"

sourcefile <- load("GAevents_list_airnz.RData")
events_list_airnz1 <- google_analytics_4(id, date_range = c(startDate, endDate), 
                                              metrics = c("sessions", "hits", "pageViews"), 
                                              dimensions = c("date", "EventCategory", "EventAction"),
                                        anti_sample = TRUE)

events_list_airnz1 <- events_list_airnz %>%
  #filter(grepl("2016|2017", yearMonth, ignore.case = TRUE))
  mutate(Date = ymd(date)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  mutate(yearMonth = paste(Year,Month, sep = "-")) %>%
  select(-date, -Year, -Month, -sessions, -pageViews) %>%
  group_by(Date, yearMonth, EventCategory, EventAction) %>%
  #filter(grepl("ecommerce|chatbot|mast head|cta|Scroll Depth|footer menu|youtube|loyalty|videoNebulaCX|flight booking|social|airnz|fare finder|sign in|tab panel|apps download|deal pages|navigation|form field", 
  #             EventCategory, ignore.case = TRUE))

  
  

# export dataframes to csv
write_csv(events_list_airnz1, "events_list_airnz1.csv")
save(events_list_airnz, file = "GAevents_list_airnz.RData")


