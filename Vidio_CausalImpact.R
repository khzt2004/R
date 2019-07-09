library(tidyverse)
library(readxl)
library(CausalImpact)
library(lubridate)
library(chron)
library(janitor)
library(xts)
library(tibbletime)
library(padr)
library(plotly)

# ads <- read_excel("C:\\Users\\User\\Documents\\TV_Ads_MayJune.xlsx")
ads <- read_excel("TV_Ads_MayJune.xlsx")

### filter out time where hours greater than 24 first ###
ads <- ads %>% 
  filter(!grepl("^24:|^28:", Time)) %>% 
  mutate(Date = as.Date(Date),
         Time = as.numeric(Time)) %>% 
  mutate(Time = times(Time)) %>% 
  mutate(date_time = ymd_hms(paste(Date, Time, sep=" "), tz="Asia/Jakarta")) %>% 
  clean_names() %>% 
  mutate(date_time2 = date_time) %>% 
  as_tbl_time(index = date_time) %>% 
  collapse_by("hourly", side="start", clean=TRUE) %>% 
  group_by_all() %>%
  summarise(sum_byhour = sum(!is.na(version), na.rm=TRUE),
         count_byhour = n()) %>% 
  ungroup() %>%
  pad("hour", by = "date_time") %>% 
  fill_by_value(sum_byhour, count_byhour, value = 0) 
  
  
ggplot(ads, aes(x = as.Date(date_time), y = sum_byhour)) + geom_line()


# explore data by hour - this will determine the intervention hour
ads_hour <- ads %>%
  group_by(hour = lubridate::hour(date_time2), program_name) %>% 
  summarise(sum_byhour = sum(sum_byhour))

# ggplotly for interactive plot by hour
ggplot(ads_hour, aes(x = hour, y = sum_byhour)) + 
  geom_line() +
  facet_wrap(vars(program_name), ncol = 3)

p <- ggplotly(ggplot(ads_hour, aes(x = hour, y = sum_byhour)) + 
                geom_line() +
                facet_wrap(vars(program_name), ncol = 3))

p

