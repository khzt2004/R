library(tidyverse)
library(readxl)
library(CausalImpact)
library(lubridate)
library(chron)
library(janitor)
library(xts)
library(tibbletime)
library(padr)

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
  mutate(date_time = as.POSIXct(date_time)) %>% 
  pad("hour", by = "date_time") %>% 
  fill_by_value(sum_byhour, count_byhour, value = 0) 
  
  
ggplot(ads, aes(x = as.Date(date_time), y = count)) + geom_line()

df_ts <- xts(x = ads, order.by = ads$date_time)

patroli <- ads %>% 
  filter(program_name == "PATROLI") %>% 
  group_by_all() %>% 
  count(version)

ggplot(patroli, 
       aes(x = as.Date(date_time), y = n)) +
  geom_line() +
  scale_x_date(date_labels ="%Y-%m-%d", date_breaks ="1 day")



