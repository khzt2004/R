library(tidyverse)
library(readxl)
library(CausalImpact)
library(lubridate)

ads <- read_excel("C:\\Users\\User\\Documents\\TV_Ads_MayJune.xlsx")

### filter out time where hours greater than 24 first ###
ads <- ads %>% 
  filter(!grepl("^24:|^28:", Time)) %>% 
  mutate(Time = as.numeric(Time) * 24 * 3600) %>% 
  mutate(Time = hms(seconds_to_period(Time))) %>% 
  mutate(Time = sprintf("%s:%s:%s", hour(Time), minute(Time), second(Time))) %>% 
  mutate(Time = format(Time, "%H:%M:%S"))
  