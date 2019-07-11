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
library(googleAnalyticsR)
library(future.apply)

ads <- read_excel("C:\\Users\\User\\Documents\\TV_Ads_MayJune.xlsx")
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


#### Get data from Google Analytics #####

# login as new_user = TRUE if switching accounts. Otherwise do not set new_user = true
ga_auth()
# ga_auth(new_user = TRUE)


# get account list -------------------------------------
# account_list <- ga_account_list()
# 
# gaids <- c(account_list[604,'viewId'])
# 
# 
# data <- google_analytics(gaids,
#                    date_range = c("2018-01-01","2018-06-01"),
#                    metrics = c("sessions"),
#                    dimensions = c("date", "hour", "minute", "channelGrouping"),
#                    # anti_sample = TRUE,
#                    max = -1,
#                    useResourceQuotas = TRUE)


### Feed in csv from unsampled report in GA ####
### Next steps: get data broken down by device category ###

sessions_h1 <- read_csv("Sessions by Date - Jan - Jun 2018.csv")

sessions_h2 <- read_csv("Sessions by Date - Jul - Dec 2018.csv")

sessions_h1_2019 <- read_csv("Sessions by Date - Jan - Jun 2019.csv")

sessions_2018_2019 <- rbind(sessions_h1, 
                            sessions_h2,
                            sessions_h1_2019) %>% 
  clean_names()

rm(sessions_h1, sessions_h2, sessions_h1_2019)

GA_org_direct_sessions <- sessions_2018_2019 %>% 
  filter(default_channel_grouping == 'Organic Search' |
           default_channel_grouping == 'Direct') %>% 
  mutate(date = ymd(date),
         time = (paste(hour, minute, sep=" ")),
         seconds = "00") %>% 
  mutate(date_time =ymd_hms(paste(date, paste(hour, minute, seconds, sep=":")),  tz="Asia/Jakarta"))


# collapse the graph by week / month ie a higher dimension 
GA_org_direct_sessions %>%
  arrange(date_time) %>% 
  as_tbl_time(index = date_time) %>% 
  collapse_by("weekly") %>% 
  ggplot(aes(x = date_time, y = sessions)) + 
  geom_line() +
  facet_wrap(vars(default_channel_grouping), nrow = 2)








