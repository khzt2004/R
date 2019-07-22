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
library(scales)
library(tidyquant)

ads <- read_excel("C:\\Users\\User\\Documents\\TV_Ads_MayJune.xlsx")
ads <- read_excel("TV_Ads_MayJune.xlsx")

##### filter out time where hours greater than 24 first #####
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


##### explore data by hour - this will determine the intervention hour #####
ads_hour <- ads %>%
  group_by(hour = lubridate::hour(date_time2), program_name) %>% 
  summarise(sum_byhour = sum(sum_byhour))

### ggplotly for interactive plot by hour
ggplot(ads_hour, aes(x = hour, y = sum_byhour)) + 
  geom_line() +
  facet_wrap(vars(program_name), ncol = 3)

p <- ggplotly(ggplot(ads_hour, aes(x = hour, y = sum_byhour)) + 
                geom_line() +
                facet_wrap(vars(program_name), ncol = 3))

p

##### explore data by time - this will determine the frequency of ads over time #####
ads_timeseries <- ads %>%
  replace_na(list(sum_byhour=0)) %>%
  filter(!is.na(date_time2)) %>% 
  arrange(date_time2) %>% 
  as_tbl_time(index = date_time2) %>% 
  collapse_by("hourly") %>% 
  group_by(date_time2, program_name) %>% 
  summarise(sum_byhour = sum(sum_byhour))

ads_timeseries %>% 
  ggplot(aes(x = date_time2, y = sum_byhour)) + 
  geom_line(size=1) +
  facet_wrap(vars(program_name)) +
  theme_bw()

##### ggplotly for interactive plot by date #####
ggplot(ads_timeseries, aes(x = date_time2, y = sum_byhour)) + 
  geom_line() +
  facet_wrap(vars(program_name))

tp <- ggplotly(ggplot(ads_timeseries, aes(x = date_time2, y = sum_byhour)) + 
                geom_line() +
                 facet_wrap(vars(program_name)))

tp

##### Investigate ShopeeLiga Match Ads #####
ads_timeseries_shopeeliga <- ads_timeseries %>% 
  filter(program_name == 'SHOPEELIGA1(BIGMATCH)' & date_time2 < '2019-05-19 00:00:00') %>% 
  ungroup() %>% 
  mutate(date_time2 = as.POSIXct(date_time2))

ads_timeseries_shopeeliga %>% 
  ggplot(aes(date_time2, sum_byhour, group=1)) + 
  geom_col(colour="darkblue")

plotly_liga <- ggplotly(ggplot(ads_timeseries_shopeeliga, 
                               aes(x = date_time2, y = sum_byhour)) + 
                          geom_col(colour="darkblue") )

plotly_liga



##### Feed in csv from unsampled report in GA #####
### Next steps: get data broken down by device category ###

# sessions_h1 <- read_csv("Sessions by Date - Jan - Jun 2018.csv")
# 
# sessions_h2 <- read_csv("Sessions by Date - Jul - Dec 2018.csv")
# 
# sessions_h1_2019 <- read_csv("Sessions by Date - Jan - Jun 2019.csv")
# 
# sessions_2018_2019 <- rbind(sessions_h1, 
#                             sessions_h2,
#                             sessions_h1_2019) %>% 
#   clean_names()
# 
# rm(sessions_h1, sessions_h2, sessions_h1_2019)

### Feed in csv table export from BigQuery ###
# sessions_2018_2019 <- read_csv("BQ_sessions_by_date_tablet.csv")
# sessions_2018_2019 <- sessions_2018_2019 %>% 
#   clean_names() %>% 
#   replace_na(list(visits=0))


##### Feed in csv table export for Q2 2019 (tv_attribution_set_2019) from BigQuery #####
# source: BQ Query SQL shown below, change date to between Apr & Jun 2019
# run query, save as table and then export it to Google Cloud Storage as csv
# From Google Cloud Storage, download csv files into local directory and read csv

sessions_q2_2019_p1 <- read_csv("tv_attribution000000000000.csv")

sessions_q2_2019_p2 <- read_csv("tv_attribution000000000001.csv")

sessions_q2_2019_p3 <- read_csv("tv_attribution000000000002.csv")

sessions_q2_2019 <- rbind(sessions_q2_2019_p1,
                            sessions_q2_2019_p2,
                            sessions_q2_2019_p3)

rm(sessions_q2_2019_p1, sessions_q2_2019_p2, sessions_q2_2019_p3)

sessions_q2_2019 <- sessions_q2_2019 %>% 
  clean_names() %>% 
  replace_na(list(visits=0))


##### BQ query #####
# select 
# date,
# deviceCategory,
# channelGrouping,
# hour,
# minute,
# sum(visits) visits 
# from (
#   SELECT
#   date,
#   visitStartTime,
#   device.deviceCategory deviceCategory,
#   EXTRACT(HOUR
#           FROM
#           TIMESTAMP_SECONDS(visitStartTime) AT TIME ZONE 'Asia/Jakarta') hour,
#   EXTRACT(minute
#           FROM
#           TIMESTAMP_SECONDS(visitStartTime) AT TIME ZONE 'Asia/Jakarta') minute,
#   channelGrouping,
#   SUM(totals.visits) Visits
#   FROM
#   `analisis-production.88939979.ga_sessions_*`
#   WHERE
#   --   _TABLE_SUFFIX BETWEEN '20190701'
#   --   AND '20190710'
#   _TABLE_SUFFIX BETWEEN '20180101'
#   AND '20190630'
#   and regexp_contains(channelGrouping, 'Direct|Organic Search')
#   GROUP BY
#   date,
#   visitStartTime,
#   deviceCategory,
#   hour,
#   minute,
#   channelGrouping
# )
# GROUP BY
# date,
# visitStartTime,
# deviceCategory,
# hour,
# minute,
# channelGrouping


GA_org_direct_sessions <- sessions_q2_2019 %>% 
  filter(channel_grouping == 'Organic Search' |
           channel_grouping == 'Direct') %>% 
  mutate(date = ymd(date),
         time = (paste(hour, minute, sep=" ")),
         seconds = "00") %>% 
  mutate(date_time =ymd_hms(paste(date, paste(hour, minute, seconds, sep=":")),  tz="Asia/Jakarta"))


##### collapse the graph by week / month ie a higher dimension #####
GA_org_direct_sessions_weekly <- GA_org_direct_sessions %>%
  arrange(date_time) %>% 
  as_tbl_time(index = date_time) %>% 
  collapse_by("hourly") %>% 
  group_by(date_time, device_category, channel_grouping) %>% 
  summarise(visits = sum(visits))

GA_org_direct_sessions_weekly %>% 
  ggplot(aes(x = date_time, y = visits)) + 
  geom_line(aes(colour = channel_grouping), size=1) +
  facet_grid(device_category ~ channel_grouping) +
  theme_bw()


##### Determine pre and post event time frames varying post period timepre.period  #####
pre.period <- as.POSIXct(c("2019-04-01 00:00:00","2019-06-26 19:00:00"), tz = "Asia/Jakarta")
post.period <- as.POSIXct(c("2019-06-26 21:30:00","2019-07-02 00:00:00"), tz = "Asia/Jakarta")

GA_org_direct_sessions_visits <- GA_org_direct_sessions_weekly %>%
  filter(device_category == 'mobile' & channel_grouping == 'Organic Search') %>% 
  group_by(date_time) %>% 
  summarise(visits = sum(visits))

GA_org_direct_sessions_visits <- xts(GA_org_direct_sessions_visits[-1],
                                     order.by = GA_org_direct_sessions_visits$date_time)
                          
                                     
ad_impact_model = CausalImpact(GA_org_direct_sessions_visits, pre.period, post.period)

# to get the p-value: ad_impact_model$summary$p[1]

summary(ad_impact_model)
summary(ad_impact_model, "report")





