library(tidyverse)
library(readxl)
library(CausalImpact)
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
library(lubridate)

ads <- read_excel("C:\\Users\\User\\Documents\\TV_Ads_MayJune.xlsx")
ads <- read_excel("TV_Ads_MayJune.xlsx")

##### filter out time where hours greater than 24 first #####
ads_clean <- ads %>% 
  filter(!grepl("^24:|^28:", Time)) %>% 
  mutate(Date = as.Date(Date),
         Time = as.numeric(Time)) %>% 
  mutate(Time = times(Time)) %>% 
  mutate(date_time = ymd_hms(paste(Date, Time, sep=" "), tz="Asia/Jakarta")) %>% 
  clean_names() %>% 
  mutate(date_time2 = date_time,
         hour_amended = hour(date_time2)) %>% 
  mutate(hourly_bucket_amended = case_when(hour_amended >= 19 & hour_amended <= 23 ~ "Prime Time",
         hour_amended >= 6 & hour_amended <= 12 ~ "Morning",
         hour_amended >= 13 & hour_amended <= 19 ~ "Afternoon",
         hour_amended >= 0 & hour_amended <= 5 ~ "Midnight")) %>% 
  as_tbl_time(index = date_time) %>% 
  collapse_by("hourly", side="start", clean=TRUE) %>% 
  group_by_all() %>%
  summarise(sum_byhour = sum(!is.na(version), na.rm=TRUE),
            count_byhour = n()) %>% 
  ungroup() %>%
  pad("hour", by = "date_time") %>% 
  fill_by_value(sum_byhour, count_byhour, value = 0) 


ggplot(ads_clean, aes(x = as.Date(date_time), y = sum_byhour)) + geom_line()


##### explore data by hour - this will determine the intervention hour #####
ads_hour <- ads_clean %>%
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
ads_timeseries <- ads_clean %>%
  replace_na(list(sum_byhour=0)) %>%
  filter(!is.na(date_time2)) %>% 
  arrange(date_time2) %>% 
  as_tbl_time(index = date_time2) %>% 
  collapse_by("hourly") %>% 
  group_by(date_time2, program_name, content_type, hourly_bucket_amended, hour_amended) %>% 
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

# sessions_q2_2019_p1 <- read_csv("tv_attribution000000000000.csv")
# 
# sessions_q2_2019_p2 <- read_csv("tv_attribution000000000001.csv")
# 
# sessions_q2_2019_p3 <- read_csv("tv_attribution000000000002.csv")
# 
# sessions_q2_2019 <- rbind(sessions_q2_2019_p1,
#                             sessions_q2_2019_p2,
#                             sessions_q2_2019_p3)
# 
# rm(sessions_q2_2019_p1, sessions_q2_2019_p2, sessions_q2_2019_p3)
# 
# sessions_q2_2019 <- sessions_q2_2019 %>% 
#   clean_names() %>% 
#   replace_na(list(visits=0))


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

##### Feed in csv table export from Google Analytics for 2018-2019 Jan-June #####
sessions_q1_2018 <- read_csv("sessions export - Jan 2018 - Mar 2018.csv")
sessions_q2_2018 <- read_csv("sessions export - Apr 2018 - Jun 2018.csv")
sessions_q3_2018 <- read_csv("sessions export - Jul 2018 - Sep 2018.csv")
sessions_q4_2018 <- read_csv("sessions export - Oct 2018 - Dec 2018.csv")
sessions_q1_2019 <- read_csv("sessions export - Jan 2019 - Mar 2019.csv")
sessions_q2_2019 <- read_csv("sessions export - Apr 2019 - Jun 2019.csv")
sessions_jul_2019 <- read_csv("sessions export - Jul 2019.csv")

sessions_combined <- rbind(sessions_q1_2018,
                          sessions_q2_2018,
                          sessions_q3_2018,
                          sessions_q4_2018,
                          sessions_q1_2019,
                          sessions_q2_2019,
                          sessions_jul_2019)

rm(sessions_q1_2018, sessions_q2_2018, sessions_q1_2019, sessions_q2_2019, sessions_q3_2018,
   sessions_q4_2018, sessions_jul_2019)

sessions_combined <- sessions_combined %>% 
  clean_names() %>% 
  replace_na(list(sessions=0))

GA_org_direct_sessions <- sessions_combined %>% 
  filter(default_channel_grouping == 'Organic Search' |
           default_channel_grouping == 'Direct') %>% 
  mutate(date = ymd(date),
         time = (paste(hour, minute, sep=" ")),
         seconds = "00") %>% 
  mutate(date_time =ymd_hms(paste(date, paste(hour, minute, seconds, sep=":")),  tz="Asia/Jakarta"))


##### collapse the graph by week / month ie a higher dimension #####
GA_org_direct_sessions_weekly <- GA_org_direct_sessions %>%
  arrange(date_time) %>% 
  as_tbl_time(index = date_time) %>% 
  collapse_by("hourly") %>% 
  group_by(date_time, device_category, default_channel_grouping) %>% 
  summarise(sessions = sum(sessions))


GA_org_direct_sessions_weekly %>% 
  filter(date_time > as.POSIXct("2019-05-29 01:00:00", tz="Asia/Jakarta")) %>% 
  ggplot(aes(x = date_time, y = sessions)) + 
  geom_line(aes(colour = default_channel_grouping), size=1) +
  facet_grid(device_category ~ default_channel_grouping) +
  theme_bw()

############## CAN BE SKIPPED ##############
##### Determine pre and post event time frames varying post period timepre.period  #####
pre.period <- as.POSIXct(c("2019-01-01 00:00:00","2019-05-15 20:50:00"), tz = "Asia/Jakarta")
post.period <- as.POSIXct(c("2019-05-15 23:00:00","2019-05-22 23:00:00"), tz = "Asia/Jakarta")

GA_org_direct_sessions_visits <- GA_org_direct_sessions_weekly %>%
  filter(device_category == 'desktop' & default_channel_grouping == 'Organic Search') %>% 
  group_by(date_time) %>% 
  summarise(sessions = sum(sessions)) %>% 
  mutate(year = year(date_time))


### build time series from dataframe ###
GA_org_direct_sessions_visits <- xts(GA_org_direct_sessions_visits[,2],
                                     order.by = GA_org_direct_sessions_visits$date_time)
                          

### build causal impact model from time series ###
ad_impact_model = CausalImpact(GA_org_direct_sessions_visits, 
                               pre.period,
                               post.period,
                               model.args = list(nseasons = 7, season.duration = 24))

# to get the p-value: ad_impact_model$summary$p[1]

summary(ad_impact_model)
plot(ad_impact_model)
summary(ad_impact_model, "report")

ad_model_response <- ad_impact_model[["series"]]$response
ad_model_response <- fortify.zoo(ad_model_response, name = "Date")

ad_model_predict <- ad_impact_model[["series"]]$point.pred
ad_model_predict <- fortify.zoo(ad_model_predict, name = "Date")

ad_model_predict_response <- cbind(ad_model_response, ad_model_predict)
colnames(ad_model_predict_response) <- c("Date", 
                                         "ad_model_response",
                                         "Date2",
                                         "ad_model_predict")
ad_model_predict_response <- ad_model_predict_response %>% 
  select(-Date2) %>% 
  filter(Date > "2019-05-15 23:00:00" &
         Date < "2019-05-22 23:00:00")

ad_model_predict_response %>% 
  ggplot(aes(x = Date, y = ad_model_response)) + 
  geom_line(aes(y = ad_model_response)) +
  geom_line(aes(y = ad_model_predict)) +
  geom_ribbon(data=subset(ad_model_predict_response, 
                          "2019-05-15 17:59:00" <= Date & 
                            Date <= "2019-05-16 01:00:00"), 
              aes(ymin= ad_model_response,ymax=ad_model_predict), 
              fill="blue", 
              alpha="0.5") + 
  theme_bw()

############## CAN BE SKIPPED ##############

#### Function for running causal impact study across multiple ads ####

tv_ad_workings <- read_csv("TV Attribution - Workings_6.csv")
#tv_ad_workings <- head(tv_ad_workings, 3)

eval_causal_Impact <- function(device_cat, 
                               channel_grouping,
                               pre_intervention_start,
                               pre_intervention_end, 
                               post_intervention_start,
                               post_intervention_end) {
  
  pre.period <- as.POSIXct(c(pre_intervention_start, pre_intervention_end), tz = "Asia/Jakarta")
  post.period <- as.POSIXct(c(post_intervention_start, post_intervention_end), tz = "Asia/Jakarta")
  
  GA_org_direct_sessions_visits <- GA_org_direct_sessions_weekly %>%
    filter(device_category == device_cat & default_channel_grouping == channel_grouping) %>% 
    group_by(date_time) %>% 
    summarise(sessions = sum(sessions)) %>% 
    mutate(year = year(date_time))
  
  
  ### build time series from dataframe ###
  GA_org_direct_sessions_visits <- xts(GA_org_direct_sessions_visits[,2],
                                       order.by = GA_org_direct_sessions_visits$date_time)
  
  
  ### build causal impact model from time series ###
  ad_impact_model = CausalImpact(GA_org_direct_sessions_visits, 
                                 pre.period, 
                                 post.period,
                                 model.args = list(nseasons = 7, season.duration = 24))
  
  
 return(list(p_value = list(ad_impact_model$summary$p[1]), 
        expected = list(ad_impact_model$summary$Actual[1]), 
        predicted = list(ad_impact_model$summary$Pred[1]),
        relative_effect = list(ad_impact_model$summary$RelEffect[1]),
        relative_effect_stddev_pct = list(ad_impact_model$summary$RelEffect.sd[1])))
  
}


tv_ad_workings_causalimpact <- tv_ad_workings %>%
  clean_names() %>% 
  mutate(device_category_web_traffic = tolower(device_category_web_traffic))
  

tv_ad_workings_causalimpact <- tv_ad_workings_causalimpact %>% 
  separate(pre_intervention_period, c("pre_intervention_start", "pre_intervention_end"), ",") %>% 
  separate(post_intervention_period, c("post_intervention_start", "post_intervention_end"), ",")



test_output <- list()

start_time <- Sys.time()
for (i in 1:nrow(tv_ad_workings_causalimpact)) {
modeloutput <- eval_causal_Impact(tv_ad_workings_causalimpact$device_category_web_traffic[i],
                              tv_ad_workings_causalimpact$channel_grouping_web_traffic[i],
                              tv_ad_workings_causalimpact$pre_intervention_start[i],
                              tv_ad_workings_causalimpact$pre_intervention_end[i],
                              tv_ad_workings_causalimpact$post_intervention_start[i],
                              tv_ad_workings_causalimpact$post_intervention_end[i])
test_output <- append(test_output, modeloutput)
}

test_output_df <- data.frame(id=names(test_output), values=unlist(test_output))
test_output_df <- test_output_df %>% 
  group_by_at(vars(-values)) %>% 
  mutate(row_id=1:n()) %>% 
  ungroup() %>% 
  spread(key=id, value=values) %>%
  select(-row_id)

end_time <- Sys.time()
end_time - start_time

tv_ad_workings_causalimpact <- tv_ad_workings_causalimpact %>% 
  mutate(p_value = test_output_df$p_value,
         expected_avg_sessions = test_output_df$expected,
         predicted_avg_sessions = test_output_df$predicted,
         effect = test_output_df$relative_effect,
         standard_deviation = test_output_df$relative_effect_stddev_pct) %>% 
  mutate(p_value = as.numeric(p_value)) 

write_csv(tv_ad_workings_causalimpact, "tv_ad_workings_causalimpact_6.csv")

### import csv of results
total_table <- read_csv('tv_ad_total_causalimpact.csv')
total_table <- total_table %>% 
  clean_names() %>% 
  select(ad_date,
         program_name,
         hourly_bucket, 
         ad_content_type, 
         device_category_web_traffic,
         channel_grouping_web_traffic,
         p_value,
         incremental_sessions_pred_avg) %>% 
  mutate(ad_date = parse_date_time(ad_date,
                                   c('%d/%m/%Y %H:%M'), 
                                   exact = TRUE, 
                                   tz = "Asia/Jakarta"),
         is_significant_result = case_when(p_value <= 0.05 ~ "95%",
                                           p_value > 0.05 & p_value <= 0.1 ~ "90%",
                                           p_value > 0.1 ~ "not significant")) %>% 
  mutate(date = as.Date(ad_date)) %>% 
  filter(date < "2019-06-15")

### visualise heatmap of statistical significance
heatmap <- ggplotly(ggplot(total_table, aes(x = date, 
                        y = program_name,
                        fill = is_significant_result)) + 
  geom_tile() +
  facet_grid(channel_grouping_web_traffic ~ device_category_web_traffic))

heatmap

total_table_stat_sig <- total_table %>% 
  filter(is_significant_result == "95%" | is_significant_result == "90%")


### create boxplot for 7-day viz data ####

data <- read_csv("boxplot_data.csv")

p <- ggplot(data, aes(x=channel_grouping_web_traffic,
                      y=sessions,
                      color=device_category_web_traffic)) +
  coord_flip() +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3) +
  xlab("Channel Grouping") +
  ylab("Sessions") +
  labs(color = "Device Category") +
  ggtitle("Number of Sessions from Attributable Ads") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(device_category_web_traffic ~ .)

p <- ggplotly(p)
p

ggplot(data, aes(x=channel_grouping_web_traffic,
                 y=sessions,
                 color=device_category_web_traffic)) +
  coord_flip() +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3) +
  xlab("Channel Grouping") +
  ylab("Sessions") + 
  labs(color = "Device Category") +
  ggtitle("Number of Sessions from Attributable Ads") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(device_category_web_traffic ~ .)

ggsave("boxplot.png", units="in", width=6, height=3.5, dpi=400)
dev.off()


#### 3-day analysis - change timestamp of post intervention period ####

filenames <- c("TV Attribution - Workings_4.csv")

for (i in filenames) {

tv_ad_workings_3day <- read_csv(i)

tv_ad_workings_causalimpact_3day <- tv_ad_workings_3day %>%
  clean_names() %>% 
  mutate(device_category_web_traffic = tolower(device_category_web_traffic))


tv_ad_workings_causalimpact_3day <- tv_ad_workings_causalimpact_3day %>% 
  separate(pre_intervention_period, c("pre_intervention_start", "pre_intervention_end"), ",") %>% 
  separate(post_intervention_period, c("post_intervention_start", "post_intervention_end"), ",") %>% 
  mutate(post_intervention_end = as.POSIXct(post_intervention_end, tz = "Asia/Jakarta") - lubridate::days(4)) %>% 
  mutate(post_intervention_end = as.character(post_intervention_end))

test_output_3day <- list()

start_time <- Sys.time()
for (i in 1:nrow(tv_ad_workings_causalimpact_3day)) {
  modeloutput <- eval_causal_Impact(tv_ad_workings_causalimpact_3day$device_category_web_traffic[i],
                                    tv_ad_workings_causalimpact_3day$channel_grouping_web_traffic[i],
                                    tv_ad_workings_causalimpact_3day$pre_intervention_start[i],
                                    tv_ad_workings_causalimpact_3day$pre_intervention_end[i],
                                    tv_ad_workings_causalimpact_3day$post_intervention_start[i],
                                    tv_ad_workings_causalimpact_3day$post_intervention_end[i])
  test_output_3day <- append(test_output_3day, modeloutput)
}

test_output_3day_df <- data.frame(id=names(test_output_3day), values=unlist(test_output_3day))
test_output_3day_df <- test_output_3day_df %>% 
  group_by_at(vars(-values)) %>% 
  mutate(row_id=1:n()) %>% 
  ungroup() %>% 
  spread(key=id, value=values) %>%
  select(-row_id)

end_time <- Sys.time()
end_time - start_time

tv_ad_workings_causalimpact_3day <- tv_ad_workings_causalimpact_3day %>% 
  mutate(p_value = test_output_3day_df$p_value,
         expected_avg_sessions = test_output_3day_df$expected,
         predicted_avg_sessions = test_output_3day_df$predicted,
         effect = test_output_3day_df$relative_effect,
         standard_deviation = test_output_3day_df$relative_effect_stddev_pct) %>% 
  mutate(p_value = as.numeric(p_value)) 

write_csv(tv_ad_workings_causalimpact_3day, paste0("tv_ad_workings_causalimpact_3day_", 
                                                   str_extract(string = i, pattern = "[0-9]"),
                                                   ".csv"))
}



#### 5-day analysis - change timestamp of post intervention period ####

filenames <- c("TV Attribution - Workings_2.csv")

for (i in filenames) {
  
  tv_ad_workings_5day <- read_csv(i)
  
  tv_ad_workings_causalimpact_5day <- tv_ad_workings_5day %>%
    clean_names() %>% 
    mutate(device_category_web_traffic = tolower(device_category_web_traffic))
  
  
  tv_ad_workings_causalimpact_5day <- tv_ad_workings_causalimpact_5day %>% 
    separate(pre_intervention_period, c("pre_intervention_start", "pre_intervention_end"), ",") %>% 
    separate(post_intervention_period, c("post_intervention_start", "post_intervention_end"), ",") %>% 
    mutate(post_intervention_end = as.POSIXct(post_intervention_end, tz = "Asia/Jakarta") - lubridate::days(2)) %>% 
    mutate(post_intervention_end = as.character(post_intervention_end))
  
  test_output_5day <- list()
  
  start_time <- Sys.time()
  for (i in 1:nrow(tv_ad_workings_causalimpact_5day)) {
    modeloutput <- eval_causal_Impact(tv_ad_workings_causalimpact_5day$device_category_web_traffic[i],
                                      tv_ad_workings_causalimpact_5day$channel_grouping_web_traffic[i],
                                      tv_ad_workings_causalimpact_5day$pre_intervention_start[i],
                                      tv_ad_workings_causalimpact_5day$pre_intervention_end[i],
                                      tv_ad_workings_causalimpact_5day$post_intervention_start[i],
                                      tv_ad_workings_causalimpact_5day$post_intervention_end[i])
    test_output_5day <- append(test_output_5day, modeloutput)
  }
  
  test_output_5day_df <- data.frame(id=names(test_output_5day), values=unlist(test_output_5day))
  test_output_5day_df <- test_output_5day_df %>% 
    group_by_at(vars(-values)) %>% 
    mutate(row_id=1:n()) %>% 
    ungroup() %>% 
    spread(key=id, value=values) %>%
    select(-row_id)
  
  end_time <- Sys.time()
  end_time - start_time
  
  tv_ad_workings_causalimpact_5day <- tv_ad_workings_causalimpact_5day %>% 
    mutate(p_value = test_output_5day_df$p_value,
           expected_avg_sessions = test_output_5day_df$expected,
           predicted_avg_sessions = test_output_5day_df$predicted,
           effect = test_output_5day_df$relative_effect,
           standard_deviation = test_output_5day_df$relative_effect_stddev_pct) %>% 
    mutate(p_value = as.numeric(p_value)) 
  
  write_csv(tv_ad_workings_causalimpact_5day, paste0("tv_ad_workings_causalimpact_5day_", 
                                                     str_extract(string = i, pattern = "[0-9]"),
                                                     ".csv"))
}


#### 9-day analysis - change timestamp of post intervention period ####

filenames <- c("TV Attribution - Workings_2.csv")

for (i in filenames) {
  
  tv_ad_workings_9day <- read_csv(i)
  
  tv_ad_workings_causalimpact_9day <- tv_ad_workings_9day %>%
    clean_names() %>% 
    mutate(device_category_web_traffic = tolower(device_category_web_traffic))
  
  
  tv_ad_workings_causalimpact_9day <- tv_ad_workings_causalimpact_9day %>% 
    separate(pre_intervention_period, c("pre_intervention_start", "pre_intervention_end"), ",") %>% 
    separate(post_intervention_period, c("post_intervention_start", "post_intervention_end"), ",") %>% 
    mutate(post_intervention_end = as.POSIXct(post_intervention_end, tz = "Asia/Jakarta") + lubridate::days(2)) %>% 
    mutate(post_intervention_end = as.character(post_intervention_end))
  
  test_output_9day <- list()
  
  start_time <- Sys.time()
  for (i in 1:nrow(tv_ad_workings_causalimpact_9day)) {
    modeloutput <- eval_causal_Impact(tv_ad_workings_causalimpact_9day$device_category_web_traffic[i],
                                      tv_ad_workings_causalimpact_9day$channel_grouping_web_traffic[i],
                                      tv_ad_workings_causalimpact_9day$pre_intervention_start[i],
                                      tv_ad_workings_causalimpact_9day$pre_intervention_end[i],
                                      tv_ad_workings_causalimpact_9day$post_intervention_start[i],
                                      tv_ad_workings_causalimpact_9day$post_intervention_end[i])
    test_output_9day <- append(test_output_9day, modeloutput)
  }
  
  test_output_9day_df <- data.frame(id=names(test_output_9day), values=unlist(test_output_9day))
  test_output_9day_df <- test_output_9day_df %>% 
    group_by_at(vars(-values)) %>% 
    mutate(row_id=1:n()) %>% 
    ungroup() %>% 
    spread(key=id, value=values) %>%
    select(-row_id)
  
  end_time <- Sys.time()
  end_time - start_time
  
  tv_ad_workings_causalimpact_9day <- tv_ad_workings_causalimpact_9day %>% 
    mutate(p_value = test_output_9day_df$p_value,
           expected_avg_sessions = test_output_9day_df$expected,
           predicted_avg_sessions = test_output_9day_df$predicted,
           effect = test_output_9day_df$relative_effect,
           standard_deviation = test_output_9day_df$relative_effect_stddev_pct) %>% 
    mutate(p_value = as.numeric(p_value)) 
  
  write_csv(tv_ad_workings_causalimpact_9day, paste0("tv_ad_workings_causalimpact_9day_", 
                                                     str_extract(string = i, pattern = "[0-9]"),
                                                     ".csv"))
}


### Create table of sessions breakdown by date, and attributable sessions ####

GA_org_direct_sessions_date_agg <- GA_org_direct_sessions %>% 
  select(date, device_category, default_channel_grouping, sessions) %>% 
  group_by(date, device_category, default_channel_grouping) %>% 
  summarise(sessions = sum(sessions))



####Analyse dataset of ad buzz duration ####

ad_buzz_df <- read_csv("daywise_tv_causalimpact_compilation.csv")
ad_buzz_df <- ad_buzz_df %>% 
  clean_names()


#### table for 3 day buzz ####
ad_buzz_df_3day <- ad_buzz_df %>% 
  select(ad_date, program_name, ad_content_type, hourly_bucket_amended,
         device_category_web_traffic, channel_grouping_web_traffic,
         x3day_incremental_sessions_per_period_pred_avg, x3day_statistically_significant) %>% 
  filter(x3day_statistically_significant == "Yes") %>% 
  mutate(x3day_incremental_sessions_per_period_pred_avg = abs(x3day_incremental_sessions_per_period_pred_avg)) %>% 
  mutate(ad_date = parse_date_time(ad_date, c("%d/%m/%Y %H:%M"), exact = TRUE))

#### table for 5 day buzz #### 
ad_buzz_df_5day <- ad_buzz_df %>% 
  select(ad_date, program_name, ad_content_type, hourly_bucket_amended,
         device_category_web_traffic, channel_grouping_web_traffic,
         x5day_incremental_sessions_per_period_pred_avg, x5day_statistically_significant) %>% 
  filter(x5day_statistically_significant == "Yes") %>% 
  mutate(x5day_incremental_sessions_per_period_pred_avg = abs(x5day_incremental_sessions_per_period_pred_avg)) %>% 
  mutate(ad_date = parse_date_time(ad_date, c("%d/%m/%Y %H:%M"), exact = TRUE))


#### table for 9 day buzz #### 
ad_buzz_df_9day <- ad_buzz_df %>% 
  select(ad_date, program_name, ad_content_type, hourly_bucket_amended,
         device_category_web_traffic, channel_grouping_web_traffic,
         x9day_incremental_sessions_per_period_pred_avg, x9day_statistically_significant) %>% 
  filter(x9day_statistically_significant == "Yes") %>% 
  mutate(x9day_incremental_sessions_per_period_pred_avg = abs(x9day_incremental_sessions_per_period_pred_avg)) %>% 
  mutate(ad_date = parse_date_time(ad_date, c("%d/%m/%Y %H:%M"), exact = TRUE))


#### table for split of buzz duration by ad ####
ad_buzz_df_effectiveness <- ad_buzz_df %>% 
  select(ad_date, program_name, ad_content_type, hourly_bucket_amended,
         device_category_web_traffic, channel_grouping_web_traffic,
         ad_effectiveness_duration) %>% 
  filter(!is.na(ad_effectiveness_duration)) %>% 
  group_by(ad_effectiveness_duration) %>% 
  summarise(ad_count = length(unique(ad_date)))


### create boxplot for 3-day viz data ####
p <- ggplot(ad_buzz_df_3day, aes(x=channel_grouping_web_traffic,
                      y=x3day_incremental_sessions_per_period_pred_avg,
                      color=device_category_web_traffic)) +
  coord_flip() +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3) +
  xlab("Channel Grouping") + 
  ylab("Sessions") +
  labs(color = "Device Category") +
  ggtitle("Number of Sessions from Attributable Ads") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(device_category_web_traffic ~ .)

p <- ggplotly(p)
p

ggsave("3-day boxplot.png", units="in", width=6, height=3.5, dpi=400)
dev.off()


### create boxplot for 5-day viz data ####
p <- ggplot(ad_buzz_df_5day, aes(x=channel_grouping_web_traffic,
                                 y=x5day_incremental_sessions_per_period_pred_avg,
                                 color=device_category_web_traffic)) +
  coord_flip() +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3) +
  xlab("Channel Grouping") + 
  ylab("Sessions") +
  labs(color = "Device Category") +
  ggtitle("Number of Sessions from Attributable Ads") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(device_category_web_traffic ~ .)

p <- ggplotly(p)
p

ggsave("5-day boxplot.png", units="in", width=6, height=3.5, dpi=400)
dev.off()


### create boxplot for 9-day viz data ####
p <- ggplot(ad_buzz_df_9day, aes(x=channel_grouping_web_traffic,
                                 y=x9day_incremental_sessions_per_period_pred_avg,
                                 color=device_category_web_traffic)) +
  coord_flip() +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3) +
  xlab("Channel Grouping") + 
  ylab("Sessions") +
  labs(color = "Device Category") +
  ggtitle("Number of Sessions from Attributable Ads") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(device_category_web_traffic ~ .)

p <- ggplotly(p)
p

ggsave("9-day boxplot.png", units="in", width=6, height=3.5, dpi=400)
dev.off()


ad_buzz_df_summary <- ad_buzz_df %>% 
  select(ad_date, program_name, ad_content_type, hourly_bucket_amended,
         device_category_web_traffic, channel_grouping_web_traffic,
         ad_effectiveness_duration) %>% 
  filter(!is.null(ad_effectiveness_duration) & !is.na(ad_effectiveness_duration))


#### create facet plot for cumulative ad sessions by device ####
cumulative_ad_sessions_df <- read_csv("tv_ad_cumulative_sessions.csv")
cumulative_ad_sessions <- cumulative_ad_sessions_df %>% 
  clean_names()
  

ggplot(cumulative_ad_sessions, aes(x = days_after_ad, y = attributable_sessions)) + 
  geom_bar(stat="identity") +
  facet_wrap(vars(device_category), nrow = 3, scales = "free") +
  ggtitle("Cumulative Attributable Sessions by No. of Days After Ad") +
  theme(plot.title = element_text(hjust = 0.5))

p <- ggplotly(ggplot(cumulative_ad_sessions, aes(x = days_after_ad, y = attributable_sessions)) + 
                geom_bar(stat="identity") +
                facet_wrap(vars(device_category), nrow = 3, scales = "free") +
                ggtitle("Cumulative Attributable Sessions by No. of Days After Ad") +
                theme(plot.title = element_text(hjust = 0.5)))

p
         