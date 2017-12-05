library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()
#id_hk <- account_list[64,'viewId']
#id_indo <- account_list[65,'viewId']
#id_my <- account_list[66,'viewId']
#id_ph <- account_list[67,'viewId']
#id_sg <- account_list[68,'viewId']
#id_tw <- account_list[69,'viewId']

id_hk <- account_list$viewId[account_list$viewName=='HK Web Live View (hkwl hk$)']
id_indo <- account_list$viewId[account_list$viewName=='ID Web Live View (idwl idr)']
id_my <- account_list$viewId[account_list$viewName=='MY Web Live View (mywl myr)']
id_ph <- account_list$viewId[account_list$viewName=='PH Web Live View (phwl php)']
id_sg <- account_list$viewId[account_list$viewName=='SG Web Live View (sgwl sgd)']
id_tw <- account_list$viewId[account_list$viewName=='TW Web Live View (twwl twd)']

id_combined <- c(id_hk,id_indo, id_my, id_ph, id_sg, id_tw)

my_segments <- ga_segment_list()
segs <- my_segments$items

usersHDILA_Segment <- "gaid::l1sUng85RTaQLNgGmwmIyA"
seg_HDILA_users <- segment_ga4("usersHDILA_Segment", segment_id = usersHDILA_Segment)

usersdidntvisitHDILA_Segment <- "gaid::WMNWzAoSRWO9EFJYyvvmTg"
seg_nonHDILA_users <- segment_ga4("usersdidntvisitHDILA_Segment", segment_id = usersdidntvisitHDILA_Segment)


# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-09-08"
endDate <- "2017-11-28"

# Pivot - newRepeat
ga_traffic_data_merged <- data.frame()

ga_data_temp_sg <- 
  google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_sg$id_combined <- id_sg
ga_data_temp_sg$segment <- "HDILA users"

ga_data_temp_my <- 
  google_analytics_4(id_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_my$id_combined <- id_my
ga_data_temp_my$segment <- "HDILA users"

ga_data_temp_ph <- 
  google_analytics_4(id_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_ph$id_combined <- id_ph
ga_data_temp_ph$segment <- "HDILA users"

ga_data_temp_indo <- 
  google_analytics_4(id_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_indo$id_combined <- id_indo
ga_data_temp_indo$segment <- "HDILA users"

ga_data_temp_hk <- 
  google_analytics_4(id_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_hk$id_combined <- id_hk
ga_data_temp_hk$segment <- "HDILA users"

ga_data_temp_tw <- 
  google_analytics_4(id_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_tw$id_combined <- id_tw
ga_data_temp_tw$segment <- "HDILA users"

ga_data_temp_sg_nonHDILA <- 
  google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_sg_nonHDILA$id_combined <- id_sg
ga_data_temp_sg_nonHDILA$segment <- "non HDILA users"

ga_data_temp_my_nonHDILA <- 
  google_analytics_4(id_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_my_nonHDILA$id_combined <- id_my
ga_data_temp_my_nonHDILA$segment <- "non HDILA users"

ga_data_temp_ph_nonHDILA <- 
  google_analytics_4(id_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_ph_nonHDILA$id_combined <- id_ph
ga_data_temp_ph_nonHDILA$segment <- "non HDILA users"

ga_data_temp_indo_nonHDILA <- 
  google_analytics_4(id_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_indo_nonHDILA$id_combined <- id_indo
ga_data_temp_indo_nonHDILA$segment <- "non HDILA users"

ga_data_temp_hk_nonHDILA <- 
  google_analytics_4(id_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_hk_nonHDILA$id_combined <- id_hk
ga_data_temp_hk_nonHDILA$segment <- "non HDILA users"

ga_data_temp_tw_nonHDILA <- 
  google_analytics_4(id_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions"), 
                     dimensions = c("landingPagePath", "sourceMedium", "userType"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_data_temp_tw_nonHDILA$id_combined <- id_tw
ga_data_temp_tw_nonHDILA$segment <- "non HDILA users"


ga_usertraffic_data_merged <- rbind(ga_data_temp_sg,
                                            ga_data_temp_my,
                                            ga_data_temp_ph,
                                            ga_data_temp_indo,
                                            ga_data_temp_hk,
                                            ga_data_temp_tw,
                                    ga_data_temp_sg_nonHDILA,
                                    ga_data_temp_my_nonHDILA,
                                    ga_data_temp_ph_nonHDILA,
                                    ga_data_temp_indo_nonHDILA,
                                    ga_data_temp_hk_nonHDILA,
                                    ga_data_temp_tw_nonHDILA)

ga_usertraffic_data_merged1 <- ga_usertraffic_data_merged %>%
  select(-id_combined) %>%
  group_by(landingPagePath, sourceMedium, userType, segment) %>%
  summarize(sessions = sum(sessions))

ga_usertraffic_data_merged_test <- ga_usertraffic_data_merged1 %>%
  spread(userType, sessions, fill = 0) %>%
  mutate(TotalSessions = `New Visitor` + `Returning Visitor`) %>%
  ungroup() %>%
  top_n(50, TotalSessions) %>%
  arrange(-TotalSessions)

write_csv(ga_usertraffic_data_merged_test, "ga_user_traffic_data_merged.csv")


# Calculator
ga_calc <- data.frame()

ga_calc_temp_sg <- 
  google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_sg$id_combined <- id_sg
ga_calc_temp_sg$segment <- "HDILA users"

ga_calc_temp_my <- 
  google_analytics_4(id_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_my$id_combined <- id_my
ga_calc_temp_my$segment <- "HDILA users"

ga_calc_temp_indo <- 
  google_analytics_4(id_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_indo$id_combined <- id_indo
ga_calc_temp_indo$segment <- "HDILA users"

ga_calc_temp_ph <- 
  google_analytics_4(id_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_ph$id_combined <- id_ph
ga_calc_temp_ph$segment <- "HDILA users"

ga_calc_temp_hk <- 
  google_analytics_4(id_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_hk$id_combined <- id_hk
ga_calc_temp_hk$segment <- "HDILA users"

ga_calc_temp_tw <- 
  google_analytics_4(id_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_tw$id_combined <- id_tw
ga_calc_temp_tw$segment <- "HDILA users"

ga_calc_nonusers_temp_sg <- 
  google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_nonusers_temp_sg$id_combined <- id_sg
ga_calc_nonusers_temp_sg$segment <- "non HDILA users"

ga_calc_nonusers_temp_my <- 
  google_analytics_4(id_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_nonusers_temp_my$id_combined <- id_my
ga_calc_nonusers_temp_my$segment <- "non HDILA users"

ga_calc_nonusers_temp_tw <- 
  google_analytics_4(id_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_nonusers_temp_tw$id_combined <- id_tw
ga_calc_nonusers_temp_tw$segment <- "non HDILA users"

ga_calc_nonusers_temp_hk <- 
  google_analytics_4(id_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_nonusers_temp_hk$id_combined <- id_hk
ga_calc_nonusers_temp_hk$segment <- "non HDILA users"

ga_calc_nonusers_temp_indo <- 
  google_analytics_4(id_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_nonusers_temp_indo$id_combined <- id_indo
ga_calc_nonusers_temp_indo$segment <- "non HDILA users"

ga_calc_nonusers_temp_ph <- 
  google_analytics_4(id_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("users", "sessionDuration", "pageViews", "sessions", "itemRevenue", "transactions"), 
                     dimensions = c("deviceCategory"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_nonusers_temp_ph$id_combined <- id_ph
ga_calc_nonusers_temp_ph$segment <- "non HDILA users"

ga_calc_combined <- rbind(ga_calc_nonusers_temp_ph,
                          ga_calc_nonusers_temp_sg,
                          ga_calc_nonusers_temp_my,
                          ga_calc_nonusers_temp_indo,
                          ga_calc_nonusers_temp_hk,
                          ga_calc_nonusers_temp_tw,
                          ga_calc_temp_tw,
                          ga_calc_temp_sg,
                          ga_calc_temp_my,
                          ga_calc_temp_hk,
                          ga_calc_temp_indo,
                          ga_calc_temp_ph)

ga_calc_combined_data <- ga_calc_combined %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) 

write_csv(ga_calc_combined_data, "ga_calc_combined.csv")






