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
id_app_hk <- account_list$viewId[account_list$viewName=='HK App Live View (hkal)']
id_app_indo <- account_list$viewId[account_list$viewName=='ID App Live View (idal)']
id_app_my <- account_list$viewId[account_list$viewName=='MY App Live View (myal)']
id_app_ph <- account_list$viewId[account_list$viewName=='PH App Live View (phal)']
id_app_sg <- account_list$viewId[account_list$viewName=='SG App Live View (sgal)']
id_app_tw <- account_list$viewId[account_list$viewName=='TW App Live View (twal)']

id_app_combined <- c(id_app_hk,id_app_indo, id_app_my, id_app_ph, id_app_sg, id_app_tw)

id_combined <- c(id_hk,id_indo, id_my, id_ph, id_sg, id_tw)

my_segments <- ga_segment_list()
segs <- my_segments$items

usersHDILA_Segment <- "gaid::l1sUng85RTaQLNgGmwmIyA"
seg_HDILA_users <- segment_ga4("usersHDILA_Segment", segment_id = usersHDILA_Segment)

usersdidntvisitHDILA_Segment <- "gaid::WMNWzAoSRWO9EFJYyvvmTg"
seg_nonHDILA_users <- segment_ga4("usersdidntvisitHDILA_Segment", segment_id = usersdidntvisitHDILA_Segment)

app_users_nonHDILA_Segment <- "gaid::m2mhcOZtRyaR6_UJ7qeu9w"
seg_app_users_nonHDILA <- segment_ga4("app_users_nonHDILA_Segment", segment_id = app_users_nonHDILA_Segment)

app_users_HDILA_Segment <- "gaid::8KAj6dDjTe6vQTHWqe9oEA"
seg_app_users_HDILA <- segment_ga4("app_users_HDILA_Segment", segment_id = app_users_HDILA_Segment)


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



# Frequency - app + mobile by month

ga_calc_temp_sg <- 
  google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_sg$id_combined <- id_sg
ga_calc_temp_sg$segment <- "HDILA users"

ga_calc_temp_my <- 
  google_analytics_4(id_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_my$id_combined <- id_my
ga_calc_temp_my$segment <- "HDILA users"

ga_calc_temp_indo <- 
  google_analytics_4(id_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_indo$id_combined <- id_indo
ga_calc_temp_indo$segment <- "HDILA users"

ga_calc_temp_ph <- 
  google_analytics_4(id_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_ph$id_combined <- id_ph
ga_calc_temp_ph$segment <- "HDILA users"

ga_calc_temp_hk <- 
  google_analytics_4(id_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_hk$id_combined <- id_hk
ga_calc_temp_hk$segment <- "HDILA users"

ga_calc_temp_tw <- 
  google_analytics_4(id_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_temp_tw$id_combined <- id_tw
ga_calc_temp_tw$segment <- "HDILA users"

ga_calc_mobileapp_combined <- rbind(ga_calc_temp_tw,
                          ga_calc_temp_sg,
                          ga_calc_temp_my,
                          ga_calc_temp_hk,
                          ga_calc_temp_indo,
                          ga_calc_temp_ph)


ga_calc_mobileapp <- ga_calc_mobileapp_combined %>%
  filter(deviceCategory != "desktop") %>%
  select(-deviceCategory, -id_combined) %>%
  group_by(yearMonth, segment) %>%
  summarise(sessions = sum(sessions),
            users = sum(users))

ga_calc_appusers_temp_sg <- 
  google_analytics_4(id_app_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_app_users_HDILA),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_appusers_temp_sg$id_combined <- id_app_sg
ga_calc_appusers_temp_sg$segment <- "app HDILA users"

ga_calc_appusers_temp_my <- 
  google_analytics_4(id_app_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_app_users_HDILA),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_appusers_temp_my$id_combined <- id_app_my
ga_calc_appusers_temp_my$segment <- "app HDILA users"

ga_calc_appusers_temp_tw <- 
  google_analytics_4(id_app_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_app_users_HDILA),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_appusers_temp_tw$id_combined <- id_app_tw
ga_calc_appusers_temp_tw$segment <- "app HDILA users"

ga_calc_appusers_temp_hk <- 
  google_analytics_4(id_app_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_app_users_HDILA),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_appusers_temp_hk$id_combined <- id_app_hk
ga_calc_appusers_temp_hk$segment <- "app HDILA users"

ga_calc_appusers_temp_indo <- 
  google_analytics_4(id_app_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_app_users_HDILA),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_appusers_temp_indo$id_combined <- id_app_indo
ga_calc_appusers_temp_indo$segment <- "app HDILA users"

ga_calc_appusers_temp_ph <- 
  google_analytics_4(id_app_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "yearMonth"),
                     segments = c(seg_app_users_HDILA),
                     anti_sample = TRUE,
                     max = -1)
ga_calc_appusers_temp_ph$id_combined <- id_app_ph
ga_calc_appusers_temp_ph$segment <- "app HDILA users"

ga_app_combined <- rbind(ga_calc_appusers_temp_sg,
                          ga_calc_appusers_temp_my,
                          ga_calc_appusers_temp_ph,
                          ga_calc_appusers_temp_indo,
                          ga_calc_appusers_temp_hk,
                          ga_calc_appusers_temp_tw)

ga_app_combined1 <- ga_app_combined %>%
  select(-deviceCategory, id_combined) %>%
  group_by(yearMonth, segment) %>%
  summarise(sessions = sum(sessions),
            users = sum(users))


ga_calc_combined_data <- rbind(ga_app_combined1,
                               ga_calc_mobileapp)

#ga_calc_combined_data <- ga_calc_combined %>%
#  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) 

write_csv(ga_calc_combined_data, "ga_sessionsperuser_combined.csv")


# Session Count - mobileweb by month

ga_sessioncount_temp_sg <- 
  google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_temp_sg$id_combined <- id_sg
ga_sessioncount_temp_sg$segment <- "HDILA users"

ga_sessioncount_temp_my <- 
  google_analytics_4(id_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_temp_my$id_combined <- id_my
ga_sessioncount_temp_my$segment <- "HDILA users"

ga_sessioncount_temp_ph <- 
  google_analytics_4(id_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_temp_ph$id_combined <- id_ph
ga_sessioncount_temp_ph$segment <- "HDILA users"

ga_sessioncount_temp_indo <- 
  google_analytics_4(id_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_temp_indo$id_combined <- id_indo
ga_sessioncount_temp_indo$segment <- "HDILA users"

ga_sessioncount_temp_hk <- 
  google_analytics_4(id_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_temp_hk$id_combined <- id_hk
ga_sessioncount_temp_hk$segment <- "HDILA users"

ga_sessioncount_temp_tw <- 
  google_analytics_4(id_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_HDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_temp_tw$id_combined <- id_tw
ga_sessioncount_temp_tw$segment <- "HDILA users"


ga_sessioncount_nonHDILA_temp_sg <- 
  google_analytics_4(id_sg, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_nonHDILA_temp_sg$id_combined <- id_sg
ga_sessioncount_nonHDILA_temp_sg$segment <- "non HDILA users"

ga_sessioncount_nonHDILA_temp_my <- 
  google_analytics_4(id_my, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_nonHDILA_temp_my$id_combined <- id_my
ga_sessioncount_nonHDILA_temp_my$segment <- "non HDILA users"

ga_sessioncount_nonHDILA_temp_ph <- 
  google_analytics_4(id_ph, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_nonHDILA_temp_ph$id_combined <- id_ph
ga_sessioncount_nonHDILA_temp_ph$segment <- "non HDILA users"

ga_sessioncount_nonHDILA_temp_indo <- 
  google_analytics_4(id_indo, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_nonHDILA_temp_indo$id_combined <- id_indo
ga_sessioncount_nonHDILA_temp_indo$segment <- "non HDILA users"

ga_sessioncount_nonHDILA_temp_hk <- 
  google_analytics_4(id_hk, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_nonHDILA_temp_hk$id_combined <- id_hk
ga_sessioncount_nonHDILA_temp_hk$segment <- "non HDILA users"

ga_sessioncount_nonHDILA_temp_tw <- 
  google_analytics_4(id_tw, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "users"), 
                     dimensions = c("deviceCategory", "sessionCount"),
                     segments = c(seg_nonHDILA_users),
                     anti_sample = TRUE,
                     max = -1)
ga_sessioncount_nonHDILA_temp_tw$id_combined <- id_tw
ga_sessioncount_nonHDILA_temp_tw$segment <- "non HDILA users"


ga_sessioncount_mobileweb <- rbind(ga_sessioncount_temp_sg,
                                   ga_sessioncount_temp_my,
                                   ga_sessioncount_temp_indo,
                                   ga_sessioncount_temp_hk,
                                   ga_sessioncount_temp_ph,
                                   ga_sessioncount_temp_tw,
                                   ga_sessioncount_nonHDILA_temp_sg,
                                   ga_sessioncount_nonHDILA_temp_my,
                                   ga_sessioncount_nonHDILA_temp_indo,
                                   ga_sessioncount_nonHDILA_temp_hk,
                                   ga_sessioncount_nonHDILA_temp_ph,
                                   ga_sessioncount_nonHDILA_temp_tw)


ga_sessioncount_mobilewebcombined <- ga_sessioncount_mobileweb %>%
  mutate(segment_updated = case_when(deviceCategory == "desktop" & segment == "HDILA users" ~ "HDILA Users (desktop)",
                                     deviceCategory == "mobile" & segment == "HDILA users" ~ "HDILA Users (mobileweb)",
                                     deviceCategory == "tablet" & segment == "HDILA users" ~ "HDILA Users (mobileweb)",
                                     deviceCategory == "desktop" & segment == "non HDILA users" ~ "non HDILA Users (desktop)",
                                     deviceCategory == "mobile" & segment == "non HDILA users" ~ "non HDILA Users (mobileweb)",
                                     deviceCategory == "tablet" & segment == "non HDILA users" ~ "non HDILA Users (mobileweb)")) %>%
  mutate(sessionCount = as.numeric(sessionCount)) %>%
  mutate(session_Count = case_when(sessionCount == 1 ~ "1",
                                   sessionCount == 2 ~ "2",
                                   sessionCount == 3 ~ "3",
                                   sessionCount == 4 ~ "4",
                                   sessionCount == 5 ~ "5",
                                   sessionCount == 6 ~ "6",
                                   sessionCount == 7 ~ "7",
                                   sessionCount == 8 ~ "8",
                                   sessionCount >= 9 & sessionCount <= 14 ~ "9-14",
                                   sessionCount >= 15 & sessionCount <= 25 ~ "15-25",
                                   sessionCount >= 26 & sessionCount <= 50 ~ "26-50",
                                   sessionCount >= 51 & sessionCount <= 100 ~ "51-100",
                                   sessionCount >= 101 & sessionCount <= 200 ~ "101-200",
                                   sessionCount >= 201 ~ "201+"
                                   )) %>%
  left_join(account_list[c("viewId", "viewName")], by = c("id_combined" = "viewId")) %>%
  mutate(Country = case_when(grepl("HK", viewName, ignore.case = TRUE) ~"HK",
                             grepl("ID", viewName, ignore.case = TRUE) ~"ID",
                             grepl("SG", viewName, ignore.case = TRUE) ~"SG",
                             grepl("MY", viewName, ignore.case = TRUE) ~"MY",
                             grepl("PH", viewName, ignore.case = TRUE) ~"PH",
                             grepl("TW", viewName, ignore.case = TRUE) ~"TW")) %>%
  select(Country, session_Count, segment_updated, users, sessions)


write_csv(ga_sessioncount_mobilewebcombined, "ga_sessioncount_mobilewebcombined.csv")






