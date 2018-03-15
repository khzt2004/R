library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)
library(zoo)


ga_auth(new_user = TRUE)
## get your accounts and view ID
account_list <- ga_account_list()

view_id <- account_list$viewId[account_list$viewName=='Master Global All markets - Filtered View']

id_combined <- c(view_id)

# selecting segments
my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

se_trans <- segment_element("transactions", 
                      operator = "GREATER_THAN", 
                      type = "METRIC", 
                      comparisonValue = 0, 
                      scope = "USER")

se_usertype <- segment_element("userType", 
                       operator = "EXACT", 
                       type = "DIMENSION", 
                       expressions = "New Visitor",
                       scope = "USER")

sv_simple <- segment_vector_simple(list(list(se_trans)))
sv_simple2 <- segment_vector_simple(list(list(se_usertype)))
seg_defined <- segment_define(list(sv_simple))
segment_trans_usertype <- segment_ga4("simple", user_segment = seg_defined)

segment_for_usertype <- "gaid::wZRM4rlFQvqpC-mzu6frBg"
seg_usertype <- segment_ga4("usertype", segment_id = segment_for_usertype)


# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2018-01-10"
endDate <- "2018-02-05"

# Slide 11 - Current State of Play
# ecommerce conversion rate is used here but other conversion types can be included too
# other conversion types: goalXXCompletions
ga_data_currentstate <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("sessions", "transactions", "transactionRevenue"), 
                     dimensions = c("yearMonth", "deviceCategory", "userType"),
                     segments = c(seg_allUsers),
                     anti_sample = TRUE,
                     max = -1)

ga_data_currentstate <-  ga_data_currentstate %>%
  mutate(yearMonth = as.yearmon(as.character(yearMonth), "%Y%m"))


chartData <- ga_data_currentstate %>%
  group_by(yearMonth) %>%
  summarize(Sessions = sum(sessions),
            transactions = sum(transactions)) %>%
  mutate(transactionsPerSession = transactions / Sessions) %>%
  select(-transactions) %>%
  rename(`Conversion Rate` = transactionsPerSession)

# traffic growth rate by device
currentState_trafficGrowth_device <- ga_data_currentstate %>%
  select(yearMonth, deviceCategory, sessions) %>%
  group_by(deviceCategory, yearMonth) %>%
  summarize(sessions = sum(sessions))

currentState_trafficGrowth_device <- currentState_trafficGrowth_device %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  spread(yearMonth, sessions)

currentState_trafficGrowth_device$`% change` <- (currentState_trafficGrowth_device[[3]]-currentState_trafficGrowth_device[[2]])/currentState_trafficGrowth_device[[2]]
currentState_trafficGrowth_device$`% change` <- round(currentState_trafficGrowth_device$`% change`, 3)  
  
# traffic growth rate by new vs returning
currentState_trafficGrowth_userType <- ga_data_currentstate %>%
  select(yearMonth, userType, sessions) %>%
  group_by(userType, yearMonth) %>%
  summarize(sessions = sum(sessions))

currentState_trafficGrowth_userType <- currentState_trafficGrowth_userType %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  spread(yearMonth, sessions)

currentState_trafficGrowth_userType$`% change` <- (currentState_trafficGrowth_userType[[3]]-currentState_trafficGrowth_userType[[2]])/currentState_trafficGrowth_userType[[2]]
currentState_trafficGrowth_userType$`% change` <- round(currentState_trafficGrowth_userType$`% change`, 3)  

# traffic growth rate by new/returning and device category
ga_data_currentstate_all <- ga_data_currentstate %>%
  group_by(userType, deviceCategory, yearMonth) %>%
  summarize(sessions= sum(sessions))

currentState_trafficGrowth_all <- ga_data_currentstate_all %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  spread(yearMonth, sessions)

currentState_trafficGrowth_all$`% change` <- (currentState_trafficGrowth_all[[4]]-currentState_trafficGrowth_all[[3]])/currentState_trafficGrowth_all[[3]]
currentState_trafficGrowth_all$`% change` <- round(currentState_trafficGrowth_all$`% change`, 3)  

# Transactions & Revenue growth rate
ga_data_currentstate_txnrevenue <- ga_data_currentstate %>%
  group_by(deviceCategory, yearMonth) %>%
  summarize(transactions = sum(transactions),
            transactionRevenue = sum(transactionRevenue)) %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  gather(variable, value, -(yearMonth:deviceCategory)) %>%
  unite(temp, yearMonth, variable) %>%
  spread(temp, value)

# Table for Ecommerce Conversion Rate by Device
ga_data_currentstate_ecomm_conv <- ga_data_currentstate %>%
  group_by(deviceCategory, yearMonth) %>%
  summarize(transactions = sum(transactions),
            sessions = sum(sessions)) %>%
  filter((yearMonth == min(yearMonth) | yearMonth == max(yearMonth))) %>%
  gather(variable, value, -(yearMonth:deviceCategory)) %>%
  unite(temp, yearMonth, variable) %>%
  spread(temp, value)

# Slide 12 - Your Users Are Now Mobile First
# Share of Sessions By Platforms
sessions_deviceSplit <- ga_data_currentstate %>%
  group_by(deviceCategory, yearMonth) %>%
  summarize(devicesessions = sum(sessions)) %>%
  left_join(chartData, on = "yearMonth") %>%
  select(-`Conversion Rate`) %>%
  gather(variable, value, -(yearMonth:deviceCategory)) %>%
  unite(temp, deviceCategory, variable) %>%
  spread(temp, value) %>%
  mutate(desktop = desktop_devicesessions/desktop_Sessions,
         mobile = mobile_devicesessions/mobile_Sessions,
         tablet = tablet_devicesessions/tablet_Sessions) %>%
  select(1, 8:10)

sessions_deviceSplit_latestmonth <- sessions_deviceSplit %>%
  filter(yearMonth == max(yearMonth))


# Slide 13: Mobile vs Desktop Users: Revenue Split
revShare_deviceSplit <- ga_data_currentstate %>%
  group_by(deviceCategory) %>%
  summarise(transactionRevenue = sum(transactionRevenue)) %>%
  mutate(percentRev = transactionRevenue / sum(transactionRevenue))

cr_aov_deviceSplit <- ga_data_currentstate %>%
  group_by(deviceCategory) %>%
  summarise(CR = sum(transactions) / sum(sessions),
            AOV = sum(transactionRevenue) / sum(transactions))
         
# Slide 14: Impact of Mobile vs Desktop in Purchase Paths are Different
ga_data_shoppingstage <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "pageViews"), 
                   dimensions = c("shoppingStage", "deviceCategory"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_shoppingstage_filter <- ga_data_shoppingstage %>%
  filter(shoppingStage == "ADD_TO_CART" | shoppingStage == "CHECKOUT")

# Slide 17: What If - Potential incremental revenue from optimising your mobile site
ga_data_potentialrevenue <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactions", "transactionRevenue"), 
                   dimensions = c("deviceCategory"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_potentialrevenue_table <- ga_data_potentialrevenue %>%
  mutate(`Conversion Rate` = transactions / sessions,
         `Avg Order Value` = transactionRevenue / transactions) %>%
  select( 1:4, Revenue = transactionRevenue, 5:7 ) %>%
  filter(deviceCategory != 'tablet') %>%
  select(deviceCategory, sessions, `Conversion Rate`, `Avg Order Value`, Revenue) %>%
  gather(key = metric, value = value, 2:5) %>%
  spread(deviceCategory, value) %>%
  arrange(desc(metric))

mobile_50pct_sessions <- ga_data_potentialrevenue_table$mobile[ga_data_potentialrevenue_table$metric == "sessions"][1]
mobile_50pct_CR <- 1.5*(ga_data_potentialrevenue_table$mobile[ga_data_potentialrevenue_table$metric == "Conversion Rate"][1])
mobile_50pct_AOV <- ga_data_potentialrevenue_table$mobile[ga_data_potentialrevenue_table$metric == "Avg Order Value"][1]
mobile_50pct_Rev <- mobile_50pct_sessions*mobile_50pct_CR*mobile_50pct_AOV

mobile_desktop_parity_sessions <- ga_data_potentialrevenue_table$mobile[ga_data_potentialrevenue_table$metric == "sessions"][1]
mobile_desktop_parity_CR <- ga_data_potentialrevenue_table$desktop[ga_data_potentialrevenue_table$metric == "Conversion Rate"][1]
mobile_desktop_parity_AOV <- ga_data_potentialrevenue_table$mobile[ga_data_potentialrevenue_table$metric == "Avg Order Value"][1]
mobile_desktop_parity_Rev <- mobile_desktop_parity_sessions*mobile_desktop_parity_CR*mobile_desktop_parity_AOV

potentialRevenue_cols <- data.frame(metric = c("sessions", "Revenue", "Conversion Rate", "Avg Order Value"), 
                                    `Increase Mobile Conversion Rate by 50 pct` = c(mobile_50pct_sessions, 
                                          mobile_50pct_Rev, mobile_50pct_CR, 
                                          mobile_50pct_AOV), 
                                    `Bring Mobile to Parity with Desktop` = c(mobile_desktop_parity_sessions, 
                                                                              mobile_desktop_parity_Rev,
                                                                              mobile_desktop_parity_CR, 
                                                                              mobile_desktop_parity_AOV))

ga_data_potentialrevenue_table_merged <- ga_data_potentialrevenue_table %>%
  left_join(potentialRevenue_cols, by = "metric")

date_range_months <- as.numeric((as.Date(endDate) - as.Date(startDate))/30)
mobile_incrementalRev_50pct <- ((mobile_50pct_Rev - ga_data_potentialrevenue_table$mobile[ga_data_potentialrevenue_table$metric == "Revenue"][1])/date_range_months)*12
mobile_incrementalRev_parity <- ((mobile_desktop_parity_Rev - ga_data_potentialrevenue_table$mobile[ga_data_potentialrevenue_table$metric == "Revenue"][1])/date_range_months)*12

# Slide 38: What If - Potential incremental revenue from optimising your mobile site
ga_data_sessions_gender <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions"), 
                   dimensions = c("deviceCategory", "userGender"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_sessions_age <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions"), 
                   dimensions = c("deviceCategory", "userAgeBracket"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_sessions_gender_table <- ga_data_sessions_gender %>%
  select(deviceCategory, userGender, sessions) %>%
  filter(deviceCategory != 'tablet') %>%
  group_by(deviceCategory) %>%
  mutate(sessions_pct = sessions/sum(sessions)) %>%
  select(deviceCategory, userGender, sessions_pct) %>%
  spread(userGender, sessions_pct)

ga_data_sessions_age_table <- ga_data_sessions_age %>%
  select(deviceCategory, userAgeBracket, sessions) %>%
  filter(deviceCategory != 'tablet') %>%
  group_by(deviceCategory) %>%
  mutate(sessions_pct = sessions/sum(sessions)) %>%
  select(deviceCategory, userAgeBracket, sessions_pct) %>%
  spread(userAgeBracket, sessions_pct)


# Slide 39: Dissecting value based on demographics
ga_data_sessions_gender_split <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactionRevenue"), 
                   dimensions = c("deviceCategory", "userGender", "userAgeBracket"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_sessions_gender_split_table <- ga_data_sessions_gender_split %>%
  mutate(RevenuePerSession = transactionRevenue / sessions) %>%
  select(deviceCategory, userGender, userAgeBracket, RevenuePerSession) %>%
  filter(deviceCategory != 'tablet') %>%
  group_by(deviceCategory) 

ga_data_sessions_gender_split_table_female <- ga_data_sessions_gender_split_table %>%
  filter(userGender == "female") %>%
  select(deviceCategory, userAgeBracket, RevenuePerSession) %>%
  spread(deviceCategory, RevenuePerSession)

ga_data_sessions_gender_split_table_male <- ga_data_sessions_gender_split_table %>%
  filter(userGender == "male") %>%
  select(deviceCategory, userAgeBracket, RevenuePerSession) %>%
  spread(deviceCategory, RevenuePerSession)

# Slide 41: Which regions are your mobile users from?
ga_data_region_current <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactionRevenue"), 
                   dimensions = c("deviceCategory", "country"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)
ga_data_region_current$timeframe <- "current"

ga_data_region_previous <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(as.Date(startDate) - 365, as.Date(endDate) - 365), 
                   metrics = c("sessions", "transactionRevenue"), 
                   dimensions = c("deviceCategory", "country"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)
ga_data_region_previous$timeframe <- "previous"

ga_data_region_combined <- rbind(ga_data_region_current, ga_data_region_previous)

ga_data_region_combined_calc <- ga_data_region_combined %>%
  group_by(deviceCategory, country, timeframe) %>%
  filter(deviceCategory == "mobile") %>%
  mutate(RevenuePerSession = transactionRevenue / sessions) %>%
  ungroup() %>%
  select(timeframe, country, sessions, transactionRevenue, RevenuePerSession) %>%
  gather(metrics, value, 3:5) %>%
  unite(timeframe_concat, timeframe, metrics, sep = '_') %>%
  spread(timeframe_concat, value) %>%
  mutate(sessions_pct_change = current_sessions / previous_sessions) %>%
  select(country, current_RevenuePerSession, sessions_pct_change) %>%
  filter(current_RevenuePerSession > 0)


# Slide 42: Reach your users at the right time
ga_data_hourday_device <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions"), 
                   dimensions = c("deviceCategory", "hour", "dayOfWeekName"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_hourday_device_table <- ga_data_hourday_device %>%
  filter(deviceCategory != "tablet") %>%
  group_by(deviceCategory, hour) %>%
  summarise(sessions= sum(sessions)) %>%
  spread(deviceCategory, sessions) %>%
  mutate(sessionbyHour = desktop + mobile) %>%
  mutate(desktop_pct = desktop / sessionbyHour,
         mobile_pct = mobile / sessionbyHour) %>%
  select(hour, desktop = desktop_pct, mobile = mobile_pct)

ga_data_weekday_device_table <- ga_data_hourday_device %>%
  filter(deviceCategory != "tablet") %>%
  group_by(deviceCategory, dayOfWeekName) %>%
  summarise(sessions= sum(sessions)) %>%
  spread(deviceCategory, sessions) %>%
  mutate(sessionbyDayofWeek = desktop + mobile) %>%
  mutate(desktop_pct = desktop / sessionbyDayofWeek,
         mobile_pct = mobile / sessionbyDayofWeek) %>%
  select(dayOfWeekName, desktop = desktop_pct, mobile = mobile_pct)


# Slide 43: Session Signals of Valuable Users
ga_data_pageDepth_CR <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("transactionsPerSession"), 
                   dimensions = c("pageDepth"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)


# Value of Site Search Traffic

# slide 73: Drive first time purchaser
ga_data_repeatpurchase <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate, endDate), 
                     metrics = c("transactionsPerUser"), 
                     dimensions = c("userType"),
                     segments = c(seg_usertype),
                     anti_sample = TRUE,
                     max = -1)


# upload data to Googlesheets - what if owner of google sheet is different
my_sheets <- gs_ls()
myworksheet <- gs_key("1ENbfT76-Q_HcmvzuePMTqQjF3EU4IJxMevJ4rgdNK3c")

# alternative - get access to specified google sheets by title
# myworksheet <- gs_title("Sendo_Measurement Deck")
gs_edit_cells(myworksheet, ws = "GA Data", input = chartData, anchor = "J3")
gs_edit_cells(myworksheet, ws = "GA Data", input = currentState_trafficGrowth_device, anchor = "A17")
gs_edit_cells(myworksheet, ws = "GA Data", input = currentState_trafficGrowth_userType, anchor = "A24") 
gs_edit_cells(myworksheet, ws = "GA Data", input = currentState_trafficGrowth_all, anchor = "A30") 
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_currentstate_txnrevenue, anchor = "A39")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_currentstate_ecomm_conv, anchor = "A45") 
gs_edit_cells(myworksheet, ws = "GA Data", input = sessions_deviceSplit, anchor = "J60") 
gs_edit_cells(myworksheet, ws = "GA Data", input = sessions_deviceSplit_latestmonth, anchor = "A78") 
gs_edit_cells(myworksheet, ws = "GA Data", input = revShare_deviceSplit, anchor = "A85") 
gs_edit_cells(myworksheet, ws = "GA Data", input = cr_aov_deviceSplit, anchor = "A91") 
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_potentialrevenue_table, anchor = "A196") 
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_potentialrevenue_table_merged, anchor = "A205") 
gs_edit_cells(myworksheet, ws = "GA Data", input = mobile_incrementalRev_50pct, anchor = "B212") 
gs_edit_cells(myworksheet, ws = "GA Data", input = mobile_incrementalRev_parity, anchor = "B213") 
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_sessions_gender_table, anchor = "E219")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_sessions_age_table, anchor = "E233")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_sessions_gender_split_table_female, anchor = "E250")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_sessions_gender_split_table_male, anchor = "E264")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_hourday_device_table, anchor = "E300")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_weekday_device_table, anchor = "E327")



gs_edit_cells(myworksheet, ws = "Analysis Steps_Merchandising", input = sessions_deviceSplit_latestmonth, anchor = "J3") 