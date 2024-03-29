library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)
library(zoo)
library(easyRFM)


ga_auth(new_user = TRUE)
## get your accounts and view ID
account_list <- ga_account_list()

# change the view name to the view that you wish to conduct analysis on
view_id <- account_list$viewId[account_list$viewName=='Lazada SG - SGD']
# view_id <- account_list$viewId[account_list$viewName=='Roll-up All (filtered)']
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

segment_def_for_call <- "sessions::condition::ga:deviceCategory==desktop"

## make the v3 segment object in the v4 segment object:
seg_obj_desktop <- segment_ga4("Desktop", segment_id = segment_def_for_call)


# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2017-12-01"
endDate <- "2018-03-31"

startDate2 <- "2017-01-01"

# Slide 11 - Current State of Play
# ecommerce conversion rate is used here but other conversion types can be included too
# other conversion types: goalXXCompletions
ga_data_currentstate <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate2, endDate), 
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

# Slide 17 & 18: What If - Potential incremental revenue from optimising your mobile site
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


# slide 40: What are your valuable users' interests?
ga_data_valuable_userinterests <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactionRevenue"), 
                   dimensions = c("interestAffinityCategory"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_valuable_userinterests_table <- ga_data_valuable_userinterests %>%
  mutate(revenuePerSession = transactionRevenue/sessions) %>%
  filter(sessions >= quantile(sessions, 0.7)) %>%
  arrange(desc(revenuePerSession)) %>%
  top_n(10)


ga_data_valuable_usermarket <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactionRevenue"), 
                   dimensions = c("interestInMarketCategory"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1) 

ga_data_valuable_usermarket_table <- ga_data_valuable_usermarket %>%
  mutate(revenuePerSession = transactionRevenue/sessions) %>%
  filter(sessions >= quantile(sessions, 0.7)) %>%
  arrange(desc(revenuePerSession)) %>%
  top_n(10)
  


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
                   metrics = c("sessions", "transactionsPerSession"), 
                   dimensions = c("pageDepth"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_pageDepth_CR_table <- ga_data_pageDepth_CR %>%
  select(pageDepth, sessions, transactionsPerSession) %>%
  mutate(pageDepth = as.numeric(pageDepth),
         sessions = as.numeric(sessions),
         transactionsPerSession = transactionsPerSession/100)%>%
  arrange(pageDepth) %>%
  mutate(`% cumulative sessions` = cumsum(sessions)/sum(sessions)) %>%
  select(pageDepth, `% cumulative sessions`, `Conv. Rate`= transactionsPerSession) %>%
  filter(pageDepth <= 25)

ga_data_sessionDuration_CR <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactionsPerSession"), 
                   dimensions = c("sessionDurationBucket"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_sessionDuration_CR_table <- ga_data_sessionDuration_CR %>%
  select(sessionDurationBucket, sessions, transactionsPerSession) %>%
  mutate(sessionDurationBucket = as.numeric(sessionDurationBucket),
         transactionsPerSession = transactionsPerSession/100,
         sessions = as.numeric(sessions)) %>%
  arrange(sessionDurationBucket) %>%
  mutate(`% cumulative sessions` = cumsum(sessions)/sum(sessions)) %>%
  select(sessionDurationBucket, `% cumulative sessions`, `Conv. Rate`= transactionsPerSession) %>%
  filter(sessionDurationBucket < 500)
  


# Slide 51: Re-engage Users with High Product Engagement
se_productdetail <- segment_element("productDetailViews", 
                      operator = "GREATER_THAN", 
                      type = "METRIC", 
                      comparisonValue = 0, 
                      scope = "USER")

se2_bought <- segment_element("transactions", 
                              operator = "GREATER_THAN", 
                              type = "METRIC", 
                              comparisonValue = 0, 
                              scope = "USER")

se2_didnotbuy <- segment_element("transactions", 
                              operator = "EQUAL", 
                              type = "METRIC", 
                              comparisonValue = 0, 
                              scope = "USER")

sv_productdetail <- segment_vector_simple(list(list(se_productdetail)))
sv_bought <- segment_vector_simple(list(list(se2_bought)))
sv_didnotbuy <- segment_vector_simple(list(list(se2_didnotbuy)))

seg_defined_view_boughtproduct <- segment_define(list(sv_productdetail, sv_bought))
seg_defined_view_didnotbuy <- segment_define(list(sv_productdetail, sv_didnotbuy))

segment4_view_boughtproduct <- segment_ga4("Viewed Product Page - Buy", 
                        user_segment = seg_defined_view_boughtproduct)

segment4_view_didnotbuy  <- segment_ga4("Viewed Product Page - Didn't Buy", 
                                           user_segment = seg_defined_view_didnotbuy)

lastThreemonths <- Sys.Date() - 90
yesterday <- Sys.Date() - 1

ga_data_highpdtengagement <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(lastThreemonths, yesterday), 
                   metrics = c("sessions", "pageviews"), 
                   # dimensions = c("searchUsed"),
                   segments = c(segment4_view_boughtproduct, 
                                segment4_view_didnotbuy),
                   anti_sample = TRUE,
                   max = -1)

ga_data_highpdtengagement_table <- ga_data_highpdtengagement %>%
  mutate(pageviewsPerSession = pageviews/sessions) %>%
  select(segment, pageviewsPerSession)

# Slide 57: Page Loading Time Impacts Your Conversions
se_bounced <- segment_element("bounces", 
                                    operator = "GREATER_THAN", 
                                    type = "METRIC", 
                                    comparisonValue = 0, 
                                    scope = "SESSION")

se2_nonbounce <- segment_element("bounces", 
                              operator = "EQUAL", 
                              type = "METRIC", 
                              comparisonValue = 0, 
                              scope = "SESSION")

se2_boughtsessions <- segment_element("transactions", 
                              operator = "GREATER_THAN", 
                              type = "METRIC", 
                              comparisonValue = 0, 
                              scope = "SESSION")

se2_didnotbuysessions <- segment_element("transactions", 
                                 operator = "EQUAL", 
                                 type = "METRIC", 
                                 comparisonValue = 0, 
                                 scope = "SESSION")

sv_bounce <- segment_vector_simple(list(list(se_bounced)))
sv_nonbounce <- segment_vector_simple(list(list(se2_nonbounce)))
sv_buysession <- segment_vector_simple(list(list(se2_boughtsessions)))
sv_nonbuysession <- segment_vector_simple(list(list(se2_didnotbuysessions)))

seg_defined_buyer <- segment_define(list(sv_buysession))
seg_defined_nonbuyer_nobounce <- segment_define(list(sv_nonbuysession, sv_nonbounce))
seg_defined_bounce <- segment_define(list(sv_bounce))

segment4_buyer <- segment_ga4("Buyers",
                              session_segment = seg_defined_buyer)

segment4_nonbuyer_nobounce <- segment_ga4("Non Buyers, No Bounce", 
                                          session_segment = seg_defined_nonbuyer_nobounce)

segment4_bounce <- segment_ga4("Bounce Sessions", 
                               session_segment = seg_defined_bounce)

ga_data_pageloadtime <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("avgPageLoadTime"), 
                   dimensions = c("deviceCategory"),
                   segments = c(segment4_buyer,
                                segment4_nonbuyer_nobounce,
                                segment4_bounce),
                   anti_sample = TRUE,
                   max = -1)

ga_data_pageloadtime_table <- ga_data_pageloadtime %>%
  spread(segment, avgPageLoadTime)

# Slide 61: Site Search Optimisation
ga_data_sitesearch_value <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactionRevenue"), 
                   dimensions = c("searchUsed"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_searchterms <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("productListViews", "productListCTR"), 
                   dimensions = c("searchKeyword"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_sitesearch_value_table <- ga_data_sitesearch_value %>%
  mutate(revenuePerSession = transactionRevenue / sessions,
         shareofSessions = sessions/sum(sessions)) %>%
  select(searchUsed, `Per Session Value` = revenuePerSession, `Share of Sessions` = shareofSessions)

ga_data_searchterms_table <- ga_data_searchterms %>%
  mutate(productListClicks = productListViews * productListCTR)

pdt_listviews_avg <- sum(ga_data_searchterms_table$productListViews) / length(ga_data_searchterms_table$searchKeyword)
pdt_listviews_50pct <- quantile(ga_data_searchterms_table$productListViews, 0.5)
pdt_listCTR_avg <- sum(ga_data_searchterms_table$productListClicks) / length(ga_data_searchterms_table$searchKeyword)
pdt_listCTR_50pct <- quantile(ga_data_searchterms_table$productListCTR, 0.5)

ga_data_searchterms_tablefiltered <- ga_data_searchterms_table %>%
  select(searchKeyword, productListViews, productListCTR)

# sample scatter plot
searchterm_scatter <-ggplot(data = ga_data_searchterms_tablefiltered, aes(x = productListViews, y = productListCTR)) + 
  geom_point(size=3, colour = "#D55E00")
searchterm_scatter

# Slide 65: Merchandising Optimisation: Catalog Engagement
ga_data_productName <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("productListViews", "productListCTR"), 
                   dimensions = c("productName"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_productName_table <- ga_data_productName %>%
  select(productName, productListViews, productListCTR)

# sample scatter plot
g<-ggplot(data = ga_data_productName_table, aes(x = productListViews, y = productListCTR)) + 
  geom_point(size=3, colour = "#D55E00")
g           


# Slide 67: Merchandising Optimisation: Product Engagement
ga_data_productDetail <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("productDetailViews", "productAddsToCart"), 
                   dimensions = c("productName"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_productDetail_table <- ga_data_productDetail %>%
  mutate(addtoCart_rate = productAddsToCart/productDetailViews) %>%
  filter(addtoCart_rate >= 0 & addtoCart_rate < 2)

# sample scatter plot
g1<-ggplot(data = ga_data_productDetail_table, aes(x = productDetailViews, y = addtoCart_rate)) + 
  geom_point(size=3, colour = "#D55E00")
g1  



# Slide 68: How do Sendo Users Shop on Catalog Pages?
ga_data_catalogposition <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("productListViews", "productAddsToCart", "productListClicks"), 
                   dimensions = c("productListPosition"),
                   segments = seg_obj_desktop,
                   anti_sample = TRUE,
                   max = -1)

ga_data_catalogposition_table <- ga_data_catalogposition %>%
  filter(productListPosition != "(not set)") %>%
  mutate(productListPosition = as.numeric(productListPosition)) %>%
  mutate(productListPositionBin = case_when(productListPosition >= 0 & productListPosition <= 10 ~ "0-10",
                                            productListPosition >= 11 & productListPosition <= 20 ~ "11-20",
                                            productListPosition >= 21 & productListPosition <= 30 ~ "21-30",
                                            productListPosition >= 31 & productListPosition <= 50 ~ "31-50",
                                            productListPosition >= 51 & productListPosition <= 70 ~ "51-70",
                                            productListPosition >= 71 & productListPosition <= 100 ~ "71-100",
                                            productListPosition >= 101 ~ "> 100")) %>%
  group_by(productListPositionBin) %>%
  summarise(productListViews = sum(productListViews),
            productAddsToCart = sum(productAddsToCart),
            productListClicks = sum(productListClicks)) %>%
  mutate(productListViewTotal = productListViews[1],
         cumulative_shareofViews = productListViews / productListViewTotal,
         addtoCart_rate = productAddsToCart/productListViews,
         productCTR = productListClicks /productListViews) %>%
  select(productListPositionBin,
         productListViews,
         productAddsToCart,
         productListClicks,
         productListViewTotal,
         `Share of Views` = cumulative_shareofViews,
         `A2C` = addtoCart_rate,
         CTR = productCTR) %>%
  arrange(productListPositionBin)

# Slide 71: When To Re-Engage Your New Users?
ga_data_daysSinceLastSession <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions"), 
                   dimensions = c("sessionCount", "daysSinceLastSession", "deviceCategory"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_daysSinceLastSession_table <- ga_data_daysSinceLastSession %>%
  mutate(sessionCount = as.numeric(sessionCount),
         daysSinceLastSession = as.numeric(daysSinceLastSession),
         weight = daysSinceLastSession * sessions) %>%
  group_by(sessionCount, deviceCategory) %>%
  summarise(sessions = sum(sessions),
            avgDaysSinceLastSession = sum(weight)/sum(sessions)) %>%
  filter(sessionCount <= 30)

ggplot(ga_data_daysSinceLastSession_table, 
       aes(x=sessionCount, y=avgDaysSinceLastSession, color=deviceCategory)) +
  geom_bar(stat="identity") + facet_wrap(~deviceCategory) + theme_minimal()


# Slide 72: Valuable Users by Long-term Behaviour
ga_data_freq_CR <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("sessionCount"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_recency_CR <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("sessions", "transactions"), 
                   dimensions = c("daysSinceLastSession"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

ga_data_freq_CR_table <- ga_data_freq_CR %>%
  mutate(CR = transactions / sessions, sessionCount = as.numeric(sessionCount)) %>%
  select(sessionCount, CR) %>%
  filter(sessionCount <= 35) %>%
  arrange(sessionCount)


ga_data_recency_CR_table <- ga_data_recency_CR %>%
  mutate(CR = transactions / sessions, daysSinceLastSession = as.numeric(daysSinceLastSession)) %>%
  select(`Days Since Last Session` = daysSinceLastSession, CR) %>%
  filter(`Days Since Last Session` <= 30) %>%
  arrange(`Days Since Last Session`)

# RFM
ga_data_RFM <- 
  google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                   date_range = c(startDate, endDate), 
                   metrics = c("transactionRevenue"), 
                   dimensions = c("dimension5", "date", "transactionId"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)

# result <- rfm_auto(ga_data_RFM,id="dimension5",payment="transactionRevenue",
#                   date="date",date_format = "%Y%m%d")


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
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_valuable_userinterests_table, anchor = "K281")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_valuable_usermarket_table, anchor = "K294")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_pageDepth_CR_table, anchor = "D340")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_sessionDuration_CR_table, anchor = "M340")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_highpdtengagement_table, anchor = "I371")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_sitesearch_value_table, anchor = "E371")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_pageloadtime_table, anchor = "H391")
# this needs to be optimised as large amounts of data will vastly slow down google sheets API
# gs_edit_cells(myworksheet, ws = "SiteSearchOptimisation", input = ga_data_searchterms_tablefiltered, anchor = "A1")
# this needs to be optimised as large amounts of data will vastly slow down google sheets API
# gs_edit_cells(myworksheet, ws = "productListViewsCTR", input = ga_data_productName_table, anchor = "A1")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_catalogposition_table, anchor = "E449")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_daysSinceLastSession_table, anchor = "E473")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_freq_CR_table, anchor = "E508")
gs_edit_cells(myworksheet, ws = "GA Data", input = ga_data_recency_CR_table, anchor = "H508")