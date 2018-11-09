library(googleAnalyticsR)
library(future.apply)
library(tidyverse)
library(bigrquery)

## setup multisession R for your parallel data fetches -------------------------------------
plan(multisession)

# login as new_user = TRUE if switching accounts. Otherwise do not set new_user = true
ga_auth()
# ga_auth(new_user = TRUE)

# get list of custom dimensions -------------------------------------
customdimensions_list <- as.data.frame(ga_custom_vars_list(17015991, "UA-17015991-1", 
                                                           type = c("customDimensions")))

Sys.setenv(GA_AUTH_FILE = "C:/Users/User/Documents/.httr-oauth")
# need alternative for mac

# get account list -------------------------------------
account_list <- ga_account_list()

## the ViewIds to fetch all at once -------------------------------------
gaids <- c(account_list[2122,'viewId'], account_list[2125,'viewId'], account_list[2128,'viewId'])

# selecting segments -------------------------------------
my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

my_fetch <- function(x) {
  google_analytics(x, 
                   date_range = c("2018-01-01","yesterday"), 
                   metrics = c("sessions", "transactions", "transactionRevenue"), 
                   dimensions = c("yearMonth", "deviceCategory", "userType"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)
}

## makes 3 API calls at once -------------------------------------
all_data <- future_lapply(gaids, my_fetch)
df1 <- data.frame(all_data[1])
df1 <- df1 %>% mutate(viewID = account_list[2122,'viewName'])
df2 <- data.frame(all_data[2])
df2 <- df2 %>% mutate(viewID = account_list[2125,'viewName'])
df3 <- data.frame(all_data[3])
df3 <- df3 %>% mutate(viewID = account_list[2128,'viewName'])
df_all <- rbind(df1,df2,df3)

# query multiple segments -------------------------------------
segment_for_newusers <- "gaid::-2"
seg_newusers <- segment_ga4("new Users", segment_id = segment_for_newusers)
segment_for_returnusers <- "gaid::-3"
seg_returnusers <- segment_ga4("return Users", segment_id = segment_for_returnusers)
segment_for_paidusers <- "gaid::-4"
seg_paidusers <- segment_ga4("paid Users", segment_id = segment_for_paidusers)
segment_for_organicusers <- "gaid::-5"
seg_organicusers <- segment_ga4("organic Users", segment_id = segment_for_organicusers)
segment_for_searchusers <- "gaid::-6"
seg_searchusers <- segment_ga4("search Users", segment_id = segment_for_searchusers)
segment_for_directusers <- "gaid::-7"
seg_directusers <- segment_ga4("direct Users", segment_id = segment_for_directusers)
segment_for_referralusers <- "gaid::-8"
seg_referralusers <- segment_ga4("referral Users", segment_id = segment_for_referralusers)
segment_for_convusers <- "gaid::-9"
seg_convusers <- segment_ga4("conv Users", segment_id = segment_for_convusers)
segment_for_transactionusers <- "gaid::-10"
seg_transactionusers <- segment_ga4("transaction Users", segment_id = segment_for_transactionusers)
segment_for_mobiletabletusers <- "gaid::-11"
seg_mobiletabletusers <- segment_ga4("mobiletablet Users", segment_id = segment_for_mobiletabletusers)

segmentlist <- c(seg_allUsers,
                 seg_newusers,
                 seg_returnusers,
                 seg_paidusers,
                 seg_organicusers,
                 seg_searchusers,
                 seg_directusers,
                 seg_referralusers,
                 seg_convusers) 

segmentlisting <- split(segmentlist, (seq_along(segmentlist) - 1L) %/% 4L)

ga_data_final_segment <- data.frame()

for (i in segmentlisting) {
  ga_data_segment_eg <- 
    google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate2, endDate), 
                     metrics = c("sessions", "transactions", "transactionRevenue"), 
                     dimensions = c("yearMonth", "deviceCategory", "userType"),
                     segments = i,
                     anti_sample = TRUE,
                     max = -1)
  
  ga_data_final_segment <- rbind(ga_data_final_segment, ga_data_segment_eg)
}


## pick a profile with data to query
ga_id <- account_list[1123,'viewId']

## get a list of what metrics and dimensions you can use
ga_auth()
meta <- google_analytics_meta()

googleAnalyticsR:::gadget_GASegment()

## make two segment elements
se <- segment_element("sessions", 
                      operator = "GREATER_THAN", 
                      type = "METRIC", 
                      comparisonValue = 3, 
                      scope = "USER")

se3 <- segment_element("medium", 
                       operator = "REGEXP", 
                       type = "DIMENSION", 
                       expressions = "^(email|referral)$",
                       scope = "SESSION")

sv_simple <- segment_vector_simple(list(list(segment_ga_google5sec)))
seg_defined <- segment_define(sv_simple)
segment4 <- segment_ga4("simple", user_segment = seg_defined)


# segments: semicolon is "AND", a comma is "OR"

segment_def_medium <- "sessions::condition::ga:medium=~^(email|referral)$"
seg_obj_medium <- segment_ga4("test", segment_id = segment_def_medium)

segment_def_google30sec <- "sessions::condition::ga:source=~^(google)$;ga:timeOnPage>30"
seg_obj_google30sec <- segment_ga4("test", segment_id = segment_def_google30sec)

segment_def_morethan3sessions <- "sessions::condition::ga:sessions>3"
seg_obj_morethan3sessions <- segment_ga4("test", segment_id = segment_def_morethan3sessions)

segment_def_orgtraffic_w_conversions <- "sessions::condition::ga:medium=~^(organic)$;ga:goal11Completions>0"
seg_obj_orgtraffic_w_conversions <- segment_ga4("test", segment_id = segment_def_orgtraffic_w_conversions)

segment_seq_example <- google_analytics_4(ga_id, 
                                          date_range = c("2017-01-01","2017-03-01"), 
                                          dimensions = c('source','country'), 
                                          segments = seg_obj_orgtraffic_w_conversions,
                                          metrics = c('sessions','bounceRate', 'timeOnPage', 'goal11Completions')
)

segment_seq_example

segment_def_mktids <- "sessions::condition::ga:dimension2=@mktid"
seg_obj_mktids <- segment_ga4("test", segment_id = segment_def_mktids)

segment_seq_mktids <- google_analytics_4(ga_id, 
                                         date_range = c("2017-01-01","2017-03-01"), 
                                         dimensions = c('source','dimension2'), 
                                         segments = seg_obj_mktids,
                                         metrics = c('sessions','bounceRate', 'timeOnPage', 'goal11Completions')
)

segment_seq_mktids


google_analytics_4(ga_id, #=This is a (dynamic) ViewID parameter
                   date_range = c("2018-01-01","2018-01-30"), 
                   metrics = c("sessions", "users"), 
                   dimensions = c("deviceCategory", "sourceMedium", "date"),
                   #anti_sample = TRUE,
                   max = -1,
                   useResourceQuotas = TRUE)


# get data directly from bigquery --------------------------------------------------

project <- "api-project-929144044809"
  
get_data_query <- paste0(
  "SELECT
      date,
  device_category,
  cabin_class,
  country_ga,
  country_selection,
  # get total entry per page
  SUM(first_ent) AS Homepage,
  SUM(second_ent) AS CIB_ChooseFlight,
  SUM(third_ent) AS CIB_PassengerDetails,
  SUM(fourth_ent) AS CIB_PaymentDetails,
  SUM(fifth_ent) AS CIB_BookingConfirmation,
  # get total completion at each step
  SUM(first_cplt) AS Homepage_Complete,
  SUM(second_cplt) AS CIB_ChooseFlight_Complete,
  SUM(third_cplt) AS CIB_PassengerDetails_Complete,
  SUM(fourth_cplt) AS CIB_PaymentDetails_Complete,
  # get total drop-off at each step
  SUM(first_ent)-SUM(first_cplt) AS Homepage_Drop,
  SUM(second_ent)-SUM(second_cplt) AS CIB_ChooseFlight_Drop,
  SUM(third_ent)-SUM(third_cplt) AS CIB_PassengerDetails_Drop,
  SUM(fourth_ent)-SUM(fourth_cplt) AS CIB_PaymentDetails_Drop,
  # get direct entrance not from previous step
  SUM(second_ent)-SUM(first_cplt) AS CIB_ChooseFlight_Indirect,
  SUM(third_ent)-SUM(second_cplt) AS CIB_PassengerDetails_Indirect,
  SUM(fourth_ent)-SUM(third_cplt) AS CIB_PaymentDetails_Indirect,
  SUM(fifth_ent)-SUM(fourth_cplt) AS CIB_BookingConfirmation_Indirect,
  # add in new requested dimension
  channel,
  source,
  medium,
  campaign,
  source_medium
  FROM (
  #open funnel where a step requires ONLY the previous step
  SELECT
  a.date AS date,
  a.vid AS vid,
  a.sid AS sid,
  a.device_category AS device_category,
  # regroup cabin class value
  (CASE
  WHEN REGEXP_MATCH(b.cabin_class, r'.*ECONOMY') THEN 'ECONOMY/PREMIUM ECONOMY'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*BUSINESS') THEN 'BUSINESS'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*(FIRST|SUITE)') THEN 'FIRST'
  ELSE 'NA' END) AS cabin_class,
  b.country_ga AS country_ga,
  
  b.country_selection AS country_selection,
  b.channel AS channel,
  b.source AS source,
  b.medium AS medium,
  b.campaign AS campaign,
  b.source_medium AS source_medium,
  a.firstPage AS firstPage,
  a.secondPage AS secondPage,
  a.thirdPage AS thirdPage,
  a.fourthPage AS fourthPage,
  a.fifthPage AS fifthPage,
  # get entrance to each step
  IF(a.firstPage >0, 1,0) AS first_ent,
  IF(a.secondPage >0, 1,0) AS second_ent,
  IF(a.thirdPage >0, 1,0) AS third_ent,
  IF(a.fourthPage >0, 1,0) AS fourth_ent,
  IF(a.fifthPage >0, 1,0) AS fifth_ent,
  # get completion of each step to the next
  IF(a.firstPage > 0
  AND a.firstPage < a.secondPage,1,0) AS first_cplt,
  IF(a.secondPage > 0
  AND a.secondPage < a.thirdPage,1,0) AS second_cplt,
  IF(a.thirdPage > 0
  AND a.thirdPage < a.fourthPage,1,0) AS third_cplt,
  IF(a.fourthPage > 0
  AND a.fourthPage < a.fifthPage,1,0) AS fourth_cplt
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s4.date
  WHEN s4.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s4.vid
  WHEN s4.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s4.sid
  WHEN s4.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s4.device_category
  WHEN s4.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  IF(s0.firstPage IS NULL,0,s0.firstPage) AS firstPage,
  IF(s0.secondPage IS NULL,0,s0.secondPage) AS secondPage,
  IF(s0.thirdPage IS NULL,0,s0.thirdPage) AS thirdPage,
  IF(s0.fourthPage IS NULL,0,s0.fourthPage) AS fourthPage,
  IF(s4.firstHit IS NULL,0,s4.firstHit) AS fifthPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s3.date
  WHEN s3.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s3.vid
  WHEN s3.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s3.sid
  WHEN s3.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s3.device_category
  WHEN s3.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s0.thirdPage AS thirdPage,
  s3.firstHit AS fourthPage
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s2.date
  WHEN s2.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s2.vid
  WHEN s2.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s2.sid
  WHEN s2.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s2.device_category
  WHEN s2.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s2.firstHit AS thirdPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s1.date
  WHEN s1.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s1.vid
  WHEN s1.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s1.sid
  WHEN s1.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s1.device_category
  WHEN s1.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstHit AS firstPage,
  s1.firstHit AS secondPage
  FROM (
  # Begin Subquery #1 aka s0
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24')),
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/Homepage')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s0
  # End Subquery #1 aka s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #2 aka s1
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_ChooseFlight')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s1
  # End Subquery #2 aka s1
  ON
  s0.vid = s1.vid
  AND s0.sid = s1.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #3 aka s2
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_PassengerDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s2
  # End Subquery #3 aka s2
  ON
  s0.vid = s2.vid
  AND s0.sid= s2.sid) AS s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #4 aka s3
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_PaymentDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s3
  # End Subquery #4 aka s3
  ON
  s0.vid = s3.vid
  AND s0.sid= s3.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #5 aka s4
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_BookingConfirmation')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s4
  ON
  s0.vid = s4.vid
  AND s0.sid= s4.sid) a
  LEFT JOIN (
  SELECT
  date,
  fullVisitorId AS vid,
  visitId AS sid,
  geoNetwork.country AS country_ga,
  channelGrouping AS channel,
  trafficSource.source AS source,
  trafficSource.medium AS medium,
  trafficSource.campaign AS campaign,
  CONCAT(trafficSource.source, ' / ', trafficSource.medium) AS source_medium,
  MAX(IF(customDimensions.index = 9, customDimensions.value, NULL)) WITHIN RECORD AS country_selection,
  MAX(IF(customDimensions.index = 31, customDimensions.value, NULL)) WITHIN RECORD AS cabin_class
  
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))) b
  ON
  a.vid = b.vid
  AND a.sid = b.sid
  AND a.date = b.date)
  GROUP BY
  date,
  device_category,
  cabin_class,
  country_ga,
  country_selection,
  channel,
  source,
  medium,
  campaign,
  source_medium"
)

df_beforepos <- bq_table_download(bq_project_query(project, 
                                                   get_data_query,
                                                   use_legacy_sql = TRUE))



get_data_query2 <- paste0(
  "SELECT
  date,
  device_category,
  cabin_class,
  country_ga,
  
  # point of sale --------------------------------------------
  point_of_sale,
  # ----------------------------------------------------------
  
  country_selection,
  # get total entry per page
  SUM(first_ent) AS Homepage,
  SUM(second_ent) AS CIB_ChooseFlight,
  SUM(third_ent) AS CIB_PassengerDetails,
  SUM(fourth_ent) AS CIB_PaymentDetails,
  SUM(fifth_ent) AS CIB_BookingConfirmation,
  # get total completion at each step
  SUM(first_cplt) AS Homepage_Complete,
  SUM(second_cplt) AS CIB_ChooseFlight_Complete,
  SUM(third_cplt) AS CIB_PassengerDetails_Complete,
  SUM(fourth_cplt) AS CIB_PaymentDetails_Complete,
  # get total drop-off at each step
  SUM(first_ent)-SUM(first_cplt) AS Homepage_Drop,
  SUM(second_ent)-SUM(second_cplt) AS CIB_ChooseFlight_Drop,
  SUM(third_ent)-SUM(third_cplt) AS CIB_PassengerDetails_Drop,
  SUM(fourth_ent)-SUM(fourth_cplt) AS CIB_PaymentDetails_Drop,
  # get direct entrance not from previous step
  SUM(second_ent)-SUM(first_cplt) AS CIB_ChooseFlight_Indirect,
  SUM(third_ent)-SUM(second_cplt) AS CIB_PassengerDetails_Indirect,
  SUM(fourth_ent)-SUM(third_cplt) AS CIB_PaymentDetails_Indirect,
  SUM(fifth_ent)-SUM(fourth_cplt) AS CIB_BookingConfirmation_Indirect,
  # add in new requested dimension
  channel,
  source,
  medium,
  campaign,
  source_medium
  FROM (
  #open funnel where a step requires ONLY the previous step
  SELECT
  a.date AS date,
  a.vid AS vid,
  a.sid AS sid,
  a.device_category AS device_category,
  # regroup cabin class value
  (CASE
  WHEN REGEXP_MATCH(b.cabin_class, r'.*ECONOMY') THEN 'ECONOMY/PREMIUM ECONOMY'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*BUSINESS') THEN 'BUSINESS'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*(FIRST|SUITE)') THEN 'FIRST'
  ELSE 'NA' END) AS cabin_class,
  b.country_ga AS country_ga,
  
  # point of sale -------------------------------------------------------
  b.point_of_sale AS point_of_sale,
  # ---------------------------------------------------------------------
  
  b.country_selection AS country_selection,
  b.channel AS channel,
  b.source AS source,
  b.medium AS medium,
  b.campaign AS campaign,
  b.source_medium AS source_medium,
  a.firstPage AS firstPage,
  a.secondPage AS secondPage,
  a.thirdPage AS thirdPage,
  a.fourthPage AS fourthPage,
  a.fifthPage AS fifthPage,
  # get entrance to each step
  IF(a.firstPage >0, 1,0) AS first_ent,
  IF(a.secondPage >0, 1,0) AS second_ent,
  IF(a.thirdPage >0, 1,0) AS third_ent,
  IF(a.fourthPage >0, 1,0) AS fourth_ent,
  IF(a.fifthPage >0, 1,0) AS fifth_ent,
  # get completion of each step to the next
  IF(a.firstPage > 0
  AND a.firstPage < a.secondPage,1,0) AS first_cplt,
  IF(a.secondPage > 0
  AND a.secondPage < a.thirdPage,1,0) AS second_cplt,
  IF(a.thirdPage > 0
  AND a.thirdPage < a.fourthPage,1,0) AS third_cplt,
  IF(a.fourthPage > 0
  AND a.fourthPage < a.fifthPage,1,0) AS fourth_cplt
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s4.date
  WHEN s4.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s4.vid
  WHEN s4.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s4.sid
  WHEN s4.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s4.device_category
  WHEN s4.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  IF(s0.firstPage IS NULL,0,s0.firstPage) AS firstPage,
  IF(s0.secondPage IS NULL,0,s0.secondPage) AS secondPage,
  IF(s0.thirdPage IS NULL,0,s0.thirdPage) AS thirdPage,
  IF(s0.fourthPage IS NULL,0,s0.fourthPage) AS fourthPage,
  IF(s4.firstHit IS NULL,0,s4.firstHit) AS fifthPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s3.date
  WHEN s3.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s3.vid
  WHEN s3.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s3.sid
  WHEN s3.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s3.device_category
  WHEN s3.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s0.thirdPage AS thirdPage,
  s3.firstHit AS fourthPage
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s2.date
  WHEN s2.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s2.vid
  WHEN s2.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s2.sid
  WHEN s2.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s2.device_category
  WHEN s2.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s2.firstHit AS thirdPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s1.date
  WHEN s1.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s1.vid
  WHEN s1.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s1.sid
  WHEN s1.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s1.device_category
  WHEN s1.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstHit AS firstPage,
  s1.firstHit AS secondPage
  FROM (
  # Begin Subquery #1 aka s0
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24')),
  WHERE
  REGEXP_MATCH(hits.page.pagePath, '/Homepage')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s0
  # End Subquery #1 aka s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #2 aka s1
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_ChooseFlight')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s1
  # End Subquery #2 aka s1
  ON
  s0.vid = s1.vid
  AND s0.sid = s1.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #3 aka s2
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_PassengerDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s2
  # End Subquery #3 aka s2
  ON
  s0.vid = s2.vid
  AND s0.sid= s2.sid) AS s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #4 aka s3
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_PaymentDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s3
  # End Subquery #4 aka s3
  ON
  s0.vid = s3.vid
  AND s0.sid= s3.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #5 aka s4
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_BookingConfirmation')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s4
  ON
  s0.vid = s4.vid
  AND s0.sid= s4.sid) a
  LEFT JOIN (
  SELECT
  date,
  fullVisitorId AS vid,
  visitId AS sid,
  geoNetwork.country AS country_ga,
  channelGrouping AS channel,
  trafficSource.source AS source,
  trafficSource.medium AS medium,
  trafficSource.campaign AS campaign,
  CONCAT(trafficSource.source, ' / ', trafficSource.medium) AS source_medium,
  MAX(IF(customDimensions.index = 9, customDimensions.value, NULL)) WITHIN RECORD AS country_selection,
  MAX(IF(customDimensions.index = 31, customDimensions.value, NULL)) WITHIN RECORD AS cabin_class,
  # new CD --------------------------------------------------------------------
  MAX(IF(customDimensions.index = 22, customDimensions.value, NULL)) WITHIN RECORD AS point_of_sale
  
  # ---------------------------------------------------------------------------
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))) b
  ON
  a.vid = b.vid
  AND a.sid = b.sid
  AND a.date = b.date)
  GROUP BY
  date,
  device_category,
  cabin_class,
  country_ga,
  country_selection,
  point_of_sale,
  channel,
  source,
  medium,
  campaign,
  source_medium"
)

df_afterpos <- bq_table_download(bq_project_query(project, 
                                                   get_data_query2,
                                                   use_legacy_sql = TRUE))


get_data_query_ORB <- paste0(
  "SELECT
      date,
  device_category,
  cabin_class,
  country_ga,
  country_selection,
  # get total entry per page
  SUM(first_ent) AS Homepage,
  SUM(second_ent) AS ORB_ChooseFlight,
  SUM(third_ent) AS ORB_PassengerDetails,
  SUM(fourth_ent) AS ORB_PaymentDetails,
  SUM(fifth_ent) AS ORB_BookingConfirmation,
  # get total completion at each step
  SUM(first_cplt) AS Homepage_Complete,
  SUM(second_cplt) AS ORB_ChooseFlight_Complete,
  SUM(third_cplt) AS ORB_PassengerDetails_Complete,
  SUM(fourth_cplt) AS ORB_PaymentDetails_Complete,
  # get total drop-off at each step
  SUM(first_ent)-SUM(first_cplt) AS Homepage_Drop,
  SUM(second_ent)-SUM(second_cplt) AS ORB_ChooseFlight_Drop,
  SUM(third_ent)-SUM(third_cplt) AS ORB_PassengerDetails_Drop,
  SUM(fourth_ent)-SUM(fourth_cplt) AS ORB_PaymentDetails_Drop,
  # get direct entrance not from previous step
  SUM(second_ent)-SUM(first_cplt) AS ORB_ChooseFlight_Indirect,
  SUM(third_ent)-SUM(second_cplt) AS ORB_PassengerDetails_Indirect,
  SUM(fourth_ent)-SUM(third_cplt) AS ORB_PaymentDetails_Indirect,
  SUM(fifth_ent)-SUM(fourth_cplt) AS ORB_BookingConfirmation_Indirect,
  # add in new requested dimension
  channel,
  source,
  medium,
  campaign,
  source_medium
  FROM (
  #open funnel where a step requires ONLY the previous step
  SELECT
  a.date AS date,
  a.vid AS vid,
  a.sid AS sid,
  a.device_category AS device_category,
  # regroup cabin class value
  (CASE
  WHEN REGEXP_MATCH(b.cabin_class, r'.*ECONOMY') THEN 'ECONOMY/PREMIUM ECONOMY'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*BUSINESS') THEN 'BUSINESS'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*(FIRST|SUITE)') THEN 'FIRST'
  ELSE 'NA' END) AS cabin_class,
  b.country_ga AS country_ga,
  b.country_selection AS country_selection,
  b.channel AS channel,
  b.source AS source,
  b.medium AS medium,
  b.campaign AS campaign,
  b.source_medium AS source_medium,
  a.firstPage AS firstPage,
  a.secondPage AS secondPage,
  a.thirdPage AS thirdPage,
  a.fourthPage AS fourthPage,
  a.fifthPage AS fifthPage,
  # get entrance to each step
  IF(a.firstPage >0, 1,0) AS first_ent,
  IF(a.secondPage >0, 1,0) AS second_ent,
  IF(a.thirdPage >0, 1,0) AS third_ent,
  IF(a.fourthPage >0, 1,0) AS fourth_ent,
  IF(a.fifthPage >0, 1,0) AS fifth_ent,
  # get completion of each step to the next
  IF(a.firstPage > 0
  AND a.firstPage < a.secondPage,1,0) AS first_cplt,
  IF(a.secondPage > 0
  AND a.secondPage < a.thirdPage,1,0) AS second_cplt,
  IF(a.thirdPage > 0
  AND a.thirdPage < a.fourthPage,1,0) AS third_cplt,
  IF(a.fourthPage > 0
  AND a.fourthPage < a.fifthPage,1,0) AS fourth_cplt
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s4.date
  WHEN s4.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s4.vid
  WHEN s4.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s4.sid
  WHEN s4.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s4.device_category
  WHEN s4.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  IF(s0.firstPage IS NULL,0,s0.firstPage) AS firstPage,
  IF(s0.secondPage IS NULL,0,s0.secondPage) AS secondPage,
  IF(s0.thirdPage IS NULL,0,s0.thirdPage) AS thirdPage,
  IF(s0.fourthPage IS NULL,0,s0.fourthPage) AS fourthPage,
  IF(s4.firstHit IS NULL,0,s4.firstHit) AS fifthPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s3.date
  WHEN s3.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s3.vid
  WHEN s3.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s3.sid
  WHEN s3.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s3.device_category
  WHEN s3.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s0.thirdPage AS thirdPage,
  s3.firstHit AS fourthPage
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s2.date
  WHEN s2.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s2.vid
  WHEN s2.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s2.sid
  WHEN s2.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s2.device_category
  WHEN s2.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s2.firstHit AS thirdPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s1.date
  WHEN s1.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s1.vid
  WHEN s1.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s1.sid
  WHEN s1.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s1.device_category
  WHEN s1.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstHit AS firstPage,
  s1.firstHit AS secondPage
  FROM (
  # Begin Subquery #1 aka s0
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/Homepage')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s0
  # End Subquery #1 aka s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #2 aka s1
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_ChooseFlight')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s1
  # End Subquery #2 aka s1
  ON
  s0.vid = s1.vid
  AND s0.sid = s1.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #3 aka s2
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_PassengerDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s2
  # End Subquery #3 aka s2
  ON
  s0.vid = s2.vid
  AND s0.sid= s2.sid) AS s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #4 aka s3
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_PaymentDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s3
  # End Subquery #4 aka s3
  ON
  s0.vid = s3.vid
  AND s0.sid= s3.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #5 aka s4
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_BookingConfirmation')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s4
  ON
  s0.vid = s4.vid
  AND s0.sid= s4.sid) a
  LEFT JOIN (
  SELECT
  date,
  fullVisitorId AS vid,
  visitId AS sid,
  geoNetwork.country AS country_ga,
  channelGrouping AS channel,
  trafficSource.source AS source,
  trafficSource.medium AS medium,
  trafficSource.campaign AS campaign,
  CONCAT(trafficSource.source, ' / ', trafficSource.medium) AS source_medium,
  MAX(IF(customDimensions.index = 9, customDimensions.value, NULL)) WITHIN RECORD AS country_selection,
  MAX(IF(customDimensions.index = 31, customDimensions.value, NULL)) WITHIN RECORD AS cabin_class
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))) b
  ON
  a.vid = b.vid
  AND a.sid = b.sid
  AND a.date = b.date)
  GROUP BY
  date,
  device_category,
  cabin_class,
  country_ga,
  country_selection,
  channel,
  source,
  medium,
  campaign,
  source_medium"
)

df_ORB_beforepos <- bq_table_download(bq_project_query(project, 
                                                   get_data_query_ORB,
                                                   use_legacy_sql = TRUE))


get_data_query_ORB2 <- paste0(
  "SELECT
      date,
  device_category,
  cabin_class,
  country_ga,
  
  # point of sale --------------------------------------------
  point_of_sale,
  # ----------------------------------------------------------
  
  country_selection,
  # get total entry per page
  SUM(first_ent) AS Homepage,
  SUM(second_ent) AS ORB_ChooseFlight,
  SUM(third_ent) AS ORB_PassengerDetails,
  SUM(fourth_ent) AS ORB_PaymentDetails,
  SUM(fifth_ent) AS ORB_BookingConfirmation,
  # get total completion at each step
  SUM(first_cplt) AS Homepage_Complete,
  SUM(second_cplt) AS ORB_ChooseFlight_Complete,
  SUM(third_cplt) AS ORB_PassengerDetails_Complete,
  SUM(fourth_cplt) AS ORB_PaymentDetails_Complete,
  # get total drop-off at each step
  SUM(first_ent)-SUM(first_cplt) AS Homepage_Drop,
  SUM(second_ent)-SUM(second_cplt) AS ORB_ChooseFlight_Drop,
  SUM(third_ent)-SUM(third_cplt) AS ORB_PassengerDetails_Drop,
  SUM(fourth_ent)-SUM(fourth_cplt) AS ORB_PaymentDetails_Drop,
  # get direct entrance not from previous step
  SUM(second_ent)-SUM(first_cplt) AS ORB_ChooseFlight_Indirect,
  SUM(third_ent)-SUM(second_cplt) AS ORB_PassengerDetails_Indirect,
  SUM(fourth_ent)-SUM(third_cplt) AS ORB_PaymentDetails_Indirect,
  SUM(fifth_ent)-SUM(fourth_cplt) AS ORB_BookingConfirmation_Indirect,
  # add in new requested dimension
  channel,
  source,
  medium,
  campaign,
  source_medium
  FROM (
  #open funnel where a step requires ONLY the previous step
  SELECT
  a.date AS date,
  a.vid AS vid,
  a.sid AS sid,
  a.device_category AS device_category,
  # regroup cabin class value
  (CASE
  WHEN REGEXP_MATCH(b.cabin_class, r'.*ECONOMY') THEN 'ECONOMY/PREMIUM ECONOMY'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*BUSINESS') THEN 'BUSINESS'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*(FIRST|SUITE)') THEN 'FIRST'
  ELSE 'NA' END) AS cabin_class,
  b.country_ga AS country_ga,
  # point of sale -------------------------------------------------------
  b.point_of_sale AS point_of_sale,
  # ---------------------------------------------------------------------
  b.country_selection AS country_selection,
  b.channel AS channel,
  b.source AS source,
  b.medium AS medium,
  b.campaign AS campaign,
  b.source_medium AS source_medium,
  a.firstPage AS firstPage,
  a.secondPage AS secondPage,
  a.thirdPage AS thirdPage,
  a.fourthPage AS fourthPage,
  a.fifthPage AS fifthPage,
  # get entrance to each step
  IF(a.firstPage >0, 1,0) AS first_ent,
  IF(a.secondPage >0, 1,0) AS second_ent,
  IF(a.thirdPage >0, 1,0) AS third_ent,
  IF(a.fourthPage >0, 1,0) AS fourth_ent,
  IF(a.fifthPage >0, 1,0) AS fifth_ent,
  # get completion of each step to the next
  IF(a.firstPage > 0
  AND a.firstPage < a.secondPage,1,0) AS first_cplt,
  IF(a.secondPage > 0
  AND a.secondPage < a.thirdPage,1,0) AS second_cplt,
  IF(a.thirdPage > 0
  AND a.thirdPage < a.fourthPage,1,0) AS third_cplt,
  IF(a.fourthPage > 0
  AND a.fourthPage < a.fifthPage,1,0) AS fourth_cplt
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s4.date
  WHEN s4.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s4.vid
  WHEN s4.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s4.sid
  WHEN s4.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s4.device_category
  WHEN s4.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  IF(s0.firstPage IS NULL,0,s0.firstPage) AS firstPage,
  IF(s0.secondPage IS NULL,0,s0.secondPage) AS secondPage,
  IF(s0.thirdPage IS NULL,0,s0.thirdPage) AS thirdPage,
  IF(s0.fourthPage IS NULL,0,s0.fourthPage) AS fourthPage,
  IF(s4.firstHit IS NULL,0,s4.firstHit) AS fifthPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s3.date
  WHEN s3.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s3.vid
  WHEN s3.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s3.sid
  WHEN s3.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s3.device_category
  WHEN s3.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s0.thirdPage AS thirdPage,
  s3.firstHit AS fourthPage
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s2.date
  WHEN s2.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s2.vid
  WHEN s2.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s2.sid
  WHEN s2.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s2.device_category
  WHEN s2.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s2.firstHit AS thirdPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s1.date
  WHEN s1.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s1.vid
  WHEN s1.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s1.sid
  WHEN s1.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s1.device_category
  WHEN s1.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstHit AS firstPage,
  s1.firstHit AS secondPage
  FROM (
  # Begin Subquery #1 aka s0
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/Homepage')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s0
  # End Subquery #1 aka s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #2 aka s1
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_ChooseFlight')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s1
  # End Subquery #2 aka s1
  ON
  s0.vid = s1.vid
  AND s0.sid = s1.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #3 aka s2
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_PassengerDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s2
  # End Subquery #3 aka s2
  ON
  s0.vid = s2.vid
  AND s0.sid= s2.sid) AS s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #4 aka s3
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_PaymentDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s3
  # End Subquery #4 aka s3
  ON
  s0.vid = s3.vid
  AND s0.sid= s3.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #5 aka s4
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_BookingConfirmation')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s4
  ON
  s0.vid = s4.vid
  AND s0.sid= s4.sid) a
  LEFT JOIN (
  SELECT
  date,
  fullVisitorId AS vid,
  visitId AS sid,
  geoNetwork.country AS country_ga,
  channelGrouping AS channel,
  trafficSource.source AS source,
  trafficSource.medium AS medium,
  trafficSource.campaign AS campaign,
  CONCAT(trafficSource.source, ' / ', trafficSource.medium) AS source_medium,
  MAX(IF(customDimensions.index = 9, customDimensions.value, NULL)) WITHIN RECORD AS country_selection,
  MAX(IF(customDimensions.index = 31, customDimensions.value, NULL)) WITHIN RECORD AS cabin_class,
  # new CD --------------------------------------------------------------------
  MAX(IF(customDimensions.index = 22, customDimensions.value, NULL)) WITHIN RECORD AS point_of_sale
  
  # ---------------------------------------------------------------------------
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))) b
  ON
  a.vid = b.vid
  AND a.sid = b.sid
  AND a.date = b.date)
  GROUP BY
  date,
  device_category,
  cabin_class,
  country_ga,
  country_selection,
  point_of_sale,
  channel,
  source,
  medium,
  campaign,
  source_medium"
)

df_ORB_afterpos <- bq_table_download(bq_project_query(project, 
                                                       get_data_query_ORB2,
                                                       use_legacy_sql = TRUE))


get_data_query_ORB3 <- paste0(
  "SELECT *,
 CASE 
  WHEN country_ga IS NOT NULL THEN country_ga 
  WHEN country_ga IS NULL AND point_of_sale IS NULL THEN 'NULL' 
  WHEN point_of_sale IS NOT NULL AND country_ga IS NULL THEN point_of_sale 
  END AS POS_matched
  from(
  SELECT
  date,
  device_category,
  cabin_class,
  country_ga,
  
  # point of sale --------------------------------------------
  point_of_sale,
  # ----------------------------------------------------------
  
  country_selection,
  # get total entry per page
  SUM(first_ent) AS Homepage,
  SUM(second_ent) AS ORB_ChooseFlight,
  SUM(third_ent) AS ORB_PassengerDetails,
  SUM(fourth_ent) AS ORB_PaymentDetails,
  SUM(fifth_ent) AS ORB_BookingConfirmation,
  # get total completion at each step
  SUM(first_cplt) AS Homepage_Complete,
  SUM(second_cplt) AS ORB_ChooseFlight_Complete,
  SUM(third_cplt) AS ORB_PassengerDetails_Complete,
  SUM(fourth_cplt) AS ORB_PaymentDetails_Complete,
  # get total drop-off at each step
  SUM(first_ent)-SUM(first_cplt) AS Homepage_Drop,
  SUM(second_ent)-SUM(second_cplt) AS ORB_ChooseFlight_Drop,
  SUM(third_ent)-SUM(third_cplt) AS ORB_PassengerDetails_Drop,
  SUM(fourth_ent)-SUM(fourth_cplt) AS ORB_PaymentDetails_Drop,
  # get direct entrance not from previous step
  SUM(second_ent)-SUM(first_cplt) AS ORB_ChooseFlight_Indirect,
  SUM(third_ent)-SUM(second_cplt) AS ORB_PassengerDetails_Indirect,
  SUM(fourth_ent)-SUM(third_cplt) AS ORB_PaymentDetails_Indirect,
  SUM(fifth_ent)-SUM(fourth_cplt) AS ORB_BookingConfirmation_Indirect,
  # add in new requested dimension
  channel,
  source,
  medium,
  campaign,
  source_medium
  FROM (
  #open funnel where a step requires ONLY the previous step
  SELECT
  a.date AS date,
  a.vid AS vid,
  a.sid AS sid,
  a.device_category AS device_category,
  # regroup cabin class value
  (CASE
  WHEN REGEXP_MATCH(b.cabin_class, r'.*ECONOMY') THEN 'ECONOMY/PREMIUM ECONOMY'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*BUSINESS') THEN 'BUSINESS'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*(FIRST|SUITE)') THEN 'FIRST'
  ELSE 'NA' END) AS cabin_class,
  b.country_ga AS country_ga,
  # point of sale -------------------------------------------------------
  b.point_of_sale AS point_of_sale,
  # ---------------------------------------------------------------------
  b.country_selection AS country_selection,
  b.channel AS channel,
  b.source AS source,
  b.medium AS medium,
  b.campaign AS campaign,
  b.source_medium AS source_medium,
  a.firstPage AS firstPage,
  a.secondPage AS secondPage,
  a.thirdPage AS thirdPage,
  a.fourthPage AS fourthPage,
  a.fifthPage AS fifthPage,
  # get entrance to each step
  IF(a.firstPage >0, 1,0) AS first_ent,
  IF(a.secondPage >0, 1,0) AS second_ent,
  IF(a.thirdPage >0, 1,0) AS third_ent,
  IF(a.fourthPage >0, 1,0) AS fourth_ent,
  IF(a.fifthPage >0, 1,0) AS fifth_ent,
  # get completion of each step to the next
  IF(a.firstPage > 0
  AND a.firstPage < a.secondPage,1,0) AS first_cplt,
  IF(a.secondPage > 0
  AND a.secondPage < a.thirdPage,1,0) AS second_cplt,
  IF(a.thirdPage > 0
  AND a.thirdPage < a.fourthPage,1,0) AS third_cplt,
  IF(a.fourthPage > 0
  AND a.fourthPage < a.fifthPage,1,0) AS fourth_cplt
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s4.date
  WHEN s4.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s4.vid
  WHEN s4.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s4.sid
  WHEN s4.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s4.device_category
  WHEN s4.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  IF(s0.firstPage IS NULL,0,s0.firstPage) AS firstPage,
  IF(s0.secondPage IS NULL,0,s0.secondPage) AS secondPage,
  IF(s0.thirdPage IS NULL,0,s0.thirdPage) AS thirdPage,
  IF(s0.fourthPage IS NULL,0,s0.fourthPage) AS fourthPage,
  IF(s4.firstHit IS NULL,0,s4.firstHit) AS fifthPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s3.date
  WHEN s3.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s3.vid
  WHEN s3.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s3.sid
  WHEN s3.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s3.device_category
  WHEN s3.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s0.thirdPage AS thirdPage,
  s3.firstHit AS fourthPage
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s2.date
  WHEN s2.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s2.vid
  WHEN s2.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s2.sid
  WHEN s2.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s2.device_category
  WHEN s2.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s2.firstHit AS thirdPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s1.date
  WHEN s1.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s1.vid
  WHEN s1.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s1.sid
  WHEN s1.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s1.device_category
  WHEN s1.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstHit AS firstPage,
  s1.firstHit AS secondPage
  FROM (
  # Begin Subquery #1 aka s0
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/Homepage')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s0
  # End Subquery #1 aka s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #2 aka s1
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_ChooseFlight')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s1
  # End Subquery #2 aka s1
  ON
  s0.vid = s1.vid
  AND s0.sid = s1.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #3 aka s2
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_PassengerDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s2
  # End Subquery #3 aka s2
  ON
  s0.vid = s2.vid
  AND s0.sid= s2.sid) AS s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #4 aka s3
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_PaymentDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s3
  # End Subquery #4 aka s3
  ON
  s0.vid = s3.vid
  AND s0.sid= s3.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #5 aka s4
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/ORB_BookingConfirmation')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s4
  ON
  s0.vid = s4.vid
  AND s0.sid= s4.sid) a
  LEFT JOIN (
  SELECT
  date,
  fullVisitorId AS vid,
  visitId AS sid,
  geoNetwork.country AS country_ga,
  channelGrouping AS channel,
  trafficSource.source AS source,
  trafficSource.medium AS medium,
  trafficSource.campaign AS campaign,
  CONCAT(trafficSource.source, ' / ', trafficSource.medium) AS source_medium,
  MAX(IF(customDimensions.index = 9, customDimensions.value, NULL)) WITHIN RECORD AS country_selection,
  MAX(IF(customDimensions.index = 31, customDimensions.value, NULL)) WITHIN RECORD AS cabin_class,
  # new CD --------------------------------------------------------------------
  MAX(IF(customDimensions.index = 22, customDimensions.value, NULL)) WITHIN RECORD AS point_of_sale
  
  # ---------------------------------------------------------------------------
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))) b
  ON
  a.vid = b.vid
  AND a.sid = b.sid
  AND a.date = b.date)
  GROUP BY
  date,
  device_category,
  cabin_class,
  country_ga,
  country_selection,
  point_of_sale,
  channel,
  source,
  medium,
  campaign,
  source_medium) as fulltable
  LEFT JOIN ( 
  SELECT 
  * 
  FROM 
  [api-project-929144044809:46948678.CountryByteCode]) CountryByteCode 
  ON 
  fulltable.point_of_sale = CountryByteCode.Country_Code"
)

df_ORB_joinpos <- bq_table_download(bq_project_query(project, 
                                                       get_data_query_ORB3,
                                                       use_legacy_sql = TRUE))


get_data_query_CIB4 <- paste0(
  "SELECT *,
 CASE 
  WHEN country_ga IS NOT NULL THEN country_ga 
  WHEN country_ga IS NULL AND point_of_sale IS NULL THEN 'NULL' 
  WHEN point_of_sale IS NOT NULL AND country_ga IS NULL THEN point_of_sale 
  END AS POS_matched
  from(
  SELECT
  date,
  device_category,
  cabin_class,
  country_ga,
  
  # point of sale --------------------------------------------
  point_of_sale,
  # ----------------------------------------------------------
  
  country_selection,
  # get total entry per page
  SUM(first_ent) AS Homepage,
  SUM(second_ent) AS CIB_ChooseFlight,
  SUM(third_ent) AS CIB_PassengerDetails,
  SUM(fourth_ent) AS CIB_PaymentDetails,
  SUM(fifth_ent) AS CIB_BookingConfirmation,
  # get total completion at each step
  SUM(first_cplt) AS Homepage_Complete,
  SUM(second_cplt) AS CIB_ChooseFlight_Complete,
  SUM(third_cplt) AS CIB_PassengerDetails_Complete,
  SUM(fourth_cplt) AS CIB_PaymentDetails_Complete,
  # get total drop-off at each step
  SUM(first_ent)-SUM(first_cplt) AS Homepage_Drop,
  SUM(second_ent)-SUM(second_cplt) AS CIB_ChooseFlight_Drop,
  SUM(third_ent)-SUM(third_cplt) AS CIB_PassengerDetails_Drop,
  SUM(fourth_ent)-SUM(fourth_cplt) AS CIB_PaymentDetails_Drop,
  # get direct entrance not from previous step
  SUM(second_ent)-SUM(first_cplt) AS CIB_ChooseFlight_Indirect,
  SUM(third_ent)-SUM(second_cplt) AS CIB_PassengerDetails_Indirect,
  SUM(fourth_ent)-SUM(third_cplt) AS CIB_PaymentDetails_Indirect,
  SUM(fifth_ent)-SUM(fourth_cplt) AS CIB_BookingConfirmation_Indirect,
  # add in new requested dimension
  channel,
  source,
  medium,
  campaign,
  source_medium
  FROM (
  #open funnel where a step requires ONLY the previous step
  SELECT
  a.date AS date,
  a.vid AS vid,
  a.sid AS sid,
  a.device_category AS device_category,
  # regroup cabin class value
  (CASE
  WHEN REGEXP_MATCH(b.cabin_class, r'.*ECONOMY') THEN 'ECONOMY/PREMIUM ECONOMY'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*BUSINESS') THEN 'BUSINESS'
  WHEN REGEXP_MATCH(b.cabin_class, r'.*(FIRST|SUITE)') THEN 'FIRST'
  ELSE 'NA' END) AS cabin_class,
  b.country_ga AS country_ga,
  
  # point of sale -------------------------------------------------------
  b.point_of_sale AS point_of_sale,
  # ---------------------------------------------------------------------
  
  b.country_selection AS country_selection,
  b.channel AS channel,
  b.source AS source,
  b.medium AS medium,
  b.campaign AS campaign,
  b.source_medium AS source_medium,
  a.firstPage AS firstPage,
  a.secondPage AS secondPage,
  a.thirdPage AS thirdPage,
  a.fourthPage AS fourthPage,
  a.fifthPage AS fifthPage,
  # get entrance to each step
  IF(a.firstPage >0, 1,0) AS first_ent,
  IF(a.secondPage >0, 1,0) AS second_ent,
  IF(a.thirdPage >0, 1,0) AS third_ent,
  IF(a.fourthPage >0, 1,0) AS fourth_ent,
  IF(a.fifthPage >0, 1,0) AS fifth_ent,
  # get completion of each step to the next
  IF(a.firstPage > 0
  AND a.firstPage < a.secondPage,1,0) AS first_cplt,
  IF(a.secondPage > 0
  AND a.secondPage < a.thirdPage,1,0) AS second_cplt,
  IF(a.thirdPage > 0
  AND a.thirdPage < a.fourthPage,1,0) AS third_cplt,
  IF(a.fourthPage > 0
  AND a.fourthPage < a.fifthPage,1,0) AS fourth_cplt
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s4.date
  WHEN s4.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s4.vid
  WHEN s4.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s4.sid
  WHEN s4.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s4.device_category
  WHEN s4.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  IF(s0.firstPage IS NULL,0,s0.firstPage) AS firstPage,
  IF(s0.secondPage IS NULL,0,s0.secondPage) AS secondPage,
  IF(s0.thirdPage IS NULL,0,s0.thirdPage) AS thirdPage,
  IF(s0.fourthPage IS NULL,0,s0.fourthPage) AS fourthPage,
  IF(s4.firstHit IS NULL,0,s4.firstHit) AS fifthPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s3.date
  WHEN s3.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s3.vid
  WHEN s3.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s3.sid
  WHEN s3.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s3.device_category
  WHEN s3.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s0.thirdPage AS thirdPage,
  s3.firstHit AS fourthPage
  FROM (
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s2.date
  WHEN s2.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s2.vid
  WHEN s2.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s2.sid
  WHEN s2.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s2.device_category
  WHEN s2.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstPage AS firstPage,
  s0.secondPage AS secondPage,
  s2.firstHit AS thirdPage from(
  SELECT
  (CASE
  WHEN s0.date IS NULL THEN s1.date
  WHEN s1.date IS NULL THEN s0.date
  ELSE s0.date END) AS date,
  (CASE
  WHEN s0.vid IS NULL THEN s1.vid
  WHEN s1.vid IS NULL THEN s0.vid
  ELSE s0.vid END) AS vid,
  (CASE
  WHEN s0.sid IS NULL THEN s1.sid
  WHEN s1.sid IS NULL THEN s0.sid
  ELSE s0.sid END) AS sid,
  (CASE
  WHEN s0.device_category IS NULL THEN s1.device_category
  WHEN s1.device_category IS NULL THEN s0.device_category
  ELSE s0.device_category END) AS device_category,
  s0.firstHit AS firstPage,
  s1.firstHit AS secondPage
  FROM (
  # Begin Subquery #1 aka s0
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24')),
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/Homepage')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s0
  # End Subquery #1 aka s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #2 aka s1
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_ChooseFlight')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s1
  # End Subquery #2 aka s1
  ON
  s0.vid = s1.vid
  AND s0.sid = s1.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #3 aka s2
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_PassengerDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s2
  # End Subquery #3 aka s2
  ON
  s0.vid = s2.vid
  AND s0.sid= s2.sid) AS s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #4 aka s3
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_PaymentDetails')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s3
  # End Subquery #4 aka s3
  ON
  s0.vid = s3.vid
  AND s0.sid= s3.sid) s0
  FULL OUTER JOIN EACH (
  # Begin Subquery #5 aka s4
  SELECT
  fullVisitorId AS vid,
  visitId AS sid,
  date,
  device.deviceCategory AS device_category,
  MIN(hits.hitNumber) AS firstHit
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))
  WHERE
  REGEXP_MATCH(hits.page.pagePath, r'/CIB_BookingConfirmation')
  AND totals.visits = 1
  GROUP BY
  vid,
  sid,
  date,
  device_category) s4
  ON
  s0.vid = s4.vid
  AND s0.sid= s4.sid) a
  LEFT JOIN (
  SELECT
  date,
  fullVisitorId AS vid,
  visitId AS sid,
  geoNetwork.country AS country_ga,
  channelGrouping AS channel,
  trafficSource.source AS source,
  trafficSource.medium AS medium,
  trafficSource.campaign AS campaign,
  CONCAT(trafficSource.source, ' / ', trafficSource.medium) AS source_medium,
  MAX(IF(customDimensions.index = 9, customDimensions.value, NULL)) WITHIN RECORD AS country_selection,
  MAX(IF(customDimensions.index = 31, customDimensions.value, NULL)) WITHIN RECORD AS cabin_class,
  # new CD --------------------------------------------------------------------
  MAX(IF(customDimensions.index = 22, customDimensions.value, NULL)) WITHIN RECORD AS point_of_sale
  
  # ---------------------------------------------------------------------------
  FROM
  TABLE_DATE_RANGE([api-project-929144044809:46948678.ga_sessions_], 
  TIMESTAMP('2018-07-22'), TIMESTAMP('2018-07-24'))) b
  ON
  a.vid = b.vid
  AND a.sid = b.sid
  AND a.date = b.date)
  GROUP BY
  date,
  device_category,
  cabin_class,
  country_ga,
  country_selection,
  point_of_sale,
  channel,
  source,
  medium,
  campaign,
  source_medium) as fulltable
  LEFT JOIN ( 
  SELECT 
  * 
  FROM 
  [api-project-929144044809:46948678.CountryByteCode]) CountryByteCode 
  ON 
  fulltable.point_of_sale = CountryByteCode.Country_Code"
)

df_CIB_joinpos <- bq_table_download(bq_project_query(project, 
                                                       get_data_query_CIB4,
                                                       use_legacy_sql = TRUE))

cib <- df_CIB_joinpos %>% 
  group_by(POS_matched) %>% 
  summarise(Homepage = sum(fulltable_Homepage)) %>% 
  mutate(percent = round(Homepage / sum(Homepage),2))

orb <- df_ORB_joinpos %>% 
  group_by(POS_matched, fulltable_device_category) %>% 
  summarise(Homepage = sum(fulltable_Homepage)) %>% 
  mutate(percent = round(Homepage / sum(Homepage),2))

old_orb <- df_ORB_afterpos %>% 
  mutate(POS = case_when(!is.na(country_ga) ~ country_ga,
                         is.na(country_ga) & is.na(point_of_sale) ~ 'null',
                         !is.na(point_of_sale) & is.na(country_ga) ~ point_of_sale)) %>% 
  #filter(POS == 'Australia') %>% 
  group_by(POS, device_category) %>% 
  summarise(Homepage = sum(Homepage))


# Management API -------------------------------------
accountlist_rownumber <- 1723
mgmt_viewid <- account_list[accountlist_rownumber,'viewId']

# Management API (adwords) -------------------------------------

adwords_listing <- ga_adwords_list(account_list[accountlist_rownumber,'accountId'],
                account_list[accountlist_rownumber,'webPropertyId'])
adwords_listing <- as.data.frame(adwords_listing$items)

ga_adwords(account_list[accountlist_rownumber,'accountId'],
           account_list[accountlist_rownumber,'webPropertyId'])


# Management API (custom data source) -------------------------------------
custom_datasources <- ga_custom_datasource(account_list[accountlist_rownumber,'accountId'],
                     account_list[accountlist_rownumber,'webPropertyId'])


# Management API (Experiments) -------------------------------------
experiments <- ga_experiment_list(account_list[accountlist_rownumber,'accountId'],
              account_list[accountlist_rownumber,'webPropertyId'],
              account_list[accountlist_rownumber,'viewId'])
experiments <- as.data.frame(experiments$items)

# Management API (filters) -------------------------------------
filter_list <- ga_filter_view_list(account_list[accountlist_rownumber,'accountId'],
                    account_list[accountlist_rownumber,'webPropertyId'],
                    account_list[accountlist_rownumber,'viewId'])
filter_list <- as.data.frame(filter_list$items)

