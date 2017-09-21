library (plyr)
library (dplyr)
library (tidyr)
library (bigrquery)
library (lubridate)

# Big Query Setting
project <- "api-project-XXXX"
dataset <- "YYYY"

# auto mode to get data of yesterday
start_date <- today()-1
end_date <- today()-1

# check if yesterday table exists in the normal or intraday table
yesterday <- today()-1
yesterday_intra_table <- paste0("ga_sessions_intraday_", gsub('-', '', yesterday))
yesterday_normal_table <- paste0("ga_sessions_", gsub('-', '', yesterday))
check <- exists_table(project, dataset, yesterday_intra_table)

# define the BQ table to grab the data from
table <- ifelse(check == TRUE, paste0(dataset,".",yesterday_intra_table), paste0(dataset,".",yesterday_normal_table))

# get the data by each date

cib_get_funnel <- function(dateStart, dateEnd){
  
  cib_funnel_query <- paste0("
                             SELECT
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
                             channel, source, medium, campaign, source_medium
                             FROM (
                             #open funnel where a step requires ONLY the previous step
                             SELECT
                             a.date AS date,
                             a.vid AS vid,
                             a.sid AS sid,
                             a.device_category AS device_category,
                             # regroup cabin class value
                             (CASE WHEN REGEXP_MATCH(b.cabin_class, r'.*ECONOMY') THEN 'ECONOMY/PREMIUM ECONOMY' WHEN REGEXP_MATCH(b.cabin_class, r'.*BUSINESS') THEN 'BUSINESS' WHEN REGEXP_MATCH(b.cabin_class, r'.*(FIRST|SUITE)') THEN 'FIRST' ELSE 'NA' END) AS cabin_class,
                             b.country_ga as country_ga, 
                             b.country_selection as country_selection,
                             b.channel as channel, b.source as source, b.medium as medium, b.campaign as campaign, b.source_medium as source_medium,
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
                             (CASE WHEN s0.date IS NULL THEN s4.date WHEN s4.date IS NULL THEN s0.date ELSE s0.date END) AS date,
                             (CASE WHEN s0.vid IS NULL THEN s4.vid WHEN s4.vid IS NULL THEN s0.vid ELSE s0.vid END) AS vid,
                             (CASE WHEN s0.sid IS NULL THEN s4.sid WHEN s4.sid IS NULL THEN s0.sid ELSE s0.sid END) AS sid,
                             (CASE WHEN s0.device_category IS NULL THEN s4.device_category WHEN s4.device_category IS NULL THEN s0.device_category ELSE s0.device_category END) AS device_category,
                             IF(s0.firstPage IS NULL,0,s0.firstPage) AS firstPage,
                             IF(s0.secondPage IS NULL,0,s0.secondPage) AS secondPage,
                             IF(s0.thirdPage IS NULL,0,s0.thirdPage) AS thirdPage,
                             IF(s0.fourthPage IS NULL,0,s0.fourthPage) AS fourthPage,
                             IF(s4.firstHit IS NULL,0,s4.firstHit) AS fifthPage from(
                             SELECT
                             (CASE WHEN s0.date IS NULL THEN s3.date WHEN s3.date IS NULL THEN s0.date ELSE s0.date END) AS date,
                             (CASE WHEN s0.vid IS NULL THEN s3.vid WHEN s3.vid IS NULL THEN s0.vid ELSE s0.vid END) AS vid,
                             (CASE WHEN s0.sid IS NULL THEN s3.sid WHEN s3.sid IS NULL THEN s0.sid ELSE s0.sid END) AS sid,
                             (CASE WHEN s0.device_category IS NULL THEN s3.device_category WHEN s3.device_category IS NULL THEN s0.device_category ELSE s0.device_category END) AS device_category,
                             s0.firstPage AS firstPage,
                             s0.secondPage AS secondPage,
                             s0.thirdPage AS thirdPage,
                             s3.firstHit AS fourthPage
                             FROM (
                             SELECT
                             (CASE WHEN s0.date IS NULL THEN s2.date WHEN s2.date IS NULL THEN s0.date ELSE s0.date END) AS date,
                             (CASE WHEN s0.vid IS NULL THEN s2.vid WHEN s2.vid IS NULL THEN s0.vid ELSE s0.vid END) AS vid,
                             (CASE WHEN s0.sid IS NULL THEN s2.sid WHEN s2.sid IS NULL THEN s0.sid ELSE s0.sid END) AS sid,
                             (CASE WHEN s0.device_category IS NULL THEN s2.device_category WHEN s2.device_category IS NULL THEN s0.device_category ELSE s0.device_category END) AS device_category,
                             s0.firstPage AS firstPage,
                             s0.secondPage AS secondPage,
                             s2.firstHit AS thirdPage from(
                             SELECT
                             (CASE WHEN s0.date IS NULL THEN s1.date WHEN s1.date IS NULL THEN s0.date ELSE s0.date END) AS date,
                             (CASE WHEN s0.vid IS NULL THEN s1.vid WHEN s1.vid IS NULL THEN s0.vid ELSE s0.vid END) AS vid,
                             (CASE WHEN s0.sid IS NULL THEN s1.sid WHEN s1.sid IS NULL THEN s0.sid ELSE s0.sid END) AS sid,
                             (CASE WHEN s0.device_category IS NULL THEN s1.device_category WHEN s1.device_category IS NULL THEN s0.device_category ELSE s0.device_category END) AS device_category,
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
                             [", table ,"]
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
                             [", table ,"]
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
                             [", table ,"]
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
                             [", table ,"]
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
                             [", table ,"]
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
                             geoNetwork.country as country_ga,
                             channelGrouping as channel,
                             trafficSource.source as source,
                             trafficSource.medium as medium,
                             trafficSource.campaign as campaign,
                             CONCAT(trafficSource.source,' / ', trafficSource.medium) AS source_medium,
                             MAX(IF(customDimensions.index = 9, customDimensions.value, NULL)) WITHIN RECORD as country_selection,
                             MAX(IF(customDimensions.index = 31, customDimensions.value, NULL)) WITHIN RECORD as cabin_class
                             FROM
                             [", table ,"]) b
                             ON
                             a.vid = b.vid
                             AND a.sid = b.sid
                             AND a.date = b.date)
                             GROUP BY
                             date,
                             device_category,
                             cabin_class, country_ga, country_selection, channel, source, medium, campaign, source_medium")
  
  cib_funnel_data <- query_exec(cib_funnel_query, project, destination_table = NULL, max_pages = Inf)
  return(cib_funnel_data)
}

cib_funnel_data <- cib_get_funnel(start_date, end_date)

# Variables for the BigQuery upload portion
destinationProject <- 'api-project-XXXXXX'
destinationDataset <- 'Funnel_DS'
reportName <- paste0('CIB_Open_Funnel_', gsub('-', '', start_date)) 

# Check if the table exists, if table exists, then delete the table
tryCatch(delete_table(destinationProject, destinationDataset, reportName),
         error = function(e){
           print(paste0(reportName, " not available for deletion"))
         })

# Upload the table into big query
tryCatch(insert_upload_job(destinationProject, destinationDataset, reportName, cib_funnel_data),
         error = function(e){
           print(paste0(reportName, " failed to upload"))
         })
