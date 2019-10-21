library(bupaR)
library(tidyverse)
library(bigrquery)

# https://www.bupar.net/processmaps.html


patients

patients %>%
  process_map(type = frequency("absolute"))




project <- "api-project-929144044809"


get_data_query <- paste0(
  "select fullvisitorid as fullvisitorid_caseid,
hits.page.pagepath pagepath_activityid,
'start' as status_lifecycleid,
TIMESTAMP_SECONDS(visitStarttime + hits.time) as hit_timestamp,
device.deviceCategory deviceCategory_resourceid,
RANK() OVER (ORDER BY concat(fullVisitorId, cast(visitId as string), cast(hits.hitNumber as string)) asc) as pagepath_activity_instance_id
FROM `api-project-929144044809.46948678.ga_sessions_*` t left join unnest(hits) as hits
WHERE  _TABLE_SUFFIX BETWEEN '20190915'
AND '20190915'
AND hits.page.pagepath in('/Homepage', 
        '/CIB_ChooseFlight', 
        '/CIB_PassengerDetails', 
        '/CIB_ShoppingCart', 
        '/seatSelection.jsp', 
        '/CIB_PaymentDetails', 
        '/CIB_BookingConfirmation')
        AND hits.type = 'PAGE'
        group by 1,2,3,4,5, visitid, hits.hitNumber")


df_log <- bq_table_download(bq_project_query(project,
                                             get_data_query,
                                             use_legacy_sql = TRUE))


query_results <- read.csv("df_log.csv")
query_results <- query_results %>% 
  mutate(hit_timestamp = as.POSIXct(hit_timestamp),
         fullvisitorid_caseid = as.character(fullvisitorid_caseid))

log <- query_results %>%
  eventlog(
    case_id = "fullvisitorid_caseid",    # the user id (browser cookie)
    activity_id = "pagepath_activityid", # this contains the page name that is viewd
    activity_instance_id = "pagepath_activity_instance_id", # all user activity from that page view
    lifecycle_id = "status_lifecycleid",    # page activity: one of 'start' or 'end' per page (funnel-step)
    timestamp = "hit_timestamp",
    resource_id = "deviceCategory_resourceid"  # I fill this with device_type
  )

# show activities
activity_log <- activities(log)

