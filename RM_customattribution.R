library(tidyverse)
library(bigrquery)
library(lubridate)
library(fst)

# to do: change weights in attribution data
# 16/10/2018: linear attribution weights added 
# first / mid / last interaction order is weighted number of transactions

# write.fst(clickstream_query_data, "clickstream_query_data.fst")
# write.fst(attribution_query_data, "attribution_query_data.fst")
clickstream_query_data <- read.fst("clickstream_query_data.fst")
attribution_query_data <- read.fst("attribution_query_data.fst")

credentials <- read.delim("RM_customattribution.txt", sep= "", header = TRUE)
credentials <- credentials %>% mutate_if(is.factor, as.character)

# Big Query Setting ----------------------------------------------------
project <- as.character(credentials[credentials$Details =='project',][2])
dataset <- as.character(credentials[credentials$Details =='dataset',][2])
clickstream_gaexport_table <- as.character(credentials[credentials$Details =='clickstream_gaexport_table',][2])
tablename_withsuffixwildcard <- as.character(credentials[credentials$Details =='tablename_withsuffixwildcard',][2])

# get clickstream data -------------------------------------------------

clickstream_query <- paste0(
  "SELECT
  CASE WHEN max_interaction = interaction THEN 'Y' else 'N' END AS is_last_click, 
  *
  FROM
  (
  SELECT
  max(interaction) over(partition by fullVisitorId, nth_transaction) as max_interaction, 
  LAST_VALUE(transactionid) OVER (PARTITION BY fullVisitorId, nth_transaction 
  ORDER BY interaction
  ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS transacion_id_final,
  LAST_VALUE(hits_datetime_SGT) OVER (PARTITION BY fullVisitorId, nth_transaction 
  ORDER BY interaction
  ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS transaction_time,
  LAST_VALUE(hit_customVarName) OVER (PARTITION BY fullVisitorId, nth_transaction 
  ORDER BY interaction
  ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS customVarName_final,
  LAST_VALUE(hit_customVarValue) OVER (PARTITION BY fullVisitorId, nth_transaction 
  ORDER BY interaction
  ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS customVarValue_final,
  *
  FROM(
  SELECT
  ROW_NUMBER() OVER (PARTITION BY fullVisitorId, nth_transaction ORDER BY VisitStartTime, hits_datetime) AS interaction,
  timestamp_add(TIMESTAMP_SECONDS(CAST(visitStartTime + hits_datetime/1000 AS INT64)), interval 8 hour) AS hits_datetime_SGT,
  timestamp_add(TIMESTAMP_SECONDS(CAST(visitStartTime AS INT64)), interval 8 hour) AS VisitStartTime_SGT,
  *
  FROM(
  SELECT
  CASE
  WHEN transactionid IS NULL THEN transactions_in_cj+1
  ELSE transactions_in_cj
  END AS nth_transaction,
  *
  FROM (
  
  SELECT
  SUM(CASE
  WHEN transactionid IS NULL THEN 0
  ELSE 1 END) OVER (PARTITION BY fullVisitorId ORDER BY VisitStartTime, hits_datetime ) AS transactions_in_cj,
  CASE WHEN isTrueDirect = true THEN 'Direct' ELSE channel_temp END AS channel,
  *
  from(
  select * from
  (SELECT
  fullVisitorId,
  visitNumber,
  visitId,
  VisitStartTime,
  ARRAY_AGG(hitnumber ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc LIMIT 1)[offset(0)] AS hit_hitNumber,
  transactionid,
  ARRAY_AGG(hit_type  ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc LIMIT 1)[offset (0)] AS hit_type,
  ARRAY_AGG(hits_time ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc  limit 1)[offset (0)] AS hits_datetime,
  ARRAY_AGG(trafficsource_source ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc  limit 1)[offset (0)] AS traffic_source,
  ARRAY_AGG(trafficsource_medium ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc  limit 1)[offset (0)] AS traffic_medium,
  ARRAY_AGG(trafficsource_campaign ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc  limit 1)[offset (0)] AS traffic_campaign,
  ARRAY_AGG(trafficsource_adcontent ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc , rn desc limit 1)[offset (0)] AS traffic_adcontent,
  ARRAY_AGG(trafficsource_keyword ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc  limit 1)[offset (0)] AS traffic_keyword,
  ARRAY_AGG(trafficsource_referralpath ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc , rn desc limit 1)[offset (0)] AS traffic_referralpath,
  ARRAY_AGG(channelGrouping ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc , rn desc limit 1)[offset (0)] AS channel_temp,
  ARRAY_AGG(device_devicecategory ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc  limit 1)[offset (0)] AS device_category,
  ARRAY_AGG(geoNetwork_country ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time  desc, rn  desc limit 1)[offset (0)] AS Network_country,
  ARRAY_AGG(geoNetwork_region ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc   limit 1)[offset (0)] AS Network_region,
  ARRAY_AGG(totals_bounces ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc  limit 1)[offset (0)] AS bounce,
  ARRAY_AGG(trafficSource_isTrueDirect ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc, rn desc  limit 1)[offset (0)] isTrueDirect,
  ARRAY_AGG(customVarName ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time desc , rn desc  limit 1)[offset (0)] hit_customVarName,
  ARRAY_AGG(customVarValue ORDER BY fullVisitorId, visitStartTime ASC, transactionid DESC, hits_time  desc, rn desc limit 1)[offset (0)] hit_customVarValue,
  SUM(CASE WHEN transactionId IS NOT NULL THEN 1 ELSE 0 END) OVER (PARTITION BY fullVisitorId, visitNumber) as transaction_count
  from(  select *, ROW_NUMBER() OVER() as rn from(   SELECT
  fullVisitorId,
  visitNumber,
  hits.hitNumber as hitnumber,
  visitId,
  hits.transaction.transactionid as transactionid,
  hits.type as hit_type,
  VisitStartTime,
  hits.time as hits_time,
  trafficsource.source as trafficsource_source,
  trafficsource.medium as trafficsource_medium,
  trafficsource.campaign as trafficsource_campaign,
  trafficsource.adcontent as trafficsource_adcontent,
  trafficsource.keyword as trafficsource_keyword,
  trafficsource.referralpath as trafficsource_referralpath,
  device.devicecategory as device_devicecategory,
  hits.page.pagepath,
  geoNetwork.country as geoNetwork_country,
  geoNetwork.region as geoNetwork_region,
  hits.appinfo.name,
  channelGrouping,
  totals.bounces as totals_bounces,
  trafficSource.isTrueDirect as trafficSource_isTrueDirect ,
  hcv.customVarName as customVarName,
  hcv.customVarValue as customVarValue 
  FROM `", tablename_withsuffixwildcard, "` t, t.hits as hits left join unnest(hits.customVariables) as hcv
  WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE_SUB(CURRENT_DATE(), INTERVAL 91 DAY))
  AND FORMAT_DATE('%Y%m%d', DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY)) 
  AND fullVisitorId IN (
  SELECT
  fullVisitorId
  FROM `", tablename_withsuffixwildcard, "` t, t.hits as hits left join unnest(hits.customVariables) as hcv
  WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY))
  AND FORMAT_DATE('%Y%m%d', DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY)) 
  AND
  hits.transaction.transactionId IS NOT NULL )
  AND (hits.hitNumber=1
  OR (hits.transaction.transactionId IS NOT NULL AND hcv.customVarName = 'isNew'))
  AND ( hits.transaction.transactionId IS NOT NULL ) )
  order by
  fullVisitorId,
  visitStartTime ASC,
  transactionid DESC,
  hits_time)
  GROUP  BY
  fullVisitorId,
  visitNumber,
  visitId,
  VisitStartTime,
  transactionid)
  WHERE (hit_hitNumber = 1 AND transaction_count=0) or (hit_hitNumber >1 AND transaction_count>0)
  )
  )
  )
  )
  ) WHERE transaction_time >= timestamp(date_sub(CURRENT_DATE(), INTERVAL 1 day))
  
  ORDER BY
  fullVisitorId,
  VisitStartTime,
  hits_datetime
  "
)

clickstream_query_data <-
  bq_table_download(bq_project_query(project, clickstream_query))


# Variables for the BigQuery upload portion ----------------------------------------------
destinationproject <- project
destinationdataset <- dataset
tablename <- "clickstream_data"



# Check if the table exists, if table exists, then delete the table ------------------------
tryCatch(
  bq_table_delete(bq_table(
    destinationproject, destinationdataset, tablename
  )),
  error = function(e) {
    print(paste0(tablename, " not available for deletion"))
  }
)

# Upload the table into big query -----------------------------
tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    tablename,
    clickstream_query_data
  ),
  error = function(e) {
    print(paste0(clickstream_query_data, " failed to upload"))
  }
)


# get attribution data -------------------------------------------------

attribution_query <- paste0(
  "SELECT
  (case when transacion_id_final  is not null then 1 else 0 end) / max_interaction as linear_weight,
  CASE WHEN weight = 0
  THEN 0
  ELSE order_b4_normalization / weight
  END as attr_order,
  *
  FROM
  (
  SELECT
  first_interaction_order + mid_interaction_order + last_interaction_order as order_b4_normalization,
  SUM(first_interaction_order + mid_interaction_order + last_interaction_order) OVER (PARTITION BY transacion_id_final) as weight,
  *
  FROM
  (
  SELECT
  CASE WHEN interaction = 1
  THEN 0.4 * attrFactor
  ELSE 0
  END as first_interaction_order,
  CASE 
  WHEN max_interaction = 2
  THEN 0.1
  WHEN max_interaction = 1
  THEN 0.2
  WHEN 1 < interaction AND interaction < max_interaction
  THEN 0.2/(max_interaction - 2)
  ELSE 0
  END * attrFactor as mid_interaction_order,
  
  CASE WHEN is_last_click = 'Y'
  THEN 0.4 * attrFactor
  ELSE 0
  END as last_interaction_order,
  *
  FROM
  (
  SELECT  
  CASE WHEN channel = 'Direct' and interaction != 1 THEN 0 
  WHEN channel = 'SEM Brand' and is_last_click = 'Y' THEN 0.1
  WHEN channel = 'SEO Brand' and is_last_click = 'Y' THEN 0.1
  WHEN traffic_medium like  '%Affiliate%' AND is_last_click = 'Y' THEN 0.1
  WHEN traffic_medium like  '%Affiliate%' AND bounce = 1 THEN 0
  ELSE 1
  END as attrFactor,
  *
  FROM `", clickstream_gaexport_table ,"`
  # ----------------- to be added if date range is present ---------------- 
  # WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', date_sub(CURRENT_DATE(), INTERVAL 1 DAY))
  # AND FORMAT_DATE('%Y%m%d', date_sub(CURRENT_DATE(), INTERVAL 1 DAY))
  )
  )
  )
  ORDER BY
  fullVisitorId,
  VisitStartTime,
  hits_datetime"
)

attribution_query_data <-
  bq_table_download(bq_project_query(project, attribution_query))


# Variables for the BigQuery upload portion ----------------------------------------------
attribution_destinationproject <- project
attribution_destinationdataset <- dataset
attribution_tablename <- "attribution_data"



# Check if the table exists, if table exists, then delete the table ------------------------
tryCatch(
  bq_table_delete(bq_table(
    attribution_destinationproject, attribution_destinationdataset, attribution_tablename
  )),
  error = function(e) {
    print(paste0(attribution_tablename, " not available for deletion"))
  }
)

# Upload the table into big query -----------------------------
tryCatch(
  insert_upload_job(
    attribution_destinationproject,
    attribution_destinationdataset,
    attribution_tablename,
    attribution_query_data
  ),
  error = function(e) {
    print(paste0(attribution_query_data, " failed to upload"))
  }
)


