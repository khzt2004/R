library(bigrquery)
library(tidyverse)


project <- "bikeshare-165209" # put your projectID here

#sql <- 'SELECT
#visitId, hits.hitNumber, date,
#MAX(IF(hits.customDimensions.index = 1,
#       hits.customDimensions.value, NULL))
#WITHIN hits.customDimensions Item,
#MAX(IF(hits.customDimensions.index = 3,
#       hits.customDimensions.value, NULL))
#WITHIN hits.customDimensions Level
#FROM [google.com:analytics-bigquery:LondonCycleHelmet.ga_sessions_20130910]
#LIMIT 100'

sql <- 'SELECT 
fullVisitorId, 
visitID, 
visitNumber, 
hits.hitNumber, 
hits.page.pagePath, 
hits.type, 
hits.transaction.transactionRevenue, 
hits.eventInfo.eventCategory,
hits.eventInfo.eventAction,
hits.eventInfo.eventLabel,
hits.customDimensions.index,
hits.customDimensions.value
from [google.com:analytics-bigquery:LondonCycleHelmet.ga_sessions_20130910]'

data1 <- query_exec(sql, project = project)

write_csv(data1, "bigquery.csv")
