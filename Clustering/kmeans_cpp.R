library(ClusterR)
library(tidyverse)
library(summarytools)
library(bigrquery)



df_csv <- read_csv("clustering_testexport.csv")
df <- df_csv %>% 
  select(-fullVisitorId) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate_all(funs(as.numeric))
df <- as.data.frame(df)


# take a peek at their data for quick summary stats --------------------------------
view(dfSummary(df),  file = "clustering_df.html")

# get data directly from bigquery --------------------------------------------------
#get_data_query <- paste0(
  "SELECT fullVisitorId,
case when device.deviceCategory = 'desktop' then sum(totals.visits) else 0 end as device_desktop,
  case when device.deviceCategory = 'mobile' then sum(totals.visits) else 0 end as device_mobile,
  case when device.deviceCategory = 'tablet' then sum(totals.visits) else 0 end as device_tablet,
  sum(totals.transactions) as no_of_txns,
  sum(totals.transactionRevenue ) as transactionRevenue,
  case when channelGrouping = 'Referral' then sum(totals.visits ) else 0 end as channel_referral,
  case when channelGrouping = 'Social' then sum(totals.visits ) else 0 end as channel_social,
  case when channelGrouping = 'Direct' then sum(totals.visits ) else 0 end as channel_direct,
  case when channelGrouping = 'Organic Search' then sum(totals.visits ) else 0 end as channel_organicsearch,
  case when channelGrouping = 'Paid Search' then sum(totals.visits ) else 0 end as channel_paidsearch,
  case when channelGrouping = 'Display' then sum(totals.visits ) else 0 end as channel_display,
  case when channelGrouping = 'Paid Social' then sum(totals.visits ) else 0 end as channel_paidsocial,
  case when hcd.index = 6 and 
  hcd.value = 'editorial' then sum(totals.visits ) else 0 end as editorialtype_editorial,
  case when hcd.index = 6 and 
  hcd.value = 'advertorial' then sum(totals.visits ) else 0 end as editorialtype_advertorial
  FROM `analisis-production.89547806.ga_sessions_*`, unnest(hits) as h,
  unnest(h.customDimensions) as hcd
  WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2018-05-01'))
  AND FORMAT_DATE('%Y%m%d', DATE('2018-05-15'))
  group by fullVisitorId,device.deviceCategory, channelGrouping, hcd.index, hcd.value"
#)

#df <-
#  bq_table_download(bq_project_query(project, get_data_query))

# k means clustering -------------------------------------------------------
km_init = KMeans_rcpp(df, clusters = 5, num_init = 5, max_iters = 100, 
                      
                      initializer = 'kmeans++', verbose = F)

getcent_init = km_init$centroids

getclust_init = km_init$clusters

# each observation is associated with the nearby centroid
# new_im_init =  getcent_init[getclust_init, ]
# colnames(new_im_init) <- colnames(df)
df$cluster <- getclust_init
df$fullVisitorId <- df_csv$fullVisitorId

# mini-batch k-means clustering
km_mb = MiniBatchKmeans(df, clusters = 5, batch_size = 20, num_init = 5, max_iters = 100, 
                        
                        init_fraction = 0.2, initializer = 'kmeans++', early_stop_iter = 10,
                        
                        verbose = F)

pr_mb = predict_MBatchKMeans(df, km_mb$centroids)

getcent_mb = km_mb$centroids

# each observation is associated with the nearby centroid
# new_im_mb = getcent_mb[pr_mb, ]
# colnames(new_im_mb) <- colnames(df)
df$cluster <- pr_mb
df$fullVisitorId <- df_csv$fullVisitorId
