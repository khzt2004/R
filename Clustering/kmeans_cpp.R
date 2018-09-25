library(ClusterR)
library(tidyverse)
library(summarytools)
library(bigrquery)
library(fst)

# import a sample dataset for use with clustering ----------------------------------
# download csv file (clustering_export.csv) from github into your working directory
# https://github.com/sparklineanalytics/analysts/blob/master/analysis/clustering%20analysis/clustering_export.csv
# replace this with actual data from BigQuery
df_csv <- read_csv("clustering_export.csv")
df <- df_csv %>% 
  select(-fullVisitorId) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate_all(funs(as.numeric))
df <- as.data.frame(df)


# take a peek at their data for quick summary stats --------------------------------
view(dfSummary(df),  file = "clustering_df.html")

# get data directly from bigquery --------------------------------------------------
# 10 million rows, 15 cols of data is approx 728 mb. 
# Using fst format it compresses to 214 mb

get_data_query <- paste0(
  "# start of outermost query -------------------------------------
  select
  fullVisitorId,
  userid_cd,
  clientid_cd,
  sum(mobile_device) as mobile_device,
  sum(desktop_device) as desktop_device,
  sum(tablet_device) as tablet_device,
  sum(social_channelGrouping) as social_channelGrouping,
  sum(Referral_channelGrouping) as Referral_channelGrouping,
  sum(Direct_channelGrouping) as Direct_channelGrouping,
  sum(Organic_channelGrouping) as Organic_channelGrouping,
  sum(Paid_channelGrouping) as Paid_channelGrouping,
  sum(Display_channelGrouping) as Display_channelGrouping,
  sum(paidsocial_channelGrouping) as paidsocial_channelGrouping,
  sum(editorial_editorialtype) as editorial_editorialtype,
  sum(advertorial_editorialtype) as advertorial_editorialtype,
  sum(bola_subcategory) as bola_subcategory,
  sum(transactions) as transactions,
  sum(transactionRevenue) as transactionRevenue
  from (
  # ------------------------------------------------------------------
  # need to sum another level up to get one single row per fullvisitorid, only group by fullvisitorid ------------------------------------
  
  # unpivot the query -----------------------------------------------
  select 
  fullVisitorId,
  userid_cd,
  clientid_cd,
  case when device = 'mobile' then sum(sessions) else 0 end as mobile_device,
  case when device = 'desktop' then sum(sessions) else 0 end as desktop_device,
  case when device = 'tablet' then sum(sessions) else 0 end as tablet_device,
  case when channelGrouping = 'Social' then sum(sessions) else 0 end as social_channelGrouping,
  case when channelGrouping = 'Referral' then sum(sessions) else 0 end as Referral_channelGrouping,
  case when channelGrouping = 'Direct' then sum(sessions) else 0 end as Direct_channelGrouping,
  case when channelGrouping = 'Organic Search' then sum(sessions) else 0 end as Organic_channelGrouping,
  case when channelGrouping = 'Paid Search' then sum(sessions) else 0 end as Paid_channelGrouping,
  case when channelGrouping = 'Display' then sum(sessions) else 0 end as Display_channelGrouping,
  case when channelGrouping = 'Paid Social' then sum(sessions) else 0 end as paidsocial_channelGrouping,
  case when editorialtype = 'editorial' then sum(sessions) else 0 end as editorial_editorialtype,
  case when editorialtype = 'advertorial' then sum(sessions) else 0 end as advertorial_editorialtype,
  case when regexp_contains('^bola$', subcategory) then sum(sessions) else 0 end as bola_subcategory,
  transactions,
  transactionRevenue
  
  from(
  # -----------------------------------------------
  SELECT
  fullVisitorId,
  userid_cd,
  clientid_cd,
  device,
  channelGrouping,
  editorialtype,
  subcategory,
  transactions,
  transactionRevenue,
  COUNT(DISTINCT SessionId) AS sessions
  FROM (
  SELECT
  fullVisitorId,
  # unnest a user-scoped or session-scoped custom dimension ---------------------------------------------
  (SELECT  value FROM t.customDimensions where index=1) as userid_cd,
  (SELECT  value FROM t.customDimensions where index=20) as clientid_cd,
  -------------------------------------------------------------
  channelGrouping,
  device.deviceCategory as device,
  # unnest a hit-scoped custom dimension -------------------------
  (
  SELECT
  h.value
  FROM
  UNNEST(hit.customDimensions) h
  WHERE
  h.index = 6) AS editorialtype,
  (
  SELECT
  h.value
  FROM
  UNNEST(hit.customDimensions) h
  WHERE
  h.index = 18) AS subcategory,
  hit.page.pagepath as pagepath,
  # ------------------------------------------------------------
  totals.transactions as transactions,
  totals.transactionRevenue as transactionRevenue,
  CONCAT(CAST(fullVisitorId AS STRING), CAST(visitId AS STRING)) AS SessionId
  FROM
  `analisis-production.89547806.ga_sessions_*` t,
  UNNEST(hits) AS hit
  WHERE
  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2018-07-01'))
  AND FORMAT_DATE('%Y%m%d', DATE('2018-07-20'))
  # AND regexp_contains(clientId, '798226591.1485154909|1226251030.1518317157')
  )
  # filter for null values in custom dimension -----------------------
  where editorialtype is not null
  
  # ------------------------------------------------------------------
  GROUP BY
  fullVisitorId,
  clientid_cd,
  userid_cd,
  device,
  channelGrouping,
  editorialtype,
  subcategory,
  transactions,
  transactionRevenue)
  group by 
  fullVisitorId,
  userid_cd,
  clientid_cd,
  device,
  channelGrouping,
  editorialtype,
  subcategory,
  transactions,
  transactionRevenue
  # end outermost query ---------------------------------------------------------
  )
  group by 
  fullVisitorId,
  userid_cd,
  clientid_cd"
)

df <- bq_table_download(bq_project_query(project, get_data_query))

# optional: export data to fst format on local machine ---------------------------
# fst format provides quick and compressed storage -------------------------------
# write.fst(df, "clustering_query.fst")
# df <- read.fst("clustering_query.fst")

df <- df %>% 
  select(-fullVisitorId) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate_all(funs(as.numeric))
df <- as.data.frame(df)


# try different algorithms to suggest the optimal number of clusters to use -------------
opt_dist_fk = Optimal_Clusters_KMeans(df, max_clusters = ncol(df), plot_clusters = T,
                              
                              criterion = 'distortion_fK', fK_threshold = 0.85,
                              
                              initializer = 'optimal_init', tol_optimal_init = 0.2)


opt_var_expl = Optimal_Clusters_KMeans(df, max_clusters = ncol(df), 
                                       plot_clusters = T,
                                       criterion = 'variance_explained',
                                       initializer = 'optimal_init', 
                                       tol_optimal_init = 0.2)


opt_WCSSE = Optimal_Clusters_KMeans(df, max_clusters = ncol(df), 
                                       plot_clusters = T,
                                       criterion = 'WCSSE',
                                       initializer = 'optimal_init', 
                                       tol_optimal_init = 0.2)

opt_silhouette = Optimal_Clusters_KMeans(df, max_clusters = ncol(df), 
                                    plot_clusters = T,
                                    criterion = 'silhouette',
                                    initializer = 'optimal_init', 
                                    tol_optimal_init = 0.2)

opt_BIC = Optimal_Clusters_KMeans(df, max_clusters = ncol(df), 
                                         plot_clusters = T,
                                         criterion = 'BIC',
                                         initializer = 'optimal_init', 
                                         tol_optimal_init = 0.2)

opt_AIC = Optimal_Clusters_KMeans(df, max_clusters = ncol(df), 
                                  plot_clusters = T,
                                  criterion = 'AIC',
                                  initializer = 'optimal_init', 
                                  tol_optimal_init = 0.2)


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

# mini-batch k-means clustering------------------------------------------------
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

# export the dataframe to csv ------------------------------------------------
write_csv(df, "clustering_results.csv")
