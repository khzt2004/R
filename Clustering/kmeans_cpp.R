library(ClusterR)
library(tidyverse)
library(summarytools)



df_csv <- read_csv("clustering_testexport.csv")
df <- df_csv %>% 
  select(-fullVisitorId) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate_all(funs(as.numeric))
df <- as.data.frame(df)

# view(dfSummary(df),  file = "clustering_df.html")

# k means clustering
km_init = KMeans_rcpp(df, clusters = 5, num_init = 5, max_iters = 100, 
                      
                      initializer = 'kmeans++', verbose = F)

getcent_init = km_init$centroids

getclust_init = km_init$clusters

# each observation is associated with the nearby centroid
new_im_init = getcent_init[getclust_init, ]  


# mini-batch k-means clustering
km_mb = MiniBatchKmeans(df, clusters = 5, batch_size = 20, num_init = 5, max_iters = 100, 
                        
                        init_fraction = 0.2, initializer = 'kmeans++', early_stop_iter = 10,
                        
                        verbose = F)

pr_mb = predict_MBatchKMeans(df, km_mb$centroids)

getcent_mb = km_mb$centroids

# each observation is associated with the nearby centroid
new_im_mb = getcent_mb[pr_mb, ]   

