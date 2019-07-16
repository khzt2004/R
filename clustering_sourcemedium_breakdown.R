library(tidyverse)
options(scipen = 999)

df <- read_csv("datastudio_explorer.csv")

top5_by_cluster <- df %>% 
  group_by(centroid_id, channelGrouping, medium) %>% 
  summarise(count = sum(user_count)) %>% 
  group_by(centroid_id) %>% 
  mutate(pct_cluster = count/sum(count)*100) %>% 
  top_n(5) %>% 
  arrange(centroid_id, desc(pct_cluster)) %>% 
  select(-count)

all_cluster_results <- df %>% 
  group_by(centroid_id, channelGrouping, medium) %>% 
  summarise(count = sum(user_count)) %>% 
  group_by(centroid_id) %>% 
  mutate(pct_cluster = count/sum(count)*100) %>% 
  select(-count)
  
write_csv(top5_by_cluster, "clustering_breakdown_top5.csv")
write_csv(all_cluster_results, "clustering_breakdown_allresults.csv")
