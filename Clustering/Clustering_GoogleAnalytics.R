library(factoextra)
library(tidyverse)

# Load data
data("USArrests")

# my_data <- USArrests

my_data_raw <- read_csv("train_data.csv")
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data_raw)
# Scale variables
my_data <- scale(my_data)
# View the firt 3 rows
head(my_data, n = 3)

# find optimal number of clusters
screeplot <- fviz_nbclust(my_data, kmeans, method = "wss")

# Compute k-means  
set.seed(123)
km.res <- kmeans(my_data, 7, nstart = 25)

# Print the results
print(km.res)

aggregate(my_data, by=list(cluster=km.res$cluster), mean)

dd <- cbind(my_data, cluster = km.res$cluster)
head(dd)

dd_raw <- cbind(my_data_raw, cluster = km.res$cluster)
head(dd_raw)

# Cluster number for each of the observations
km.res$cluster

# Cluster size
km.res$size

# Cluster means
cluster_means <- km.res$centers

# visualise clusters 
fviz_cluster(km.res, data = dd,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



