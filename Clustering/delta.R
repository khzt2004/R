# Delta Airlines Fleet analysis
# Myles Harrison
# http://www.everydayanalytics.ca
# Data from Delta.com:
# http://www.delta.com/content/www/en_US/traveling-with-us/airports-and-aircraft/Aircraft.html

data <- read.csv(file="delta.csv", header=T, sep=",", row.names=1)

# scatterplot matrix of intermediary (size/non-categorical) variables
plot(data[,16:22])

# Naively apply principal components analysis to raw data and plot
pc <- princomp(data)
plot(pc)

# First component dominates greatly. What are the loadings?
summary(pc) # 1 component has > 99% variance
loadings(pc) # Can see all variance is in the range in miles 

# verify by plotting variance of columns
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.8)
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.8, log='x')
par(mar=mar)

# Scale
data2 <- data.frame(scale(data))
# Verify variance is uniform
plot(sapply(data2, var))

# Proceed with principal components
pc <- princomp(data2)
plot(pc)
plot(pc, type='l')
summary(pc) # 4 components is both 'elbow' and explains >85% variance

# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(data2)

# First for principal components
comp <- data.frame(pc$x[,1:4])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

library(rgl)
# Multi 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3)
plot3d(comp$PC1, comp$PC3, comp$PC4)

# K-means clustering
# Determine the correct number of clusters via weighted sum of squares
# (from R in Action: http://www.statmethods.net/advstats/cluster.html)
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(comp, centers=i, nstart=100, iter.max=1000)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# From scree plot elbow occurs at k = 4
# Apply k-means with k=4
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

# 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3d(comp$PC1, comp$PC3, comp$PC4, col=k$clust)

# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))

# First cluster
row.names(data[k$clust==clust[1],])
# Second Cluster
row.names(data[k$clust==clust[2],])
# Third Cluster
row.names(data[k$clust==clust[3],])
# Fourth Cluster
row.names(data[k$clust==clust[4],])

# Compare accommodation by cluster in boxplot
boxplot(data$Accommodation ~ k$cluster, 
        xlab='Cluster', ylab='Accommodation', 
        main='Plane Accommodation by Cluster')

# Compare presence of seat classes in largest clusters
data[k$clust==clust[3],30:33]
data[k$clust==clust[4],30:33]

