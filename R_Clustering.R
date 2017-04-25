# http://stackoverflow.com/questions/30260317/assign-class-to-data-frame-after-clustering
# https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html
# http://www.statmethods.net/advstats/cluster.html

library(data.table)
library(readxl)
library(XLConnect)
library(cluster)
library(factoextra)

data <- read_excel("India_Keywords.xlsx")

set.seed(123456789) #Set the seed for reproducibility
k <-kmeans(data[,-c(1, 2, 4)], centers=4) #Create 5 clusters, remove column 2
k$centers #Display&nbsp;cluster centers
table(k$cluster) #Give a count of data points in each cluster

data$cluster <- k$cluster

# PCA
res.pca <- prcomp(data[,-c(1, 2, 4)])
get_eig(res.pca)
# Default plot
fviz_eig(res.pca)


# scree plot

rng<-2:20 #K from 2 to 20
tries <-100 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(data[,-c(1, 2, 4)],centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

# plot clusters

clusplot(data[,-c(1, 2, 4)], k$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# export to excel
fileDate <- format(Sys.time(), "%m%d%Y")
fileName <- paste("India_Keywords_", fileDate, ".xlsx", sep="")

#creating an Excel workbook. Both .xls and .xlsx file formats can be used.
wb <- loadWorkbook(fileName, create = TRUE)

#creating sheets within an Excel workbook
createSheet(wb, name = "keywords")

#writing into sheets within an Excel workbook : 
#writing data frame into filteredVARs

writeWorksheet(wb, data, sheet = "keywords", startRow = 1, startCol = 1)

#saving a workbook to an Excel file :
#saves a workbook to the corresponding Excel file and writes the file to disk.
saveWorkbook(wb)
