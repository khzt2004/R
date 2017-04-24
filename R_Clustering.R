# http://stackoverflow.com/questions/30260317/assign-class-to-data-frame-after-clustering
# https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html

library(data.table)
library(readxl)

data <- read_excel("India_Keywords.xlsx")


set.seed(76964057) #Set the seed for reproducibility
k <-kmeans(data[,-c(1, 2, 4)], centers=5) #Create 5 clusters, remove column 2
k$centers #Display&nbsp;cluster centers
table(k$cluster) #Give a count of data points in each cluster

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
