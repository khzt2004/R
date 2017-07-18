library(googleAnalyticsR)
library(tidyverse)
library(corrplot)
library(RTextTools)
library(tm)
library(xts)
library(CausalImpact)
# http://randyzwitch.com/rsitecatalyst-k-means-clustering/

#specify the working directory if unable to authenticate
setwd("D:/ToBeSaved/K6O Documents")
ga_auth(new_user = TRUE)

# Pull a full list of the views that you have access to
my_accounts <- ga_account_list()

# Change this to your own view ID
my_id <- 122507877 

# Now, query for some basic data, assigning the data to a 'data frame' object 
# called 'web_data'
#web_data <- google_analytics_4(my_id, 
#                              date_range = c("2017-01-01", "2017-07-01"),
#                               metrics = c("sessions","pageviews",
#                                           "entrances","bounces"),
#                               dimensions = c("date","deviceCategory",
#                                              "channelGrouping"),
#                               anti_sample = TRUE)


web_data_test <- read.csv("https://github.com/khzt2004/R/raw/master/searchQueries.csv")


dtm <- create_matrix(web_data_test$Search.Query,
                     stemWords=TRUE,
                     removeStopwords=FALSE,
                     minWordLength=1,
                     removePunctuation= TRUE)

#Inspect most popular words, minimum frequency of 20
findFreqTerms(dtm, lowfreq=20)

#Guess for number of main topics: 
kmeans5<- kmeans(dtm, 5)

#Merge cluster assignment back to keywords
kw_with_cluster <- as.data.frame(cbind(web_data_test$Search.Query, kmeans5$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans5")

#Make df for each cluster result, quickly "eyeball" results
cluster1 <- subset(kw_with_cluster, subset=kmeans5 == 1)
cluster2 <- subset(kw_with_cluster, subset=kmeans5 == 2)
cluster3 <- subset(kw_with_cluster, subset=kmeans5 == 3)
cluster4 <- subset(kw_with_cluster, subset=kmeans5 == 4)
cluster5 <- subset(kw_with_cluster, subset=kmeans5 == 5)

# Selecting ‘k’ Using Elbow Method

#accumulator for cost results
cost_df <- data.frame()

#run kmeans for all clusters up to 100 - need to optimize speed
for(i in 1:100){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=dtm, centers=i, iter.max=100)
  
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
  
}

names(cost_df) <- c("cluster", "cost")


#Calculate lm's for emphasis
lm(cost_df$cost[1:10] ~ cost_df$cluster[1:10])
lm(cost_df$cost[10:19] ~ cost_df$cluster[10:19])
lm(cost_df$cost[20:100] ~ cost_df$cluster[20:100])

cost_df$fitted <- ifelse(cost_df$cluster <10, (19019.9 - 550.9*cost_df$cluster), 
                         ifelse(cost_df$cluster <20, (15251.5 - 116.5*cost_df$cluster),
                                (13246.1 - 35.9*cost_df$cluster)))

#Cost plot
ggplot(data=cost_df, aes(x=cluster, y=cost, group=1)) + 
  theme_bw(base_family="Garamond") + 
  geom_line(colour = "darkgreen") +
  theme(text = element_text(size=20)) +
  ggtitle("Reduction In Cost For Values of 'k'\n") +
  xlab("\nClusters") + 
  ylab("Within-Cluster Sum of Squares\n") +
  scale_x_continuous(breaks=seq(from=0, to=100, by= 10)) +
  geom_line(aes(y= fitted), linetype=2)
