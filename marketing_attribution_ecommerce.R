# load packages
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ChannelAttribution)
library(viridis)

# https://insidedatablog.wordpress.com/2017/02/02/marketing-attribution-with-markov-chains-in-r/


# use this query
#select cat_path, sum(transactions) as transactions, sum(Revenue) as Revenue from(
#  SELECT fullVisitorId, GROUP_CONCAT(channelGrouping, ' > ') as cat_path, sum(txns) as transactions, sum(Revenue)/1000000 as Revenue
#  FROM (
#    SELECT fullVisitorId, channelGrouping, visitStartTime, COUNT(DISTINCT hits.transaction.transactionId) AS txns,
#    sum(hits.product.productRevenue) as Revenue
#    FROM
#    TABLE_DATE_RANGE([airnz-ga-bigquery:125557395.ga_sessions_], TIMESTAMP('2018-01-01'), TIMESTAMP('2018-01-03'))
#    group by 1,2,3
#    ORDER BY fullVisitorId, visitStartTime
#  )
#  GROUP BY fullVisitorId)
# group by cat_path



# simulate some customer journeys
mydata = data.frame(userid = sample(c(1:1000), 5000, replace = TRUE),
                    date = sample(c(1:32), 5000, replace = TRUE),
                    channel = sample(c(0:9), 5000, replace = TRUE,
                                     prob = c(0.1, 0.15, 0.05, 0.07, 0.11, 0.07, 0.13, 0.1, 0.06, 0.16)))
mydata$date = as.Date(mydata$date, origin = "2017-01-01")
mydata$channel = paste0('channel_', mydata$channel)

mydata <- read_csv("results.csv")

# create sequence per user
seq = mydata %>%
  group_by(userid) %>%
  summarise(path = as.character(list(Channel)))
  

# group identical paths and add up conversions
seq = seq %>%
  group_by(path) %>%
  summarise(total_conversions = n())

# clean paths
seq$path = gsub("c\\(|)|\"|([\n])","", seq$path)
seq$path = gsub(",","\\1 \\2>", seq$path)

# run models
basic_model = heuristic_models(seq, "path", "total_conversions")
dynamic_model = markov_model(seq, "path", "total_conversions")

# build barplot
result = merge(basic_model,dynamic_model, by = "channel_name")
names(result) = c("channel","first","last","linear","markov")

result = melt(result, id.vars="channel")

ggplot(result, aes(channel, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_viridis(discrete=TRUE) +
  xlab("") + ylab("Conversions") +
  guides(fill = guide_legend(title = "Model"))

# build another barplot to see deviations
result = merge(basic_model,dynamic_model, by = "channel_name")
names(result) = c("channel","first","last","linear","markov")

result$first = ((result$first - result$markov)/result$markov)
result$last = ((result$last - result$markov)/result$markov)
result$linear = ((result$linear- result$markov)/result$markov)

result = melt(result[1:4], id.vars="channel")

ggplot(result, aes(channel, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_viridis(discrete=TRUE) +
  xlab("") + ylab("Deviation from markov") +
  guides(fill = guide_legend(title = "Model"))