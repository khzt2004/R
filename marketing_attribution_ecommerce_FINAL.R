# load packages
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ChannelAttribution)
library(viridis)

# https://insidedatablog.wordpress.com/2017/02/02/marketing-attribution-with-markov-chains-in-r/


# use this query, change the query date to suit your needs
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


mydata <- read_csv("results.csv")
mydata <- mydata %>%
  mutate(Revenue = if_else(is.na(Revenue), 0, Revenue))


# run models
basic_model = heuristic_models(mydata, "cat_path", "transactions", var_value='Revenue')
# dynamic_model = markov_model(seq, "path", "total_conversions")

# result for conversions
basic_model_conv <- basic_model %>%
  select(channel_name, first_touch_conversions, last_touch_conversions, linear_touch_conversions)
result = melt(basic_model_conv, id.vars="channel_name")

ggplot(result, aes(channel_name, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  ggtitle('Total Conversions') +
  scale_fill_viridis(discrete=TRUE) +
  xlab("") + ylab("Conversions") +
  guides(fill = guide_legend(title = "Model")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# result for revenue
basic_model_value <- basic_model %>%
  select(channel_name, first_touch_value, last_touch_value, linear_touch_value)
result_revenue = melt(basic_model_value, id.vars="channel_name")

ggplot(result_revenue, aes(channel_name, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  ggtitle('Total Value') +
  scale_fill_viridis(discrete=TRUE) +
  xlab("") + ylab("Conversions") +
  guides(fill = guide_legend(title = "Model")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# export master table as csv file
