library(googleAnalyticsR)
library(tidyverse)
# creates decision trees
library(rpart.plot) 
ga_auth()

# Pull a full list of the views that you have access to
my_accounts <- ga_account_list()

# Change this to your own view ID
my_id <- 122507877 

# Now, query for some basic data, assigning the data to a 'data frame' object 
# called 'web_data'
web_data <- google_analytics_4(my_id, 
                               date_range = c("2017-01-01", "2017-07-01"),
                               metrics = c("sessions","pageviews",
                                           "entrances","bounces"),
                               dimensions = c("date","deviceCategory",
                                              "channelGrouping"),
                               anti_sample = TRUE)

# use this to export data from GA to csv or read csv data

# write.csv(web_data, file = "APS2017_GA_Data.csv")
# web_data_test <- read.csv("APS2017_GA_Data.csv")
# web_data_test <- web_data_test %>%
#   select(2:8)

cats <- web_data %>% 
  select(deviceCategory, channelGrouping, sessions)

tree <- rpart(deviceCategory ~ ., cats)
plot(tree)
text(tree)
rpart.plot(tree, type=1)