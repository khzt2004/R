library(googleAnalyticsR)
library(tidyverse)
library(corrplot)
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

web_data_metrics <- web_data[,c("sessions","pageviews","entrances","bounces")]
cor(web_data_metrics)
pairs(web_data_metrics)

## Get only desktop rows, and the date, channelGrouping and sessions columns
pivoted <- web_data %>% 
  filter(deviceCategory == "desktop") %>% 
  select(date, channelGrouping, sessions) %>%
  spread(channelGrouping, sessions)

## Get rid of any NA's and replace with 0
pivoted[is.na(pivoted)] <- 0

## can't include the date as its not numeric, so remove first column. -1 subset
cor_data <- pivoted[, -1]
## round the correlation values to 2 dp
cor_table <- round(cor(cor_data),2)


cor_table %>%
  corrplot(mar = c(2, 0, 1, 0), method = "color", order = "hclust", tl.srt=45, tl.cex=0.75)

# correlation plots to find interesting relationships
# paid vs organic search
gg <- ggplot(data = pivoted) + 
  theme_minimal() + 
  ggtitle("Paid (blue) vs Organic (green) search")
gg <- gg + 
  geom_line(aes(x = as.Date(date), y = `Paid Search`), col = "blue")

gg + geom_line(aes(x = as.Date(date), y = `Organic Search`), col = "green")

# social vs email 

gg <- ggplot(data = pivoted) + 
  theme_minimal() + 
  ggtitle("Social (red) vs Email (orange)")
gg <- gg + 
  geom_line(aes(x = as.Date(date), y = Social), col = "red")
gg + geom_line(aes(x = as.Date(date), y = Email), col = "orange")

# cross correlation - social vs email
ccf(pivoted$Social, pivoted$Email)