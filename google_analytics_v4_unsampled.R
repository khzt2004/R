# http://code.markedmondson.me/googleAnalyticsR/v4.html
# http://kbroman.org/pkg_primer/pages/github.html

library(googleAnalyticsR)
ga_auth()
## get your accounts
account_list <- google_analytics_account_list()
id <- account_list[100,'viewId']
unsampled_data_fetch <- google_analytics_4(id, date_range = c("2017-01-01", "2017-01-21"), 
                                                             metrics = c("sessions","bounceRate"), 
                                                             dimensions = c("date","source", "medium"),
                                                             anti_sample = TRUE)
                                                             