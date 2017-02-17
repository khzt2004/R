# http://code.markedmondson.me/googleAnalyticsR/v4.html
# http://kbroman.org/pkg_primer/pages/github.html
# install_github("MarkEdmondson1234/googleAnalyticsR")

library(googleAnalyticsR)
ga_auth()
## get your accounts
account_list <- google_analytics_account_list()
id <- account_list[100,'viewId']
unsampled_data_fetch <- google_analytics_4(id, date_range = c("2017-01-01", "2017-02-16"), 
                                           metrics = c("sessions", "pageViews", "entrances", "bounceRate", "avgTimeOnPage", "exitRate"), 
                                           dimensions = c("pagePath", "dimension2", "date"),
                                           filtersExpression = "ga:dimension2==https://launch.solidworks.co.jp/",
                                           anti_sample = TRUE)
write.csv(unsampled_data_fetch, "My Data Export.csv")