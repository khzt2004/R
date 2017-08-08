library(RSiteCatalyst)
library(tidyverse)

key <- "<username>:<company>"
secret <- "<secret from - https://sc.omniture.com/p/suite/1.3/index.html?a=User.GetAccountInfo>"
SCAuth(key, secret)
suites <- GetReportSuites()

my.rsid <- suites[suites$site_title=="XXXXX",1]
elements.available <- GetElements(my.rsid)
metrics.available <- GetMetrics(my.rsid, elements = "<<myFavouriteElement>>")

dailyStats <- QueueOvertime(my.rsid,
                            date.from = "2017-01-01",
                            date.to = "2017-06-30",
                            metrics = c("pageviews","visits"),
                            date.granularity = "day"
)


ggplot(dailyStats, aes(x=dailyStats$datetime)) + 
  geom_line(aes(y=dailyStats$pageviews)) + 
  labs(title="Time Series Chart", 
       subtitle="Daily visits", 
       caption="Source: Adobe Analytics", 
       y="Page Views")
