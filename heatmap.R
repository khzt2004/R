library(RColorBrewer)
library(tidyverse)

# Google heatmap
google_heatmap <- read.csv("google_heatmap.csv", sep=",")
# google_heatmap1 <- spread(google_heatmap, Day.of.Week.Name, Sessions, fill = 0)
google_heatmap$Day.of.Week.Name <- factor(google_heatmap$Day.of.Week.Name, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

google_heatmap %>%
  ggplot(aes(x=Day.of.Week.Name, y=Hour, fill=Sessions)) + 
  geom_tile() + 
  scale_fill_continuous(low = 'white', high = 'red') +
  ggtitle('Adwords - Sessions by Hour and Day of Week')


# FB heatmap
fb_heatmap <- read.csv("fb_heatmap.csv", sep=",")
# fb_heatmap1 <- spread(google_heatmap, Day.of.Week.Name, Sessions, fill = 0)
fb_heatmap$Day.of.Week.Name <- factor(fb_heatmap$Day.of.Week.Name, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


fb_heatmap %>%
  ggplot(aes(x=Day.of.Week.Name, y=Hour, fill=Sessions)) + 
  geom_tile() + 
  scale_fill_continuous(low = 'white', high = 'red') +
  ggtitle('Facebook - Sessions by Hour and Day of Week')