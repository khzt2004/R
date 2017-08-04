library(RColorBrewer)
library(tidyverse)
library(viridis)
library(ggthemes)

# Google heatmap
google_heatmap <- read.csv("google_heatmap.csv", sep=",")
# google_heatmap1 <- spread(google_heatmap, Day.of.Week.Name, Sessions, fill = 0)
google_heatmap$Day.of.Week.Name <- factor(google_heatmap$Day.of.Week.Name, c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))

google_heatmap %>%
  ggplot(aes(x=Hour, y=Day.of.Week.Name, fill=Sessions)) +
  geom_tile(color="white", size=0.1) +
  # scale_fill_continuous(low = "#7baaf7", high = "#3367d6") + 
  scale_fill_viridis(name="Sessions") +
  scale_x_continuous(breaks = c(0:23), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(y = "Day of Week", title="Adwords - Sessions by Hour and Day of Week") +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_text(size=9)) + 
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=6)) +
  annotate("rect", xmin=-0.5, xmax=8, ymin=-Inf, ymax=Inf, alpha=.2, fill="blue")


# FB heatmap
fb_heatmap <- read.csv("fb_heatmap.csv", sep=",")
# fb_heatmap1 <- spread(google_heatmap, Day.of.Week.Name, Sessions, fill = 0)
fb_heatmap$Day.of.Week.Name <- factor(fb_heatmap$Day.of.Week.Name, c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))

fb_heatmap %>%
  ggplot(aes(x=Hour, y=Day.of.Week.Name, fill=Sessions)) +
  geom_tile(color="white", size=0.1) +
  scale_fill_viridis(name="Sessions") +
  scale_x_continuous(breaks = c(0:23), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(y = "Day of Week", title="Facebook - Sessions by Hour and Day of Week") +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_text(size=9)) + 
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=6)) +
  annotate("rect", xmin=-0.5, xmax=8, ymin=-Inf, ymax=Inf, alpha=.2, fill="blue")

