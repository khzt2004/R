library(tidyverse)
library(ggalt)
library(scales)
library(ggthemes)


data <- read_csv("ancillary_chart.csv")
# data_raw <- read_csv("https://raw.githubusercontent.com/khzt2004/R/master/ancillary_chart.csv")

data1 <- data %>%
  select(Market = A_market,
         'Tier Points Level' = 'A_tier_points_level', 3:6) %>%
  mutate(bookingLeadMonths = round(bookingLeadMonths,2)) %>%
  filter(B_AncillaryCategory == 'car') %>%
  group_by(Market) %>%
  summarise(`Booking Lead Months` = round(mean(bookingLeadMonths),2))

# breakdown by market
ggplot(data1, aes(y=Market, x=`Booking Lead Months`)) + 
  geom_lollipop(point.colour="steelblue", point.size=2, horizontal=TRUE) +
  theme_minimal() +
  geom_label(aes(label=`Booking Lead Months`), nudge_x = 0.25, size=3) +
  theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) +
  theme(axis.text.y=element_text(margin=margin(r=0, l=0))) +
  theme(plot.margin=unit(rep(30, 4), "pt")) +
  theme(plot.title=element_text(face="bold"))


# breakdown by tier
ggplot(data1, aes(y=Market, x=`Booking Lead Months`)) + 
  geom_lollipop(point.colour="steelblue", point.size=2, horizontal=TRUE) +
  theme_minimal() +
  geom_label(aes(label=`Booking Lead Months`), nudge_x = 0.25, size=3) +
  theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) +
  theme(axis.text.y=element_text(margin=margin(r=0, l=0))) +
  theme(plot.margin=unit(rep(30, 4), "pt")) +
  theme(plot.title=element_text(face="bold"))
