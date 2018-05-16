library(tidyverse)
library(ggalt)
library(scales)
library(ggthemes)


data <- read_csv("ancillary_chart.csv")
data <- data %>%
  mutate(B_AncillaryCategory = case_when(grepl("taxi|taxivan|shuttle", B_AncillaryCategory, ignore.case = TRUE) ~"taxi",
                                         TRUE ~ (as.character(B_AncillaryCategory))),
         A_tier_points_level = case_when(A_tier_points_level == 'ae' ~ "elite",
                                         A_tier_points_level == 'ag' ~ "gold",
                                         A_tier_points_level == 'as' ~ "silver",
                                         A_tier_points_level == 'aj' ~ "base"))
# data_raw <- read_csv("https://raw.githubusercontent.com/khzt2004/R/master/ancillary_chart.csv")

data1 <- data %>%
  select(Market = A_market,
         'Tier Points Level' = 'A_tier_points_level', 3:6) %>%
  mutate(bookingLeadMonths = round(bookingLeadMonths,2)) %>%
  filter(B_AncillaryCategory == 'taxi') %>%
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

data2 <- data %>%
  select(Market = A_market,
         'Tier Points Level' = 'A_tier_points_level', 3:6) %>%
  mutate(bookingLeadMonths = round(bookingLeadMonths,2)) %>%
  filter(B_AncillaryCategory == 'taxi') %>%
  group_by(`Tier Points Level`) %>%
  summarise(`Booking Lead Months` = round(mean(bookingLeadMonths),2)) %>%
  filter(!is.na(`Tier Points Level`))

# breakdown by tier
ggplot(data2, aes(y=`Tier Points Level`, x=`Booking Lead Months`)) + 
  geom_lollipop(point.colour="steelblue", point.size=2, horizontal=TRUE) +
  theme_minimal() +
  geom_label(aes(label=`Booking Lead Months`), nudge_x = 0.25, size=3) +
  theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) +
  theme(axis.text.y=element_text(margin=margin(r=0, l=0))) +
  theme(plot.margin=unit(rep(30, 4), "pt")) +
  theme(plot.title=element_text(face="bold"))



# histogram for exploration of distributions

data_hist <- data %>%
#  filter(grepl("insurance|car|hotel|taxi",B_AncillaryCategory) & !is.na(A_market))
  filter(grepl("car",B_AncillaryCategory) & !is.na(A_market)  )
  
ggplot(data_hist, aes(x=bookingLeadMonths, fill=A_tier_points_level)) +
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 10) +
  facet_grid(.~ A_tier_points_level) + 
  labs(fill = "Tier Points Level\n")
