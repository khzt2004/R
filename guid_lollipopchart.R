library(tidyverse)
library(ggalt)
library(scales)
library(ggthemes)


data <- read_csv("ancillary_chart2.csv")
data <- data %>%
  mutate(booking_lead_days = abs(booking_lead_days),
         ancillaryCategory = case_when(grepl("taxi|taxivan|shuttle", productName, ignore.case = TRUE) ~"taxi",
                                       productName == 'car' ~ "car",
                                       productName == 'insurance' ~ "insurance",
                                         TRUE ~ (as.character(ancillaryCategory))),
         tier_points_level = case_when(tier_points_level == 'ae' ~ "elite",
                                         tier_points_level == 'ag' ~ "gold",
                                         tier_points_level == 'as' ~ "silver",
                                         tier_points_level == 'aj' ~ "base"))
# data_raw <- read_csv("https://raw.githubusercontent.com/khzt2004/R/master/ancillary_chart.csv")

data1 <- data %>%
  filter(ancillaryCategory == 'taxi') %>%
  group_by(market) %>%
  summarise(`Booking Lead Days` = round(mean(booking_lead_days),0))

# breakdown by market
ggplot(data1, aes(y=market, x=`Booking Lead Days`)) + 
  geom_lollipop(point.colour="steelblue", point.size=2, horizontal=TRUE) +
  theme_minimal() +
  geom_label(aes(label=`Booking Lead Days`), nudge_x = 0.4, size=3) +
  theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) +
  theme(axis.text.y=element_text(margin=margin(r=0, l=0))) +
  theme(plot.margin=unit(rep(30, 4), "pt")) +
  theme(plot.title=element_text(face="bold"))

data2 <- data %>%
  select(ancillaryCategory, market,
         'Tier Points Level' = 'tier_points_level', 3:6) %>%
  mutate(bookingLeaddays = round(booking_lead_days,0)) %>%
  filter(ancillaryCategory == 'taxi') %>%
  group_by(`Tier Points Level`) %>%
  summarise(`Booking Lead Days` = round(mean(bookingLeaddays),0)) %>%
  filter(!is.na(`Tier Points Level`))

# breakdown by tier
ggplot(data2, aes(y=`Tier Points Level`, x=`Booking Lead Days`)) + 
  geom_lollipop(point.colour="steelblue", point.size=2, horizontal=TRUE) +
  theme_minimal() +
  geom_label(aes(label=`Booking Lead Days`), nudge_x = 0.25, size=3) +
  theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) +
  theme(axis.text.y=element_text(margin=margin(r=0, l=0))) +
  theme(plot.margin=unit(rep(30, 4), "pt")) +
  theme(plot.title=element_text(face="bold"))



# histogram for exploration of distributions

data_hist <- data %>%
#  filter(grepl("insurance|car|hotel|taxi",B_AncillaryCategory) & !is.na(A_market))
  filter(grepl("taxi", ancillaryCategory) & !is.na(market))
  
ggplot(data_hist, aes(x=booking_lead_days, fill=market)) +
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 10) +
  facet_grid(.~ market) +
  # facet_wrap(~ market, nrow = 2) +
  labs(title = "Ancillary Product - Car", fill = "Tier Points Level\n")
