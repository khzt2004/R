library(tidyverse)
library(reshape2)
library(data.table)

mydata <- read_csv("results.csv")
mydata <- mydata %>%
  rename_at("Booking_Lead_Days",~"BookingLeadDays")

mydata1 <- melt(mydata, id.vars = c("Booking_value", 
                                    "Research_value",
                                    "Post_Booking_vManage_value",
                                    "Post_Booking_Other_value",
                                    "DoT_value",
                                    "Ancillary_value",
                                    "NULL_stage_value"),
            variable.name = "labels", 
            value.name = "metrics")

mydata1 <- mydata1 %>%
  separate(labels, c("labels", "sublabel"), "_")

mydata1 <- mydata1%>%
  mutate(sublabel = replace(sublabel,is.na(sublabel),"Values")) %>%
  spread(sublabel, metrics)

mydata1_market_membership <- mydata1 %>%
  filter(labels == "market" | labels == "membership") %>%
  select(-Values) %>%
  mutate(BookingLeadDays = "", flightBooking = "", flightSearch = "", productRevenue = "", 
         sessions = "", users = "")

mydata1_remainder <- mydata1 %>%
  filter(labels != "market" & labels != "membership") %>%
  spread(labels, Values) %>%
  mutate(labels = "") %>%
  select(1:6, labels, 7:22)

mydata1_consolidated <- rbind(mydata1_market_membership, mydata1_remainder)

mydata1_consolidated[,9:23] <- sapply(mydata1_consolidated[,9:23],as.numeric)
mydata1_consolidated[, 9:23][is.na(mydata1_consolidated[, 9:23])] <- 0


write_csv(mydata1_consolidated, "mydata1_consolidated.csv")


