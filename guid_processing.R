library(tidyverse)
library(rpivotTable)
library(lubridate)
library(reshape2)
library(VennDiagram)

FINAL_DATASET_channel_pagepath <- FINAL_DATASET_channel_pagepath %>%
  select(-File)

save(FINAL_DATASET_channel_pagepath, file = "FINAL_DATASET_channel_pagepath.RData")
write_csv(FINAL_DATASET_channel_pagepath, "FINAL_DATASET_channel_pagepath.csv")

filtered_FINAL_DATASET_channel_pagepath <- FINAL_DATASET_channel_pagepath %>%
  filter(!grepl("selectitinerary", pagepath, ignore.case = TRUE)) %>%
  select(-File)

write_csv(filtered_FINAL_DATASET_channel_pagepath, "filtered_FINAL_DATASET_channel_pagepath.csv")


sourcefile <- load("FINAL_DATASET_channel_pagepath.RData")
rpivotTable(FINAL_DATASET_channel_pagepath)

# add binning 
FINAL_DATASET_channel_pagepath <-  FINAL_DATASET_channel_pagepath %>%
  mutate(days_to_booking_bin = case_when(days_to_booking < -7 ~ "> 1 Week After Booking",
                                         days_to_booking >= -7 & days_to_booking <= -1 ~ "1 Week After Booking",
                                         days_to_booking == 0 ~ "Day of Booking",
                                         days_to_booking >= 1 & days_to_booking <= 7 ~ "1 Week Before Booking",
                                         days_to_booking > 7 & days_to_booking <= 30 ~ "1 Week to 1 Month Before Booking",
                                         days_to_booking > 30 & days_to_booking <= 90 ~ "1 Month to 3 Months Before Booking",
                                         days_to_booking > 90 ~ "> 3 Months Before Booking")) %>%
  mutate(days_to_departure_bin = case_when(days_to_departure >= -7 & days_to_departure <= -1 ~ "1 Week After Departure",
                                           days_to_departure == 0 ~ "Day of Travel",
                                           days_to_departure >= 1 & days_to_departure <= 7 ~ "1 Week Before Departure",
                                           days_to_departure > 7 & days_to_departure <= 30 ~ "1 Week to 1 Month Before Departure",
                                           days_to_departure > 30 & days_to_departure <= 90 ~ "1 Month to 3 Months Before Departure",
                                           days_to_departure > 90 ~ "> 3 Months Before Departure")) %>%
  mutate(FB_FS_Ratio = flightbooking/flightsearch)

# binning for Dream table - page categorisation
pagesection <- read.csv("page_lookup.csv")
dream2 <- read.csv("dream2_newpagepath.csv")
# rpivotTable(dream2)

dream2_binpage <- dream2 %>%
  left_join(pagesection, by = c("newpagepath")) %>%
  mutate(page_category = case_when(grepl("/vbook", newpagepath, ignore.case = TRUE) ~"vbook",
                                   grepl("/vloyalty", newpagepath, ignore.case = TRUE) ~"vloyalty",
                                   grepl("/vauth", newpagepath, ignore.case = TRUE) ~"vauth",
                                   grepl("/vmanage", newpagepath, ignore.case = TRUE) ~"vmanage",
                                   grepl("/vpref", newpagepath, ignore.case = TRUE) ~"vpref",
                                   grepl("/content/", newpagepath, ignore.case = TRUE) ~"travel insurance",
                                   grepl("/activity-deals/", newpagepath, ignore.case = TRUE) ~"activity deals",
                                   grepl("Hotel-Information", newpagepath, ignore.case = TRUE) ~"hotel info",
                                   grepl("/entertainment/", newpagepath, ignore.case = TRUE) ~"entertainment",
                                   grepl("/bookings/complete/", newpagepath, ignore.case = TRUE) ~"bookings complete",
                                   grepl("/trips/", newpagepath, ignore.case = TRUE) ~"trips",
                                   grepl("/error/404/", newpagepath, ignore.case = TRUE) ~"error 404",
                                   grepl("/press-release", newpagepath, ignore.case = TRUE) ~"press release",
                                   TRUE ~ as.character(newpagepath))) %>%
  mutate(device_overlap_bin = case_when(grepl("desktop", device_overlap, ignore.case = TRUE) & grepl("mobile|tablet", device_overlap, ignore.case = TRUE) ~"desktop + others",
                                        device_overlap == "desktop" ~ "desktop",
                                        device_overlap == "mobileApp" ~ "App",
                                        device_overlap == "tabletWeb" | device_overlap == "mobileWeb" ~ "mobile",
                                        TRUE ~ "Others")) %>%
  mutate(pagesection_combine = coalesce(as.character(pagesection), as.character(page_category))) %>%
  select(1:3, 11, 4,10, 5:7)

write_csv(dream2_binpage, "dream2_binpage.csv")

dream3 <- read.csv("dream3_newpagepath.csv")

dream3_binpage <- dream3 %>%
  left_join(pagesection, by = c("newpagepath")) %>%
  mutate(page_category = case_when(grepl("/vbook", newpagepath, ignore.case = TRUE) ~"vbook",
                                   grepl("/vloyalty", newpagepath, ignore.case = TRUE) ~"vloyalty",
                                   grepl("/vauth", newpagepath, ignore.case = TRUE) ~"vauth",
                                   grepl("/vmanage", newpagepath, ignore.case = TRUE) ~"vmanage",
                                   grepl("/vpref", newpagepath, ignore.case = TRUE) ~"vpref",
                                   grepl("/content/", newpagepath, ignore.case = TRUE) ~"travel insurance",
                                   grepl("/activity-deals/", newpagepath, ignore.case = TRUE) ~"activity deals",
                                   grepl("Hotel-Information", newpagepath, ignore.case = TRUE) ~"hotel info",
                                   grepl("/entertainment", newpagepath, ignore.case = TRUE) ~"entertainment",
                                   grepl("/bookings/complete/", newpagepath, ignore.case = TRUE) ~"bookings complete",
                                   grepl("/trips/", newpagepath, ignore.case = TRUE) ~"trips",
                                   grepl("/error/404/", newpagepath, ignore.case = TRUE) ~"error 404",
                                   grepl("/press-release", newpagepath, ignore.case = TRUE) ~"press release",
                                   TRUE ~ as.character(newpagepath))) %>% 
  mutate(pagesection_combine = coalesce(as.character(pagesection), as.character(page_category))) %>%
  select(1:3, 11, 4:8)

write_csv(dream3_binpage, "dream3_binpage.csv")


final_dataset_fly2 <- read.csv("final_dataset_fly2.csv")

final_dataset_fly2_binned <- final_dataset_fly2 %>%
  mutate(booking_lead_days = as.integer(as.character(booking_lead_days))) %>%
  mutate(booking_lead_days_bin = case_when(booking_lead_days < 0 & booking_lead_days >= -30 ~ "-1 month",
                                           booking_lead_days < -30 & booking_lead_days > -60 ~ "-2 months",
                                           booking_lead_days < -60 ~ "> -3 months",
                                           booking_lead_days == 0 ~ '0',
                                           booking_lead_days > 0 &  booking_lead_days <= 30 ~ "1 month",
                                           booking_lead_days > 30 &  booking_lead_days <= 60 ~ "2 months",
                                           booking_lead_days > 60 &  booking_lead_days <= 90 ~ "3 months",
                                           booking_lead_days > 90 &  booking_lead_days <= 120 ~ "4 months",
                                           booking_lead_days > 120 &  booking_lead_days <= 180 ~ "5-6 months",
                                           booking_lead_days > 180 ~ "> 6 months",
                                           TRUE ~ as.character(booking_lead_days))) %>%
  mutate(device_overlap_bin = case_when(grepl("desktop", device_overlap, ignore.case = TRUE) & grepl("mobile|tablet", device_overlap, ignore.case = TRUE) ~"desktop + others",
                                        device_overlap == "desktop" ~ "desktop",
                                        device_overlap == "mobileApp" ~ "App",
                                        device_overlap == "tabletWeb" | device_overlap == "mobileWeb" ~ "mobile",
                                        TRUE ~ "Others")) %>%
  select(1:4, 8, 9, 5:7)

write_csv(final_dataset_fly2_binned, "final_dataset_fly2_binned.csv")

