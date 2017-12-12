library(tidyverse)

FINAL_DATASET_channel_pagepath <- FINAL_DATASET_channel_pagepath %>%
  select(-File)

save(FINAL_DATASET_channel_pagepath, file = "FINAL_DATASET_channel_pagepath.RData")
write_csv(FINAL_DATASET_channel_pagepath, "FINAL_DATASET_channel_pagepath.csv")

filtered_FINAL_DATASET_channel_pagepath <- FINAL_DATASET_channel_pagepath %>%
  filter(!grepl("selectitinerary", pagepath, ignore.case = TRUE)) %>%
  select(-File)

write_csv(filtered_FINAL_DATASET_channel_pagepath, "filtered_FINAL_DATASET_channel_pagepath.csv")


sourcefile <- load("FINAL_DATASET_channel_pagepath.RData")

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
                                           days_to_departure > 90 ~ "> 3 Months Before Departure")) 
  
