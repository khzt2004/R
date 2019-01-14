library(tidyverse)
library(zoo)

search_df <- read_csv("HVA_Search.csv", na = c("", "na", "NaN", "null"))
book_df <- read_csv("HVA_Book.csv", na = c("", "na", "NaN", "null"))


################## Search dataset ###############
search_df_overall <- search_df %>%
  filter(Duration == 'All Months') %>% 
  select(-`Flight Search or Booking`, -`Col Name`)

search_df_overall_tier  <- search_df %>%
  filter(Duration == 'All Months' & grepl('KF Tier', Filter)) %>% 
  select(-`Flight Search or Booking`, -`Col Name`) %>% 
  mutate(row = row_number()) %>% 
  spread(Filter, Correlation)

search_df_overall_cabinSession  <- search_df %>%
  filter(Duration == 'All Months' & grepl('Cabin Session', Filter)) %>% 
  select(-`Flight Search or Booking`, -`Col Name`) %>% 
  mutate(row = row_number()) %>% 
  spread(Filter, Correlation)

search_df_monthBreakdown <- search_df %>%
  filter(Duration != 'All Months') %>% 
  select(-`Flight Search or Booking`, -`Col Name`) %>% 
  mutate(Duration = gsub("-18", "-2018", Duration)) %>% 
  mutate(Duration = as.yearmon(Duration, "%b-%Y")) %>% 
  #mutate(row = row_number()) %>% 
  #spread(Duration, Correlation) %>% 
  filter(`Event/Custom Dimension` == 'Cabin - Session') %>% 
  mutate(Event_CustomDimension = `Event/Custom Dimension Value`)

ggplot(data=search_df_monthBreakdown, aes(x=Duration, y=Correlation, 
                                          color= Event_CustomDimension,
                                          group = Event_CustomDimension)) +
  geom_line() +
  facet_wrap(~Filter)



################## Book dataset ###############

book_df_overall <- book_df %>%
  filter(Duration == 'All Months') %>% 
  select(-`Flight Search or Booking`, -`Col Name`)

book_df_overall_tier  <- book_df %>%
  filter(Duration == 'All Months' & grepl('KF Tier', Filter)) %>% 
  select(-`Flight Search or Booking`, -`Col Name`) %>% 
  mutate(row = row_number()) %>% 
  spread(Filter, Correlation)

book_df_overall_cabinSession  <- book_df %>%
  filter(Duration == 'All Months' & grepl('Cabin Session', Filter)) %>% 
  select(-`Flight Search or Booking`, -`Col Name`) %>% 
  mutate(row = row_number()) %>% 
  spread(Filter, Correlation)

book_df_monthBreakdown <- book_df %>%
  filter(Duration != 'All Months') %>% 
  select(-`Flight Search or Booking`, -`Col Name`) %>% 
  mutate(Duration = gsub("-18", "-2018", Duration)) %>% 
  mutate(Duration = as.yearmon(Duration, "%b-%Y")) %>% 
  #mutate(row = row_number()) %>% 
  #spread(Duration, Correlation) %>% 
  filter(`Event/Custom Dimension` == 'Cabin - Session') %>% 
  mutate(Event_CustomDimension = `Event/Custom Dimension Value`)

ggplot(data=book_df_monthBreakdown, aes(x=Duration, y=Correlation, 
                                          color= Event_CustomDimension,
                                          group = Event_CustomDimension)) +
  geom_line() +
  facet_wrap(~Filter)
         