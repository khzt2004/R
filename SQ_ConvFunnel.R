library(tidyverse)

df <- read_csv("cabin_full.csv")

df_filtered <- df %>% 
  select(-Revenue) %>% 
  filter(grepl("\\[funnel\\]|ConvFunnel_Full", Segment, ignore.case = TRUE)) %>% 
  spread(Custom_Dimension_31, Sessions) %>% 
  arrange(nchar(Segment)) %>% 
  mutate_at(vars(-Segment), .funs = funs(CompletionRate = . / lag(.) ))
