library(tidyverse)

df <- read_csv("cabin_full.csv")

df_filtered <- df %>% 
  select(-Revenue) %>% 
# change the regex to find different funnel data
  filter(grepl("\\[funnel\\]|ConvFunnel_Full", Segment, ignore.case = TRUE)) %>% 
  spread(Custom_Dimension_31, Sessions) %>% 
  arrange(nchar(Segment)) %>% 
  mutate_at(vars(-Segment), 
            .funs = funs(CompletionRate_PrevStage = round(. / lag(.), 2) ,
                         DropoffRate_PrevStage = round(1- (. / lag(.)),2)))

write_csv(df_filtered, "SIA_ConvFunnel.csv")
