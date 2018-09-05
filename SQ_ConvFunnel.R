library(tidyverse)
library(ggthemes)
library(extrafont)
font_import()
loadfonts(device = "win")

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




df <- read_csv("pageloadspeed.csv")
df <- df %>% 
  select(page = "Page",
         pageloadtime = `Avg. Page Load Time (sec)`)

ggplot(df, aes(page, pageloadtime)) +
  geom_bar(aes(fill = pageloadtime), 
           position = "dodge", stat="identity") +
  geom_hline(yintercept = 9.47) +
  coord_flip() +
  theme(text=element_text(size=16,  family="Comic Sans MS"))
theme_gdocs() +
  theme(text=element_text(family="Roboto"))

