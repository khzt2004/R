library(tidyverse)
library(ggthemes)
library(extrafont)
# font_import()
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


tier_df <- read_csv("tier_full.csv")
tier_df_filtered <- tier_df %>%
  mutate(krisflyer_final = KrisFlyer + KRISFLYER,
         EliteGold_final = `Elite Gold` + `KRISFLYER ELITE GOLD`,
         EliteSilver_final = `Elite Silver` + `KRISFLYER ELITE SILVER`) %>% 
  select(-KrisFlyer, -KRISFLYER, -`Elite Gold`, -`KRISFLYER ELITE GOLD`,
         -`Elite Silver`, 
         -`KRISFLYER ELITE SILVER`) %>% 
  arrange(nchar(Segment)) %>% 
  mutate_at(vars(-Segment), 
            .funs = funs(CompletionRate_PrevStage = round(. / lag(.), 2) ,
                         DropoffRate_PrevStage = round(1- (. / lag(.)),2)))

write_csv(tier_df_filtered, "SIA_ConvFunneltier.csv")
  

df <- read_csv("pageloadspeed.csv")
df <- df %>% 
  select(page = "Page",
         pageloadtime = `Avg. Page Load Time (sec)`)

ggplot(df, aes(page, pageloadtime)) +
  geom_bar(fill = "#4285f4", 
           position = "dodge", stat="identity") +
  geom_hline(yintercept = 9.47) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_gdocs() +
  theme(legend.position = "top",
    text=element_text(colour = "#666666", 
                          family="Ubuntu"),
        legend.direction="horizontal",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


