# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/

library(tidyverse)
library(zoo)
library(ggthemes)
library(extrafont)
font_import()
loadfonts(device = "win")

search_df <- read_csv("HVA_Search.csv", na = c("", "na", "NaN", "null"))
book_df <- read_csv("HVA_Book.csv", na = c("", "na", "NaN", "null"))
pagetype_df <- read_csv("SIA_HVA_pagetype_corr.csv", na = c("", "na", "NaN", "null"))
ancillaryMFP_df <- read_csv("ancillaryMFP_final.csv", na = c("", "na", "NaN", "null"))

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
  mutate(Duration = format(as.Date(Duration, format="%b-%Y", frac=1), "%Y-%b")) %>% 
  mutate(Duration = as.yearmon(Duration, "%b-%Y")) %>% 
  #mutate(row = row_number()) %>% 
  #spread(Duration, Correlation) %>% 
  filter(`Event/Custom Dimension` == 'Cabin - Session') %>% 
  mutate(Event_CustomDimension = `Event/Custom Dimension Value`)

search_df_googleplot <- search_df %>%
  filter(Duration != 'All Months' & Filter == 'None (Overall)') %>% 
  select(-`Flight Search or Booking`, -`Col Name`) %>% 
  mutate(Duration = gsub("-18", "-2018", Duration)) %>% 
  mutate(Duration = as.yearmon(Duration, "%b-%Y")) %>% 
  mutate(Duration = as.Date(Duration, format="%b-%Y", frac=1) - 30) %>%
  #mutate(row = row_number()) %>% 
  #spread(Duration, Correlation) %>% 
  filter(`Event/Custom Dimension` == 'Cabin - Session') %>% 
  rename(Event_CustomDimension_Value = `Event/Custom Dimension Value` )

p <- ggplot(search_df_googleplot) +
  geom_line(aes(x = Duration, 
                y = Correlation, 
                colour = Event_CustomDimension_Value),
            size = 1) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand=c(0,0)) +
  theme(legend.position="top",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key=element_blank(),
        plot.margin=margin(10,10,10,10),
        panel.border = element_rect(colour = "#CCCCCC", fill=NA, size=0.8),
        text=element_text(size=11, color='#757575', family="Roboto")) +
  ggtitle(unique(search_df_googleplot$`Event/Custom Dimension`))  

p <- p + scale_color_gdocs()

ggsave(paste0("Search_", unique(search_df_googleplot$`Event/Custom Dimension`),
              ".png"), p, width=5, height=3)


################## Flight Search Heatmap ##################
search_df_heatmap <- search_df %>%
  filter(Duration == 'All Months' & Filter != 'None (Overall)') %>% 
  select(-`Flight Search or Booking`, -`Col Name`, -Duration,
         Event_CustomDimension_Value = `Event/Custom Dimension Value`) %>%
  mutate_at(c("Correlation"), funs(replace(., is.na(.), 0))) %>% 
  arrange(`Event/Custom Dimension`, Filter) %>% 
  mutate(rownumber = row_number())

search_df_heatmap_1 <- search_df_heatmap %>% 
  filter(rownumber <= 224) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
         TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_1 <- ggplot(search_df_heatmap_1, aes(Filter, Event_CD)) +
    geom_tile(aes(fill = Correlation), 
              colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
    scale_fill_gradient(low = "white", 
                        high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        aspect.ratio = 0.9,
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_1

ggsave("search_heatmap_1.png", search_heatmap_1, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)


################# Search Heatmap 2 ############

search_df_heatmap_2 <- search_df_heatmap %>% 
  filter(rownumber >= 225 & rownumber <= 520) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_2 <- ggplot(search_df_heatmap_2, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_2

ggsave("search_heatmap_2.png", search_heatmap_2, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)


################## Search Heatmap 3 ###########

search_df_heatmap_3 <- search_df_heatmap %>% 
  filter(rownumber >= 521 & rownumber <= 728) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_3 <- ggplot(search_df_heatmap_3, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_3

ggsave("search_heatmap_3.png", search_heatmap_3, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)




################## Search Heatmap 4 ###########

search_df_heatmap_4 <- search_df_heatmap %>% 
  filter(rownumber >= 729 & rownumber <= 840) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_4 <- ggplot(search_df_heatmap_4, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_4

ggsave("search_heatmap_4.png", search_heatmap_4, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################## Search Heatmap 5 ###########

search_df_heatmap_5 <- search_df_heatmap %>% 
  filter(rownumber >= 841 & rownumber <= 968) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_5 <- ggplot(search_df_heatmap_5, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_5

ggsave("search_heatmap_5.png", search_heatmap_5, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################## Search Heatmap 6 ###########

search_df_heatmap_6 <- search_df_heatmap %>% 
  filter(rownumber >= 969 & rownumber <= 1056) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_6<- ggplot(search_df_heatmap_6, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_6

ggsave("search_heatmap_6.png", search_heatmap_6, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)


################## Search Heatmap 7 ###########

search_df_heatmap_7 <- search_df_heatmap %>% 
  filter(rownumber >= 1137 & rownumber <= 1240) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_7<- ggplot(search_df_heatmap_7, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_7

ggsave("search_heatmap_7.png", search_heatmap_7, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)




################## Search Heatmap 8 ###########

search_df_heatmap_8 <- search_df_heatmap %>% 
  filter(rownumber >= 1241 & rownumber <= 1352) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_8<- ggplot(search_df_heatmap_8, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_8

ggsave("search_heatmap_8.png", search_heatmap_8, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################## Search Heatmap 9 ###########

search_df_heatmap_9 <- search_df_heatmap %>% 
  filter(rownumber >= 1353 & rownumber <= 1456) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_9<- ggplot(search_df_heatmap_9, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_9

ggsave("search_heatmap_9.png", search_heatmap_9, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################## Search Heatmap 10 ###########

search_df_heatmap_10 <- search_df_heatmap %>% 
  filter(rownumber >= 1457 & rownumber <= 1600) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_10<- ggplot(search_df_heatmap_10, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_10

ggsave("search_heatmap_10.png", search_heatmap_10, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################## Search Heatmap 11 ###########

search_df_heatmap_11 <- search_df_heatmap %>% 
  filter(rownumber >= 1601 & rownumber <= 1760) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_11<- ggplot(search_df_heatmap_11, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_11

ggsave("search_heatmap_11.png", search_heatmap_11, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################## Search Heatmap 12 ###########

search_df_heatmap_12 <- search_df_heatmap %>% 
  filter(rownumber >= 1761 & rownumber <= 1880) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_12<- ggplot(search_df_heatmap_12, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_12

ggsave("search_heatmap_12.png", search_heatmap_12, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)




################## Search Heatmap 13 ###########

search_df_heatmap_13 <- search_df_heatmap %>% 
  filter(rownumber >= 1881 & rownumber <= 2008) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_13<- ggplot(search_df_heatmap_13, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_13

ggsave("search_heatmap_13.png", search_heatmap_13, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################## Search Heatmap 14 ###########

search_df_heatmap_14 <- search_df_heatmap %>% 
  filter(rownumber >= 2009 & rownumber <= 2072) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

search_heatmap_14 <- ggplot(search_df_heatmap_14, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
search_heatmap_14

ggsave("search_heatmap_14.png", search_heatmap_14, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)

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


book_df_googleplot <- book_df %>%
  filter(Duration != 'All Months' & Filter == 'None (Overall)') %>% 
  select(-`Flight Search or Booking`, -`Col Name`) %>% 
  mutate(Duration = gsub("-18", "-2018", Duration)) %>% 
  mutate(Duration = as.yearmon(Duration, "%b-%Y")) %>% 
  mutate(Duration = as.Date(Duration, format="%b-%Y", frac=1) - 30) %>%
  #mutate(row = row_number()) %>% 
  #spread(Duration, Correlation) %>% 
  # filter(`Event/Custom Dimension` == 'Cabin - Session') %>% 
  rename(Event_CustomDimension_Value = `Event/Custom Dimension Value` )


p <- ggplot(book_df_googleplot) +
  geom_line(aes(x = Duration, 
                y = Correlation, 
                colour = Event_CustomDimension_Value),
            size = 1) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand=c(0,0)) +
  theme(legend.position="top",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key=element_blank(),
        plot.margin=margin(10,10,10,10),
        panel.border = element_rect(colour = "#CCCCCC", fill=NA, size=0.8),
        text=element_text(size=11, color='#757575', family="Roboto")) +
  ggtitle(unique(book_df_googleplot$`Event/Custom Dimension`))  

p <- p + guides(colour = guide_legend(nrow = 1)) + scale_color_gdocs()

ggsave(paste0("Book_", unique(book_df_googleplot$`Event/Custom Dimension`),
              ".png"), p, width=5, height=3)


################## Flight Booking Heatmap ##################
book_df_heatmap <- book_df %>%
  filter(Duration == 'All Months' & Filter != 'None (Overall)') %>% 
  select(-`Flight Search or Booking`, -`Col Name`, -Duration,
         Event_CustomDimension_Value = `Event/Custom Dimension Value`) %>%
  mutate_at(c("Correlation"), funs(replace(., is.na(.), 0))) %>% 
  arrange(`Event/Custom Dimension`, Filter) %>% 
  mutate(rownumber = row_number())



################# Book Heatmap 1 ############

book_df_heatmap_1 <- book_df_heatmap %>% 
  filter(rownumber >= 1 & rownumber <= 168) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_1 <- ggplot(book_df_heatmap_1, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_1

ggsave("book_heatmap_1.png", book_heatmap_1, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 2 ############

book_df_heatmap_2 <- book_df_heatmap %>% 
  filter(rownumber >= 169 & rownumber <= 304) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_2 <- ggplot(book_df_heatmap_2, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_2

ggsave("book_heatmap_2.png", book_heatmap_2, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 3 ############

book_df_heatmap_3 <- book_df_heatmap %>% 
  filter(rownumber >= 305 & rownumber <= 520) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_3 <- ggplot(book_df_heatmap_3, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_3

ggsave("book_heatmap_3.png", book_heatmap_3, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)




################# Book Heatmap 4 ############

book_df_heatmap_4 <- book_df_heatmap %>% 
  filter(rownumber >= 521 & rownumber <= 728) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_4 <- ggplot(book_df_heatmap_4, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_4

ggsave("book_heatmap_4.png", book_heatmap_4, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)


################# Book Heatmap 5 ############

book_df_heatmap_5 <- book_df_heatmap %>% 
  filter(rownumber >= 729 & rownumber <= 896) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_5 <- ggplot(book_df_heatmap_5, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_5

ggsave("book_heatmap_5.png", book_heatmap_5, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 6 ############

book_df_heatmap_6 <- book_df_heatmap %>% 
  filter(rownumber >= 897 & rownumber <= 1056) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_6 <- ggplot(book_df_heatmap_6, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_6

ggsave("book_heatmap_6.png", book_heatmap_6, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 7 ############

book_df_heatmap_7 <- book_df_heatmap %>% 
  filter(rownumber >= 1137 & rownumber <= 1352) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_7 <- ggplot(book_df_heatmap_7, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_7

ggsave("book_heatmap_7.png", book_heatmap_7, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 8 ############

book_df_heatmap_8 <- book_df_heatmap %>% 
  filter(rownumber >= 1353 & rownumber <= 1512) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_8 <- ggplot(book_df_heatmap_8, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_8

ggsave("book_heatmap_8.png", book_heatmap_8, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 9 ############

book_df_heatmap_9 <- book_df_heatmap %>% 
  filter(rownumber >= 1513 & rownumber <= 1680) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_9 <- ggplot(book_df_heatmap_9, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_9

ggsave("book_heatmap_9.png", book_heatmap_9, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 10 ############

book_df_heatmap_10 <- book_df_heatmap %>% 
  filter(rownumber >= 1681 & rownumber <= 1880) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_10 <- ggplot(book_df_heatmap_10, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_10

ggsave("book_heatmap_10.png", book_heatmap_10, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 11 ############

book_df_heatmap_11 <- book_df_heatmap %>% 
  filter(rownumber >= 1881 & rownumber <= 2008) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_11 <- ggplot(book_df_heatmap_11, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_11

ggsave("book_heatmap_11.png", book_heatmap_11, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



################# Book Heatmap 12 ############

book_df_heatmap_12 <- book_df_heatmap %>% 
  filter(rownumber >= 2009 & rownumber <= 2072) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_CustomDimension_Value) ~ " ",
                                                 TRUE ~ Event_CustomDimension_Value)) %>% 
  mutate(Event_CD = paste0(`Event/Custom Dimension`,
                           ' ',
                           Event_CustomDimension_Value))

base_size <- 9

book_heatmap_12 <- ggplot(book_df_heatmap_12, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
book_heatmap_12

ggsave("book_heatmap_12.png", book_heatmap_12, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)


########## Page Type Corr Plot ##############

base_size <- 9

pagetype_df <- pagetype_df %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Search_Booking = as.factor(Search_Booking)) %>% 
  arrange(desc(Search_Booking))



pagetype_heatmap <- ggplot(pagetype_df, aes(x = fct_rev(Search_Booking), y = Page_Type)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Search_Booking, Page_Type, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
pagetype_heatmap

ggsave("pagetype_heatmap.png", pagetype_heatmap, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)


######## Ancillary / MFP Dataframe (Search) ################
ancillaryMFP_search_df_overall <- ancillaryMFP_df %>% 
  filter(Duration == 'All Months' & `Flight Search or Booking` == 'Search') %>% 
  select(-`Flight Search or Booking`, -Col_Name)

ancillaryMFP_search_df_overall_tier <- ancillaryMFP_df %>% 
  filter(Duration == 'All Months' &
           grepl('KF Tier', Filter) &
           `Flight Search or Booking` == 'Search') %>% 
  select(-`Flight Search or Booking`, -Col_Name) %>% 
  mutate(row = row_number()) %>% 
  spread(Filter, Correlation)

ancillaryMFP_search_df_overall_cabinSession <- ancillaryMFP_df %>% 
  filter(Duration == 'All Months' &
           grepl('Cabin', Filter) &
           `Flight Search or Booking` == 'Search') %>%
  select(-`Flight Search or Booking`, -Col_Name) %>% 
  mutate(row = row_number()) %>% 
  spread(Filter, Correlation)

ancillaryMFP_search_df_heatmap <- ancillaryMFP_df %>%
  filter(Duration == 'All Months' & 
           Filter != 'None (Overall)' &
           `Flight Search or Booking` == 'Search') %>% 
  select(-`Flight Search or Booking`, -Col_Name, -Duration) %>%
  mutate_at(c("Correlation"), funs(replace(., is.na(.), 0))) %>% 
  arrange(Event_Custom_Dimension, Filter) %>% 
  mutate(rownumber = row_number())


######### Ancillary/MFP (Search) Heatmap 1 #################

ancillaryMFP_search_df_heatmap_1 <- ancillaryMFP_search_df_heatmap %>% 
  filter(rownumber >= 0 & rownumber <= 120) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_search_heatmap_1 <- ggplot(ancillaryMFP_search_df_heatmap_1, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_search_heatmap_1

ggsave("ancillaryMFP_search_heatmap_1.png", ancillaryMFP_search_heatmap_1, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######### Ancillary/MFP (Search) Heatmap 2 #################

ancillaryMFP_search_df_heatmap_2 <- ancillaryMFP_search_df_heatmap %>% 
  filter(rownumber >= 121 & rownumber <= 240) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_search_heatmap_2 <- ggplot(ancillaryMFP_search_df_heatmap_2, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_search_heatmap_2

ggsave("ancillaryMFP_search_heatmap_2.png", ancillaryMFP_search_heatmap_2, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######### Ancillary/MFP (Search) Heatmap 3 #################

ancillaryMFP_search_df_heatmap_3 <- ancillaryMFP_search_df_heatmap %>% 
  filter(rownumber >= 241 & rownumber <= 360) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_search_heatmap_3 <- ggplot(ancillaryMFP_search_df_heatmap_3, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_search_heatmap_3

ggsave("ancillaryMFP_search_heatmap_3.png", ancillaryMFP_search_heatmap_3, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######### Ancillary/MFP (Search) Heatmap 4 #################

ancillaryMFP_search_df_heatmap_4 <- ancillaryMFP_search_df_heatmap %>% 
  filter(rownumber >= 361 & rownumber <= 560) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_search_heatmap_4 <- ggplot(ancillaryMFP_search_df_heatmap_4, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_search_heatmap_4

ggsave("ancillaryMFP_search_heatmap_4.png", ancillaryMFP_search_heatmap_4, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######### Ancillary/MFP (Search) Heatmap 5 #################

ancillaryMFP_search_df_heatmap_5 <- ancillaryMFP_search_df_heatmap %>% 
  filter(rownumber >= 561 & rownumber <= 656) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_search_heatmap_5 <- ggplot(ancillaryMFP_search_df_heatmap_5, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_search_heatmap_5

ggsave("ancillaryMFP_search_heatmap_5.png", ancillaryMFP_search_heatmap_5, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######## Ancillary / MFP Dataframe (Booking) ################
ancillaryMFP_book_df_overall <- ancillaryMFP_df %>% 
  filter(Duration == 'All Months' & `Flight Search or Booking` == 'Booking') %>% 
  select(-`Flight Search or Booking`, -Col_Name)

ancillaryMFP_book_df_overall_tier <- ancillaryMFP_df %>% 
  filter(Duration == 'All Months' &
           grepl('KF Tier', Filter) &
           `Flight Search or Booking` == 'Booking') %>% 
  select(-`Flight Search or Booking`, -Col_Name) %>% 
  mutate(row = row_number()) %>% 
  spread(Filter, Correlation)

ancillaryMFP_book_df_overall_cabinSession <- ancillaryMFP_df %>% 
  filter(Duration == 'All Months' &
           grepl('Cabin', Filter) &
           `Flight Search or Booking` == 'Booking') %>%
  select(-`Flight Search or Booking`, -Col_Name) %>% 
  mutate(row = row_number()) %>% 
  spread(Filter, Correlation)

ancillaryMFP_book_df_heatmap <- ancillaryMFP_df %>%
  filter(Duration == 'All Months' & 
           Filter != 'None (Overall)' &
           `Flight Search or Booking` == 'Booking') %>% 
  select(-`Flight Search or Booking`, -Col_Name, -Duration) %>%
  mutate_at(c("Correlation"), funs(replace(., is.na(.), 0))) %>% 
  arrange(Event_Custom_Dimension, Filter) %>% 
  mutate(rownumber = row_number())



######### Ancillary/MFP (Booking) Heatmap 1 #################

ancillaryMFP_booking_df_heatmap_1 <- ancillaryMFP_book_df_heatmap %>% 
  filter(rownumber >= 0 & rownumber <= 120) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_booking_heatmap_1 <- ggplot(ancillaryMFP_booking_df_heatmap_1, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_booking_heatmap_1

ggsave("ancillaryMFP_booking_heatmap_1.png", ancillaryMFP_booking_heatmap_1, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######### Ancillary/MFP (Booking) Heatmap 2 #################

ancillaryMFP_booking_df_heatmap_2 <- ancillaryMFP_book_df_heatmap %>% 
  filter(rownumber >= 121 & rownumber <= 240) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_booking_heatmap_2 <- ggplot(ancillaryMFP_booking_df_heatmap_2, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_booking_heatmap_2

ggsave("ancillaryMFP_booking_heatmap_2.png", ancillaryMFP_booking_heatmap_2, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######### Ancillary/MFP (Booking) Heatmap 3 #################

ancillaryMFP_booking_df_heatmap_3 <- ancillaryMFP_book_df_heatmap %>% 
  filter(rownumber >= 241 & rownumber <= 360) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_booking_heatmap_3 <- ggplot(ancillaryMFP_booking_df_heatmap_3, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_booking_heatmap_3

ggsave("ancillaryMFP_booking_heatmap_3.png", ancillaryMFP_booking_heatmap_3, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######### Ancillary/MFP (Booking) Heatmap 4 #################

ancillaryMFP_booking_df_heatmap_4 <- ancillaryMFP_book_df_heatmap %>% 
  filter(rownumber >= 361 & rownumber <= 560) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_booking_heatmap_4 <- ggplot(ancillaryMFP_booking_df_heatmap_4, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_booking_heatmap_4

ggsave("ancillaryMFP_booking_heatmap_4.png", ancillaryMFP_booking_heatmap_4, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)



######### Ancillary/MFP (Booking) Heatmap 5 #################

ancillaryMFP_booking_df_heatmap_5 <- ancillaryMFP_book_df_heatmap %>% 
  filter(rownumber >= 561 & rownumber <= 656) %>% 
  mutate(Correlation = round(Correlation, 2)) %>% 
  mutate(Event_CustomDimension_Value = case_when(grepl('Clicked', 
                                                       Event_Custom_Dimension_Value) ~ " ",
                                                 TRUE ~ Event_Custom_Dimension_Value)) %>% 
  mutate(Event_CD = paste0(Event_Custom_Dimension,
                           ' ',
                           Event_Custom_Dimension_Value))

base_size <- 9

ancillaryMFP_booking_heatmap_5 <- ggplot(ancillaryMFP_booking_df_heatmap_5, aes(Filter, Event_CD)) +
  geom_tile(aes(fill = Correlation), 
            colour = "white", alpha=0.75) +
  geom_text(aes(Filter, Event_CD, label = Correlation), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position="none",
        text=element_text(size=11, color='#757575', family="Roboto"),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 family="Roboto",
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))
ancillaryMFP_booking_heatmap_5

ggsave("ancillaryMFP_booking_heatmap_5.png", ancillaryMFP_booking_heatmap_5, 
       width=13.66,
       height=7.05,
       limitsize = FALSE)

