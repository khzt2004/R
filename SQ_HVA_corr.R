# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/

library(tidyverse)
library(zoo)
library(ggthemes)
library(extrafont)
font_import()
loadfonts(device = "win")

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
  filter(`Event/Custom Dimension` == 'Cabin - Session') %>% 
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

