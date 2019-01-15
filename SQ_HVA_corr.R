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

