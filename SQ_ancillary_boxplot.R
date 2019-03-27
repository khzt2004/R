library(tidyverse)
library(forcats)
library(plotly)

df <- read_csv("SIA_DaysToDeparture_Ancillary.csv")
df_Mar2019 <- read_csv("SIA_Ancillary_Mar2019.csv")
df <- df %>% 
  mutate(Product = as.factor(Product)) %>% 
  filter(!is.na(Product) & Days_To_Departure >= 0)

df_Mar2019 <- df_Mar2019 %>% 
  mutate(Product = as.factor(Product)) %>% 
  filter(!is.na(Product) & Days_To_Departure >= 0)

p <- ggplot(df, aes(x= fct_reorder(Product, Days_To_Departure, .fun=mean), y = Days_To_Departure,
                    fill=Product)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  coord_flip() +
  ggtitle("Ancillary Products by Days to Departure") +
  xlab("Product") +
  ylab("Days to Departure") 
p <- p + theme_minimal() + theme(legend.position="none",
                                 plot.title = element_text(size=14) )
  
p

gp <- ggplotly(p)
gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1
  
gp <- gp %>% layout(margin = list(r=80, l=210),
                     annotations = c(list(text = "Ancillary Product",
                                          x = -0.65,
                                          xref = "paper",
                                          showarrow = F,
                                          textangle = -90)))
gp

htmlwidgets::saveWidget(widget=gp, 'ancillary_boxplot.html', selfcontained = FALSE)


p_Mar2019 <- ggplot(df_Mar2019, aes(x= fct_reorder(Product, Days_To_Departure, .fun=mean), y = Days_To_Departure,
                    fill=Product)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  coord_flip() +
  ggtitle("Ancillary Products by Days to Departure") +
  xlab("Product") +
  ylab("Days to Departure") 
p_Mar2019 <- p_Mar2019 + theme_minimal() + theme(legend.position="none",
                                 plot.title = element_text(size=14) )

p_Mar2019

gp_Mar2019 <- ggplotly(p_Mar2019)
gp_Mar2019[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1

gp_Mar2019 <- gp_Mar2019 %>% layout(margin = list(r=80, l=210),
                    annotations = c(list(text = "Ancillary Product",
                                         x = -0.65,
                                         xref = "paper",
                                         showarrow = F,
                                         textangle = -90)))
gp_Mar2019

htmlwidgets::saveWidget(widget=gp_Mar2019, 'ancillary_boxplot_Mar2019.html', selfcontained = FALSE)


df_calc <- df %>% 
  select(Product, Days_To_Departure, Unique_Purchases, Product_Revenue ) %>% 
  group_by(Product) %>% 
  summarise(Avg_Days_To_Departure = mean(Days_To_Departure),
         Unique_Purchases = sum(Unique_Purchases),
         Product_Revenue = sum(Product_Revenue))

df_Mar2019_calc <- df_Mar2019  %>% 
  select(Product, Days_To_Departure, Unique_Purchases, Product_Revenue ) %>% 
  group_by(Product) %>% 
  summarise(Avg_Days_To_Departure = mean(Days_To_Departure),
            Unique_Purchases = sum(Unique_Purchases),
            Product_Revenue = sum(Product_Revenue))


write_csv(df_calc, "ancillary_table.csv")
write_csv(df_Mar2019_calc, "ancillary_table_Mar2019.csv")
