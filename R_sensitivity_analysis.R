library(utils)
library(tidyverse)

price <- c(200, 175, 150)
qty <- c(500, 750, 1000, 1250)

d1 <- expand.grid(price = price, qty = qty)
df <- d1 %>% 
  mutate(avg_price = round(price/qty, 2))

df_sensitivity_table <- df %>%
  mutate(qty = as.factor(paste0("qty_", qty))) %>% 
  spread(qty, avg_price)

rownames(df_sensitivity_table) <- df_sensitivity_table$price
df_sensitivity_table <- df_sensitivity_table %>% select(-price)


base_size <- 9

df_heatmap <- df %>% 
  mutate(price = as.factor(price),
         qty = as.factor(qty))

sensitivity_heatmap <- ggplot(df_heatmap, aes(price, qty)) +
  geom_tile(aes(fill = avg_price), 
            colour = "white", alpha=0.75) +
  geom_text(aes(price, qty, label = avg_price), 
            color = 'black', size = 3) +
  scale_fill_gradient(low = "white", 
                      high = "steelblue") +
  theme_grey(base_size = base_size) +
  labs(x = "price", y = "qty") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(
        text=element_text(size=11, color='#757575'),
        axis.ticks=element_blank(), 
        aspect.ratio = 0.9,
        axis.text.x=element_text(size=base_size*0.9,
                                 angle=330, 
                                 hjust = 0, 
                                 colour="#757575"))

sensitivity_heatmap
