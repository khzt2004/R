library(ggplot2)
library(reshape2)

# https://medium.com/@randall.helms/visualizing-the-sales-funnel-a-simple-template-4dfbf0ff4400
#create a simple data frame
step = c('1. Product Page', '2. Shopping Cart', '3. Checkout', '4. Purchase')
users = c(50000, 10000, 5100, 3825) #add in your own numbers here
# ratio is the dropoff rate
ratio = users / users[1]
sales_funnel <- data.frame(step, users, ratio)

#format the data for creating the chart
all <- subset(sales_funnel, ratio == 1)$users

sales_funnel$padding <- (all - sales_funnel$users) / 2
molten <- melt(sales_funnel[, -3], id.var = 'step')
molten <- molten[order(molten$variable, decreasing = T), ]
molten$step <- factor(molten$step, level = rev(levels(molten$step)))

#make the chart
funnel_plot <- ggplot(molten, aes(x = step))  +
  geom_bar(aes(y = value, fill = variable),
           stat = 'identity',
           position = 'stack') +
  geom_text(data = sales_funnel, aes(y = all / 2, label = paste(round(ratio *
                                                                        100), '%')), color = 'black') + scale_fill_manual(values = c("#009E73", 'NA')) +
  coord_flip() +
  theme(legend.position = 'none') +
  labs(x = 'Step', y = 'Users')
print(funnel_plot)