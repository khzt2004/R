# loading libraries
library(tidyverse)
library(reshape2)
library(ggplot2)

# http://analyzecore.com/2015/02/16/customer-segmentation-lifecycle-grids-with-r/

# creating data sample
set.seed(10)
data <- data.frame(orderId=sample(c(1:1000), 5000, replace=TRUE),
                   product=sample(c('NULL','a','b','c'), 5000, replace=TRUE,
                                  prob=c(0.15, 0.65, 0.3, 0.15)))
order <- data.frame(orderId=c(1:1000),
                    clientId=sample(c(1:300), 1000, replace=TRUE))
gender <- data.frame(clientId=c(1:300),
                     gender=sample(c('male', 'female'), 300, replace=TRUE, prob=c(0.40, 0.60)))
date <- data.frame(orderId=c(1:1000),
                   orderdate=sample((1:100), 1000, replace=TRUE))
orders <- merge(data, order, by='orderId')
orders <- merge(orders, gender, by='clientId')
orders <- merge(orders, date, by='orderId')
orders <- orders[orders$product!='NULL', ]
orders$orderdate <- as.Date(orders$orderdate, origin="2012-01-01")
rm(data, date, order, gender)

# reporting date
today <- as.Date('2012-04-11', format='%Y-%m-%d')

# processing data
orders <- dcast(orders, orderId + clientId + gender + orderdate ~ product, value.var='product', fun.aggregate=length)

orders <- orders %>%
  group_by(clientId) %>%
  mutate(frequency=n(),
         recency=as.numeric(today-orderdate)) %>%
  filter(orderdate==max(orderdate)) %>%
  filter(orderId==max(orderId)) %>%
  ungroup()

# exploratory analysis
ggplot(orders, aes(x=frequency)) +
  theme_bw() +
  scale_x_continuous(breaks=c(1:10)) +
  geom_histogram(alpha=0.6, binwidth=1) +
  ggtitle("Dustribution by frequency")

ggplot(orders, aes(x=recency)) +
  theme_bw() +
  geom_histogram(alpha=0.6, binwidth=1) +
  ggtitle("Dustribution by recency")

# Next, add segments to each client based on the boundaries. 
# Create new variable 'cart', which includes products from the last cart, 
# for doing in-depth analysis.

orders.segm <- orders %>%
  mutate(segm.freq=ifelse(between(frequency, 1, 1), '1',
                          ifelse(between(frequency, 2, 2), '2',
                                 ifelse(between(frequency, 3, 3), '3',
                                        ifelse(between(frequency, 4, 4), '4',
                                               ifelse(between(frequency, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(recency, 0, 6), '0-6 days',
                         ifelse(between(recency, 7, 13), '7-13 days',
                                ifelse(between(recency, 14, 19), '14-19 days',
                                       ifelse(between(recency, 20, 45), '20-45 days',
                                              ifelse(between(recency, 46, 80), '46-80 days', '>80 days')))))) %>%
  # creating last cart feature
  mutate(cart=paste(ifelse(a!=0, 'a', ''),
                    ifelse(b!=0, 'b', ''),
                    ifelse(c!=0, 'c', ''), sep='')) %>%
  arrange(clientId)

# defining order of boundaries
orders.segm$segm.freq <- factor(orders.segm$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
orders.segm$segm.rec <- factor(orders.segm$segm.rec, levels=c('>80 days', '46-80 days', '20-45 days', '14-19 days', '7-13 days', '0-6 days'))

# combine clients into segments
lcg <- orders.segm %>%
  group_by(segm.rec, segm.freq) %>%
  summarise(quantity=n()) %>%
  mutate(client='client') %>%
  ungroup()

lcg.matrix <- dcast(lcg, segm.freq ~ segm.rec, value.var='quantity', fun.aggregate=sum)

ggplot(lcg, aes(x=client, y=quantity, fill=quantity)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(quantity)/2, label=quantity), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids")

lcg.adv <- lcg %>%
  mutate(rec.type = ifelse(segm.rec %in% c(">80 days", "46-80 days", "20-45 days"), "not recent", "recent"),
         freq.type = ifelse(segm.freq %in% c(">5", "5", "4"), "frequent", "infrequent"),
         customer.type = interaction(rec.type, freq.type))

ggplot(lcg.adv, aes(x=client, y=quantity, fill=customer.type)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_grid(segm.freq ~ segm.rec) +
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(quantity)/2, label=quantity), size=4) +
  ggtitle("LifeCycle Grids")

# with background
ggplot(lcg.adv, aes(x=client, y=quantity, fill=customer.type)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_rect(aes(fill = customer.type), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1) +
  facet_grid(segm.freq ~ segm.rec) +
  geom_bar(stat='identity', alpha=0.7) +
  geom_text(aes(y=max(quantity)/2, label=quantity), size=4) +
  ggtitle("LifeCycle Grids")

lcg.sub <- orders.segm %>%
  group_by(gender, cart, segm.rec, segm.freq) %>%
  summarise(quantity=n()) %>%
  mutate(client='client') %>%
  ungroup()

ggplot(lcg.sub, aes(x=client, y=quantity, fill=gender)) +
  theme_bw() +
  scale_fill_brewer(palette='Set1') +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', position='fill' , alpha=0.6) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids by gender (propotion)")

ggplot(lcg.sub, aes(x=gender, y=quantity, fill=cart)) +
  theme_bw() +
  scale_fill_brewer(palette='Set1') +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', position='fill' , alpha=0.6) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids by gender and last cart (propotion)")