
# http://www.mattdancho.com/investments/2016/10/23/SP500_Analysis.html

library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series 
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots
library(corrplot)   # Visuazlize correlation plots
library(magrittr)

# Web-scrape SP500 stock list
sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_node("table.wikitable") %>%
  html_table() %>%
  select(`Ticker symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
  as_tibble()
# Format names
names(sp_500) <- sp_500 %>% 
  names() %>% 
  str_to_lower() %>% 
  make.names()
# Show results
sp_500

sp_500 %>% 
  lapply(function(x) x %>% unique() %>% length()) %>%
  unlist() # show in condensed format

sp_500 %>%
  group_by(security) %>%
  summarize(count = n()) %>%
  filter(count > 1)

sp_500 %>% 
  filter(security == "Under Armour")

sp_500 <- sp_500 %>% 
  filter(ticker.symbol != "UA.C")

sp_500 %>%
  # Summarise data by frequency
  group_by(gics.sector) %>%
  summarise(count = n()) %>%
  # Visualize 
  ggplot(aes(x = gics.sector %>% fct_reorder(count),
             y = count
  )) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), size = 3, nudge_y = 4, nudge_x = .1) + 
  scale_y_continuous(limits = c(0,100)) +
  ggtitle(label = "Sector Frequency Among SP500 Stocks") +
  xlab(label = "GICS Sector") +
  theme(plot.title = element_text(size = 16)) + 
  coord_flip() 

get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  # Get stock prices
  stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
  # Rename
  names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    stock_prices <- stock_prices_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    stock_prices <- stock_prices_xts
  }
  stock_prices
}

get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
  # Convert tibble to xts
  if (!is.xts(x)) {
    x <- xts(x[,-1], order.by = x$Date)
  }
  # Get log returns
  log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
  # Rename
  names(log_returns_xts) <- "Log.Returns"
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    log_returns <- log_returns_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    log_returns <- log_returns_xts
  }
  log_returns
}

sp_500 <- sp_500 %>%
  mutate(
    stock.prices = map(ticker.symbol, 
                       function(.x) get_stock_prices(.x, 
                                                     return_format = "tibble",
                                                     from = "2007-01-01",
                                                     to = "2016-10-23")
    ),
    log.returns  = map(stock.prices, 
                       function(.x) get_log_returns(.x, return_format = "tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )  

library(plotly)
plot_ly(data   = sp_500,
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,
        color  = ~ n.trade.days,
        colors = "Blues",
        size   = ~ n.trade.days,
        text   = ~ str_c("<em>", security, "</em><br>",
                         "Ticker: ", ticker.symbol, "<br>",
                         "Sector: ", gics.sector, "<br>",
                         "Sub Sector: ", gics.sub.industry, "<br>",
                         "No. of Trading Days: ", n.trade.days),
        marker = list(opacity = 0.8,
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
) %>%
  layout(title   = 'S&amp;P500 Analysis: Stock Risk vs Reward',
         xaxis   = list(title = 'Risk/Variability (StDev Log Returns)',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
         yaxis   = list(title = 'Reward/Growth (Mean Log Returns)',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwith = 2),
         margin = list(l = 100,
                       t = 100,
                       b = 100),
         font   = list(color = '#FFFFFF'),
         paper_bgcolor = 'rgb(0, 0, 0)',
         plot_bgcolor = 'rgb(0, 0, 0)')

# From the plot we can see that a number of stocks have a unique 
# combination of high mean and low standard deviation log returns. 
# We can isolate them

sp_500 %>%
  filter(mean.log.returns >= 0.001,
         sd.log.returns < 0.0315) %>%
  select(ticker.symbol, mean.log.returns:n.trade.days) %>%
  arrange(mean.log.returns %>% desc())

# compute correlation

limit <- 30
sp_500_hp <- sp_500 %>%
  filter(n.trade.days > 1000) %>%
  filter(sd.log.returns < 0.0315) %>%
  mutate(rank = mean.log.returns %>% desc() %>% min_rank()) %>%
  filter(rank <= limit) %>%
  arrange(rank) %>%
  select(ticker.symbol, rank, mean.log.returns, sd.log.returns, log.returns)
sp_500_hp  

sp_500_hp_unnest <- sp_500_hp %>%
  select(ticker.symbol, log.returns) %>%
  unnest()
sp_500_hp_unnest

sp_500_hp_spread <- sp_500_hp_unnest %>%
  spread(key = ticker.symbol, value = Log.Returns) %>%
  na.omit()
sp_500_hp_spread

sp_500_hp_cor <- sp_500_hp_spread %>%
  select(-Date) %>%
  cor() 
sp_500_hp_cor[1:6, 1:6] # show first 6 columns and rows

sp_500_hp_cor %>%
  corrplot(order   = "hclust", 
           addrect = 11)