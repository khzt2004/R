library(rvest)
library(pbapply)
library(TTR)
library(dygraphs)
library(lubridate)
library(tidyquant)
library(rpivotTable)
library(XLConnect)

stock_prices <- tq_get("NVDA", get = "stock.prices", from = "2007-01-01", to  = "2017-06-30")

head(SMA(stock_prices$adjusted, 200))
head(SMA(stock_prices$adjusted, 50))

mov.avgs<-function(stock.df){
  stock.close<-stock.df[,5]
  ifelse((nrow(stock.df)<(2*260)),
         x<-data.frame(stock.df, 'NA', 'NA'),
         x<-data.frame(stock.df, SMA(stock.close, 200), SMA(stock.close, 50)))
  colnames(x)<-c(names(stock.df), 'sma_200','sma_50')
  x<-x[complete.cases(x$sma_200),]
  return(x)
}

stocks_ts<-pblapply(stock_prices, mov.avgs)

graph <- stock_prices %>%
  select(date, adjusted) %>%
  tq_mutate(select     = adjusted, 
            mutate_fun = SMA, 
            n          = 50) %>%
  rename(SMA_50 = SMA) %>%
  tq_mutate(select     = adjusted, 
            mutate_fun = SMA, 
            n          = 200) %>%
  rename(SMA_200 = SMA) %>%
  as_xts(date_col = date) %>%
  dygraph(main = "NVDA Stock Price") %>%
  dyAxis("y", label = "Price") %>% 
  dyRangeSelector(height = 20) %>%
  # Add shading for the recessionary period
  dyShading(from = "2007-12-01", to = "2009-06-01", color = "#FFE6E6") %>% 
  # Add an event for the financial crisis. 
  dyEvent(x = "2008-09-15", label = "Fin Crisis", labelLoc = "top", color = "red")

graph

# simple moving average graph
# find intersection where 50 day MA intersects and moves above 200 day MA
sma_graph <- stock_prices %>%
  select(date, adjusted) %>%
  tq_mutate(select     = adjusted, 
            mutate_fun = SMA, 
            n          = 50) %>%
  rename(SMA_50 = SMA) %>%
  tq_mutate(select     = adjusted, 
            mutate_fun = SMA, 
            n          = 200) %>%
  rename(SMA_200 = SMA) %>%
  as_xts(date_col = date) %>%
  dygraph(main = "NVDA Stock Price") %>%
  dyAxis("y", label = "Price") %>% 
  dyRangeSelector(height = 20) %>%
  # Add shading for the recessionary period
  dyShading(from = "2007-12-01", to = "2009-06-01", color = "#FFE6E6") %>% 
  # Add an event for the financial crisis. 
  dyEvent(x = "2008-09-15", label = "Fin Crisis", labelLoc = "top", color = "red")

sma_graph