library(tidyquant)
library(lubridate)
library(rpivotTable)
set.seed(10)
tq_get("AAPL", "key.ratios")
sp500 <- tq_index("SP500", use_fallback = TRUE) %>%
  tq_get(get = "stock.prices")

sp500_keystats <- tq_index("SP500", use_fallback = TRUE) %>%
  tq_get(get = "key.stats")

sp500_keystats_selected <- select(sp500_keystats, c(symbol, company, Market.Capitalization))

  
stocks_returns_yearly <- sp500 %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns") %>%
  mutate(year = year(date)) %>%
  left_join(sp500_keystats_selected, by="symbol")

# filter by values
stocks_returns_yearly_spread <- select(stocks_returns_yearly, -date) %>%
  spread(year, yearly.returns) %>%
  filter(`2017` > 0.1 & `2016` > 0.1 & `2015` > 0.1 & `2014` > 0)


rpivotTable(stocks_returns_yearly)
