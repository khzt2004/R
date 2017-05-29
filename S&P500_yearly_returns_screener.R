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

# sp500_filter <- filter(sp500_keystats, symbol == "jpm")
  
stocks_returns_yearly <- sp500 %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns") %>%
  mutate(year = year(date)) %>%
  left_join(sp500_keystats_selected, by="symbol")


rpivotTable(stocks_returns_yearly)
