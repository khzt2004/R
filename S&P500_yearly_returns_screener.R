library(tidyquant)
set.seed(10)
sp500 <- tq_index("SP500") %>%
  slice(1:3) %>%
  tq_get(get = "stock.prices")

stocks_returns_yearly <- sp500 %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns") 