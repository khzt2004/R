library(tidyquant)
library(rpivotTable)

stock_list <- tibble(stocks = c("AVGO", "COHR", "TXN", "INTC"))
stock_prices <- stock_list %>%
  tq_get(get = "stock.prices", from = "2007-01-01", to = "2017-06-01") %>%
  filter(date > "2016-01-01")


industry_chart <- stock_prices %>%
  ggplot(aes(x = date, y = close, color = stocks)) +
  geom_line() +
  labs(title = "stock Line Chart", y = "Closing Price", x = "") + 
  theme_tq()

industry_chart


