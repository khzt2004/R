library(tidyquant)
library(rpivotTable)

# stock_list <- tibble(stocks = c("AVGO", "COHR", "TXN", "INTC", "AMD"))
stock_list <- tibble(stocks = c("Y45.SI", "AVV.SI", "M14.SI", "S58.SI"),
                     company = c(
                       "Singapore Myanmar Investco", 
                       "CEI Ltd", 
                       "Innotek",
                       "SATS"))
stock_prices <- stock_list %>%
  tq_get(get = "stock.prices", from = "2007-01-01", to = "2017-06-01")

stock_prices <- stock_prices %>%
  filter(date > "2017-01-01" & stocks != "S58.SI")

# plot stock prices
industry_chart <- stock_prices %>%
  ggplot(aes(x = date, y = close, color = company)) +
  geom_line() +
  labs(title = "Stock Line Chart", y = "Closing Price", x = "") + 
  theme_tq()

industry_chart

# get key ratios
stock_keystats <- stock_list %>%
  tq_get(get = "key.ratios") %>%
  filter(section == "Valuation Ratios") %>%
  unnest() %>%
  filter(category == "Price to Earnings")

# plot key ratio
ratio_chart <- stock_keystats %>%
  ggplot(aes(x = date, y = value, color = stocks)) +
  geom_line() +
  labs(title = "stock Line Chart", y = "P/E", x = "") + 
  theme_tq()

ratio_chart


