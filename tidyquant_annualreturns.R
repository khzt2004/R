data(FANG)
FANG
FANG_annual_returns <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic")
FANG_annual_returns

FANG_annual_returns %>%
  ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "FANG: Annual Returns",
       subtitle = "Get annual returns quickly with tq_transmute!",
       y = "Annual Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2) +
  theme_tq() + 
  scale_fill_tq()

# Get stock pair regression
stock_prices <- c("ADBE", "NVDA") %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2016-12-31") %>%
  group_by(symbol) 

stock_pairs <- stock_prices %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "log",
               col_rename = "returns") %>%
  spread(key = symbol, value = returns)

stock_pairs %>%
  ggplot(aes(x = ADBE, y = NVDA)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Visualizing Returns Relationship of Stock Pairs") +
  theme_tq()

lm(NVDA ~ ADBE, data = stock_pairs) %>%
  summary()