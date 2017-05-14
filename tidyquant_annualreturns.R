library(tidyquant)
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

# Asset Returns
FANG_returns_monthly <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")

# Baseline Returns
baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2013-01-01", 
         to   = "2016-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")

returns_joined <- left_join(FANG_returns_monthly, 
                            baseline_returns_monthly,
                            by = "date")
returns_joined

FANG_rolling_corr <- returns_joined %>%
  tq_transmute_xy(x          = monthly.returns.x, 
                  y          = monthly.returns.y,
                  mutate_fun = runCor,
                  n          = 6,
                  col_rename = "rolling.corr.6")

FANG_rolling_corr %>%
  ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(size = 1) +
  labs(title = "FANG: Six Month Rolling Correlation to XLK",
       x = "", y = "Correlation", color = "") +
  facet_wrap(~ symbol, ncol = 2) +
  theme_tq() + 
  scale_color_tq()

# calculate annual log returns for one stock
get_annual_returns <- function(stock.symbol) {
  stock.symbol %>%
    tq_get(get  = "stock.prices",
           from = "2007-01-01",
           to   = "2016-12-31") %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 type       = "log", 
                 period     = "yearly")
}

NFLX_annual_log_returns <- get_annual_returns("NFLX")
NFLX_annual_log_returns

NFLX_annual_log_returns %>%
  ggplot(aes(x = year(date), y = yearly.returns)) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_point(size = 2, color = palette_light()[[3]]) +
  geom_line(size = 1, color = palette_light()[[3]]) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "NFLX: Visualizing Trends in Annual Returns",
       x = "", y = "Annual Returns", color = "") +
  theme_tq()

mod <- lm(yearly.returns ~ year(date), data = NFLX_annual_log_returns)
mod

library(broom)
tidy(mod)
get_model <- function(stock.symbol) {
  annual_returns <- get_annual_returns(stock.symbol)
  mod <- lm(yearly.returns ~ year(date), data = annual_returns)
  tidy(mod)
}


set.seed(10)
stocks <- tq_index("SP500") %>%
  sample_n(5)
stocks

stocks_model_stats <- stocks %>%
  mutate(model = map(symbol, get_model)) %>%
  unnest() %>%
  filter(term == "year(date)") %>%
  arrange(desc(estimate)) %>%
  select(-term)
stocks_model_stats

AMZN <- tq_get("AMZN", get = "stock.prices", from = "2015-09-01", to = "2017-05-13")
AMZN %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = palette_light()[[1]]) + 
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "AMZN Line Chart", 
       subtitle = "Log Scale, Applying Linear Trendline", 
       y = "Adjusted Closing Price", x = "") + 
  theme_tq()

AMZN %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = palette_light()[[1]]) + 
  scale_y_log10() +
  geom_smooth(method = "loess") +
  labs(title = "AMZN Line Chart", 
       subtitle = "Log Scale, Applying Loess Trendline", 
       y = "Adjusted Closing Price", x = "") + 
  theme_tq()