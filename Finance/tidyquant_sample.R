# http://www.business-science.io/code-tools/2017/01/08/tidyquant-update-0-2-0.html
library(tidyquant)

NFLX_key_ratios <- tq_get("NFLX", get = "key.ratios")
NFLX_key_ratios
NFLX_key_ratios %>%
  unnest() %>%
  filter(section == "Valuation Ratios") %>%
  select(category) %>%
  unique()

NFLX_key_ratios %>%
  unnest() %>%
  filter(category == "Price to Earnings") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  labs(title = "P/E Ratios are Easy to Retrieve with tidyquant", 
       x = "", y = "Price to Earnings",
       subtitle = "NFLX Valuation Over Time") +
  lims(y = c(0, 500))

# rolling medians
NFLX_prices <- tq_get("NFLX", get = "stock.prices", from = today() - years(1))
NFLX_prices
NFLX_prices <- NFLX_prices %>%
  tq_mutate(select = close, mutate_fun = rollapply, width = 15, FUN = median) %>%
  tq_mutate(select = close, mutate_fun = rollapply, width = 50, FUN = median) %>%
  rename(median.15 = rollapply,
         median.50 = rollapply.1)
NFLX_prices

my_palette <- c("black", "blue", "red")
NFLX_prices %>%
  select(date, close, median.15, median.50) %>%
  gather(key = type, value = price, close:median.50) %>%
  ggplot(aes(x = date, y = price, col = type)) +
  geom_line() +
  scale_colour_manual(values = my_palette) + 
  theme(legend.position="bottom") +
  ggtitle("Simple Moving Medians are a Breeze with tidyquant") +
  xlab("") + 
  ylab("Stock Price")