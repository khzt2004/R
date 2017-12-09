library(tidyquant)
library(ggthemes)
library(lubridate)
library(rpivotTable)
library(tidyverse)
library(quantmod)

# get stock returns and define weights for first portfolio
#"NVDA", "FB", "BABA", "ADBE"
portfolio1 <- c("SPY")
stock_returns_monthly <- portfolio1 %>%
  tq_get(get  = "stock.prices",
         from = "2005-01-01",
         to   = "2017-12-01") %>%
  mutate(symbol = "SPY") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") %>%
  mutate(portfolio = '1')

wts <- c(1)

# get stock returns and define weights for second portfolio
portfolio2 <- c("OV8.SI", "S58.SI", "MAIN")
stock_returns_monthly_2 <- portfolio2 %>%
  tq_get(get  = "stock.prices",
         from = "2005-01-01",
         to   = "2017-12-01") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") %>%
  mutate(portfolio = '2')

wts1 <- c(0.3, 0.3, 0.4)

# define investment sum
principal_sum <- 60000

# find growth in investment for the first portfolio
portfolio_growth_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = wts, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * principal_sum) %>%
  mutate(portfolio = '1')

# find growth in investment for the second portfolio
portfolio_growth_monthly_2 <- stock_returns_monthly_2 %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = wts1, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * principal_sum) %>%
  mutate(portfolio = '2')

# bind the two investment growth tables together
portfolio_growth_monthly_bind <- rbind(portfolio_growth_monthly, portfolio_growth_monthly_2)


# plot single portfolio
portfolio_growth_monthly %>%
  ggplot(aes(x = date, y = investment.growth)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Portfolio Growth",
       subtitle = "50% AAPL, 0% GOOG, and 50% NFLX",
       caption = "Now we can really visualize performance!",
       x = "", y = "Portfolio Value") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)


# plot multiple portfolio

portfolio_growth_monthly_bind %>%
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
  geom_line(size = 2) +
  labs(title = "Portfolio Growth",
       subtitle = paste0("Comparing Multiple Portfolios: \n 1. ", paste(portfolio1,collapse=", "), "\n 2. ", paste(portfolio2,collapse=", ")),
       caption = "See whether portfolio beats the index",
       x = "", y = "Portfolio Value",
       color = "Portfolio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

# show the returns at the end of investment period for first portfolio
returns_diff <- portfolio_growth_monthly_bind %>%
  filter(portfolio == '1') %>%
  filter(date == max(date))
returns_diff$investment.growth

# show the % growth of principal investment at end of investment period for first portfolio
paste0(round((((returns_diff$investment.growth/principal_sum)-1)*100), 3), "%")
             
# show the returns at the end of investment period for second portfolio
returns_diff1 <- portfolio_growth_monthly_bind %>%
  filter(portfolio == '2') %>%
  filter(date == max(date))
returns_diff1$investment.growth

# show the % growth of principal investment at end of investment period for second portfolio
paste0(round((((returns_diff1$investment.growth/principal_sum)-1)*100), 3), "%")

# show the % difference in portfolio amount at end of investment period
paste0(round((100*Delt(returns_diff$investment.growth, returns_diff1$investment.growth)), 3), "%")

# show the monetary difference in investment returns at end of investment period
returns_diff$investment.growth-returns_diff1$investment.growth


