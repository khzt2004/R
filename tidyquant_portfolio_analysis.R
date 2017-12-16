library(tidyquant)
library(ggthemes)
library(lubridate)
library(rpivotTable)
library(tidyverse)
library(quantmod)
library(formattable)
library(alphavantager)

# define portfolio of stocks
stock_portfolio <- c("ADBE", "ATVI", "BABA", "FB", "NVDA", "OV8.SI", "S58.SI", "SHOP")

# provide weights of stocks in portfolio
wts_map <- tibble(
  symbols = stock_portfolio,
  weights = c(0.1, 0.09, 0.1, 0.1, 0.21, 0.16, 0.14, 0.1)
)
wts_map

# get adjusted returns of stocks inportfolio
stock_returns_monthly <- stock_portfolio %>%
  tq_get(get  = "stock.prices",
         from = "2016-01-01",
         to   = "2017-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

# calculate monthly portfolio returns
portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts_map, 
               col_rename  = "Ra_using_wts_map")

# define a baseline for comparison
baseline_returns_monthly <- "SPY" %>%
  tq_get(get  = "stock.prices",
         from = "2016-01-01",
         to   = "2017-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")
baseline_returns_monthly

# join base line and portfolio returns into a single tibble
RaRb_single_portfolio <- left_join(portfolio_returns_monthly, 
                                   baseline_returns_monthly,
                                   by = "date")
RaRb_single_portfolio

# calculate portfolio performance
RaRb_single_portfolio_perf <- RaRb_single_portfolio %>%
  tq_performance(Ra = Ra_using_wts_map, Rb = Rb, performance_fun = table.CAPM)

RaRb_single_portfolio_perf_sharpe <- RaRb_single_portfolio %>%
  tq_performance(Ra = Ra_using_wts_map, Rb = Rb, performance_fun = SharpeRatio)

RaRb_single_portfolio_perf_sortino <- RaRb_single_portfolio %>%
  tq_performance(Ra = Ra_using_wts_map, MAR = 0.008, performance_fun = SortinoRatio)

# calculate any other performance metrics refer to tq_performance_fun_options()
RaRb_single_portfolio_perf_otherfunc <- RaRb_single_portfolio %>%
  tq_performance(Ra = Ra_using_wts_map, Rb = Rb, performance_fun = table.HigherMoments)



# multiple weights comparison for the same portfolio 
# define new weights table
new_weights <- c(
  0.16, 0.14, 0.1, 0.1, 0.21, 0.1, 0.09, 0.1,
  0.1, 0.3, 0.2, 0.1, 0.1, 0.05, 0.1, 0.05,
  0.1, 0.09, 0.1, 0.1, 0.21, 0.16, 0.14, 0.1
)

weights_table <-  tibble(stock_portfolio) %>%
  tq_repeat_df(n = 3) %>%
  bind_cols(tibble(new_weights)) %>%
  group_by(portfolio)
weights_table

# clone stock returns tibble from above, repeat the same no. of times as rows of new weights
stock_returns_monthly_multi <- stock_returns_monthly %>%
  tq_repeat_df(n = 3)

# get multiple portfolio returns
portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")
portfolio_returns_monthly_multi

# join returns table of multiple portfolio
RaRb_multiple_portfolio <- left_join(portfolio_returns_monthly_multi, 
                                     baseline_returns_monthly,
                                     by = "date")
RaRb_multiple_portfolio

# calculate multiple portfolio performance
RaRb_multiple_portfolio_perf<- RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

# calculate multiple portfolio sharpe ratio
RaRb_multiple_portfolio_perf_sharpe <- RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio)

RaRb_multiple_portfolio_perf_sortino <- RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, MAR = 0.008, performance_fun = SortinoRatio)


