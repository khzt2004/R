
# http://www.mattdancho.com/investments/2016/10/23/SP500_Analysis.html

library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series 
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots
library(corrplot)   # Visuazlize correlation plots
library(magrittr)

getSymbols("S58.SI", from = "2007-01-01", to = "2016-10-23")
S58.SI %>% class()
S58.SI %>% str()
S58.SI %>% head()

S58.SI %>% 
  Ad() %>% 
  chartSeries()

S58.SI %>%
  chartSeries(TA='addBBands();
                    addBBands(draw="p");
                    addVo();
                    addMACD()', 
              subset='2016',
              theme="white"
  ) 

S58.SI %>%
  Ad() %>%
  dailyReturn(type = 'log') %>% 
  head() 

S58.SI_log_returns <- S58.SI %>%
  Ad() %>%
  dailyReturn(type = "log")
names(S58.SI_log_returns) <- "MA.Log.Returns"
# Plot the log-returns    
S58.SI_log_returns %>%    
  ggplot(aes(x = S58.SI_log_returns)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5) 

probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_log_returns <- S58.SI_log_returns %>% 
  quantile(probs = probs, na.rm = TRUE)
dist_log_returns

mean_log_returns <- mean(S58.SI_log_returns, na.rm = TRUE)
sd_log_returns <- sd(S58.SI_log_returns, na.rm = TRUE)
mean_log_returns %>% exp()

# random walk
# Parameters
N     <- 1000
mu    <- mean_log_returns
sigma <- sd_log_returns
day <- 1:N
price_init <- S58.SI$S58.SI.Adjusted[[nrow(S58.SI$S58.SI.Adjusted)]]
# Simulate prices
set.seed(386) 
price  <- c(price_init, rep(NA, N-1))
for(i in 2:N) {
  price[i] <- price[i-1] * exp(rnorm(1, mu, sigma))
}
price_sim <- cbind(day, price) %>% 
  as_tibble()
# Visualize price simulation
price_sim %>%
  ggplot(aes(day, price)) +
  geom_line() +
  ggtitle(str_c("S58.SI: Simulated Prices for ", N," Trading Days"))



# monte carlo simulation
# Parameters
N     <- 252 # Number of Stock Price Simulations
M     <- 250  # Number of Monte Carlo Simulations   
mu    <- mean_log_returns
sigma <- sd_log_returns
day <- 1:N
price_init <- S58.SI$S58.SI.Adjusted[[nrow(S58.SI$S58.SI.Adjusted)]]
# Simulate prices
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)
for (j in 1:M) {
  monte_carlo_mat[[1, j]] <- price_init
  for(i in 2:N) {
    monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
  }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>%
  as_tibble() 
nm <- str_c("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  geom_line(alpha = 0.1) +
  ggtitle(str_c("MA: ", M, 
                " Monte Carlo Simulations for Prices Over ", N, 
                " Trading Days"))

end_stock_prices <- price_sim %>% 
  filter(Day == max(Day))
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
dist_end_stock_prices %>% round(2)

# Inputs
N_hist          <- nrow(S58.SI) / 252
p_start_hist    <- S58.SI$S58.SI.Adjusted[[1]]
p_end_hist      <- S58.SI$S58.SI.Adjusted[[nrow(S58.SI)]]
N_sim           <- N / 252
p_start_sim     <- p_end_hist
p_end_sim       <- dist_end_stock_prices[[4]]
# CAGR calculations
CAGR_historical <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1
CAGR_sim        <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1




