library(tidyverse)
library(plotly)
library(scales)
options(scipen=999)

return_calc <- function(initial, nperiods, pct_returns, cash_in_out) {
  
  starting_amount <- c(initial)
 
  period <- seq(1, nperiods)
 
  age <- seq(28, 28+nperiods-1)
  
  withdrawals_deposits <- rep(cash_in_out, nperiods)
  # to change the withdrawal or deposit in any period
  # withdrawals_deposits[c(2,4)] <- c(30000, -20000) # number(s) in round bracket is year number
  
  growth_rate <- rep(pct_returns, nperiods)
  # to change the growth rates in any period
  # growth_rate[c(2,4,5,9,10)] <- c(pct_returns+0.05, pct_returns-0.1, pct_returns-0.1, pct_returns+0.08, pct_returns+0.08) # number(s) in round bracket is year number
  
  withdrawals_deposit_amount <- c()
  growth_pct <- c()
  endperiod <- c()
  starting_amount <- c(initial)
  for(i in 1:nperiods) {
    withdrawals_deposit_amount <- append(withdrawals_deposit_amount, withdrawals_deposits[i])
    growth_pct <- append(growth_pct, growth_rate[i])
    endperiod <- append(endperiod, (starting_amount[i] + withdrawals_deposits[i])*(1+growth_rate[i]))
   starting_amount <- append(starting_amount, endperiod[i])
  }
  
  return(calc_values <- list(age = age,
                             period = period,
                             starting_amt = starting_amount[-(length(starting_amount))],
                             withdrawals_deposit_amount = withdrawals_deposit_amount,
                             growth_pct = growth_pct, 
                             returns = endperiod
                             ))
}

return_df <- as.tibble(return_calc(100000, 20, 0.3, 15000))
return_df <- return_df %>% 
  mutate(scenario = "base case")

bull_case_df <- as.tibble(return_calc(100000, 20, 0.3, 20000))
bull_case_df <- bull_case_df %>% 
  mutate(scenario = "bull case")

bear_case_df <- as.tibble(return_calc(100000, 20, 0.3, 10000))
bear_case_df <- bear_case_df %>% 
  mutate(scenario = "bear case")

combined_df <- return_df %>% 
  bind_rows(bull_case_df, bear_case_df)



### plot the graph of returns
p <- ggplot(data=combined_df, aes(x=age, y=returns, color = scenario)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(labels=comma) +
  labs(title="Plot of Returns by Age",
       color = "Growth Rate") +
  theme_minimal()

p <- p %>% 
  ggplotly() %>% 
  layout(separators = '.,')

p

# all things equal, increase in annual growth rates decreases the positive impact of annual cash deposits
# at 10-30% annual pct gains, 
# doubling annual cash deposits will give a 48%-30% difference in returns after 20 years
