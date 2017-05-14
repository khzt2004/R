# Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR 
library(tidyquant)
Ra <- c("FB", "ATVI", "NVDA") %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2017-05-10") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")
Ra
Rb <- "^GSPC" %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2017-05-10") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")
Rb
RaRb <- left_join(Ra, Rb, by = c("date" = "date"))
RaRb
RaRb_capm <- RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)
RaRb_capm

RaRb_annualizedreturn <- RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = NULL, 
                 performance_fun = table.AnnualizedReturns)
RaRb_annualizedreturn