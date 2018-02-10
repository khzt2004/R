library(tidyquant)
library(lubridate)
library(rpivotTable)
library(XLConnect)

set.seed(10)
tq_get("NK", "stock.prices")
sp500 <- tq_index("SP500", use_fallback = TRUE) %>%
  tq_get(get = "stock.prices")

sp500_keystats <- tq_index("SP500", use_fallback = TRUE) %>%
  tq_get(get = "key.stats")

sp500_keystats_selected <- select(sp500_keystats, c(symbol, company, Market.Capitalization))


stocks_returns_yearly <- sp500 %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns") %>%
  mutate(year = year(date)) %>%
  left_join(sp500_keystats_selected, by="symbol")


# filter by values
stocks_returns_yearly_spread <- select(stocks_returns_yearly, -date) %>%
  spread(year, yearly.returns) %>%
  filter(`2007` != "" 
         & `2014` > 0 
         & `2017` > 0 
         & `2016` > 0 
         & `2015` > 0 ) %>%
  gather(year, yearly.returns, 4:14)

rpivotTable(stocks_returns_yearly_spread)

# get info on nasdaq stocks

NYSE <- tq_exchange("NYSE") %>%
  tq_get(get = "stock.prices")

NASDAQ <- tq_exchange("NASDAQ") %>%
  tq_get(get = "stock.prices")

nasdaq_keystats <- tq_exchange("NASDAQ") %>%
  tq_get(get = "key.stats")

nasdaq_keystats_selected <- distinct(select(nasdaq, 1:7))

# get NASDAQ annual returns and join with key stats
NYSE_NASDAQ_returns_yearly <- rbind(NYSE, NASDAQ) %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns") %>%
  mutate(year = year(date)) %>%
  left_join(nasdaq_keystats_selected, by="symbol")

# filter by values, gather back for rpivotTable
NYSE_NASDAQ_returns_yearly_spread <- select(NYSE_NASDAQ_returns_yearly, -date) %>%
  spread(year, yearly.returns) %>%
  filter(`2007` != "" 
         & `2014` > 0 
         & `2017` > 0 
         & `2016` > 0 
         & `2015` > 0 ) %>%
  gather(year, yearly.returns, 8:18)

# make pivot table
rpivotTable(NYSE_NASDAQ_returns_yearly_spread)

# test filter
NYSE_NASDAQ_returns_yearly_spread_test <- NYSE_NASDAQ_returns_yearly %>%
  filter(symbol == "NVDA")

# exporting to excel - create filename

fileDate <- format(Sys.time(), "%m%d%Y")
fileName <- paste("NADSAQ_Export", fileDate, ".xlsx", sep="")

#creating an Excel workbook. Both .xls and .xlsx file formats can be used.
wb <- loadWorkbook(fileName, create = TRUE)

#creating sheets within an Excel workbook
createSheet(wb, name = "NADSAQ_Export")

#writing into sheets within an Excel workbook : 
#writing finaldf data frame into filteredVARs

writeWorksheet(wb, nasdaq_returns_yearly_spread, sheet = "NADSAQ_Export", startRow = 1, startCol = 1)

#saving a workbook to an Excel file :
#saves a workbook to the corresponding Excel file and writes the file to disk.
saveWorkbook(wb)