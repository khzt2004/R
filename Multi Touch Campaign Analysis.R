library(XLConnect)
library(tidyverse)
library(data.table)
library(rpivotTable)
library(lubridate)

# replace file name with name of appropriate source file 
df <- readWorksheetFromFile("Multi Touch Campaign Performance - Main Report.xlsx", sheet = 1)
dfmultitouch <- as_tibble(df)

dfmultitouch1 <- dfmultitouch %>%
  mutate(created_month = month(Lead.Created.Date, label = TRUE)) %>%
  mutate(created_year = year(Lead.Created.Date)) %>%
  mutate(created_day = wday(Lead.Created.Date, label = TRUE)) %>%
  mutate(makedate = as_date(Lead.Created.Date))

EndAccountTest <- dfmultitouch1 %>%
  filter(End.Account.Pseudo.ID == "ANGUSR7N")

EndAccountTest %>%
  ggplot(aes(x = Lead.Created.Date, color = Scorecard.Product)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

EndAccountSummary <- dfmultitouch1 %>%
  filter(End.Account.Pseudo.ID == "ANGUSR7N") %>%
  group_by(End.Account.Pseudo.ID, Scorecard.Product, Lead.Account.Country) %>%
  summarise(n = n())


rpivotTable(EndAccountSummary)
