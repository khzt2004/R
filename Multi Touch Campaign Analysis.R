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

rpivotTable(dfmultitouch1)

# create table of lic dollars for each ID
df_licdollars <- dfmultitouch1 %>% 
  select(End.Account.Pseudo.ID, Lic.Dollars) %>%
  replace_na(list(Lic.Dollars=0)) %>%
  group_by(End.Account.Pseudo.ID) %>%
  summarise(Lic_Dollars = sum(Lic.Dollars))

rpivotTable(df_licdollars)

# count of different activities for each ID
dfmultitouch_count <- dfmultitouch1 %>%
  select(End.Account.Pseudo.ID, Sub.Delivery.Mechanism, Lic.Dollars) %>%
  group_by(End.Account.Pseudo.ID, Sub.Delivery.Mechanism) %>%
  summarize(count=n()) %>%
  spread(Sub.Delivery.Mechanism, count) %>%
  left_join(df_licdollars, by = "End.Account.Pseudo.ID")

dfmultitouch_count[is.na(dfmultitouch_count)] <- 0

rpivotTable(dfmultitouch_count)

# stacked bar chart for each ID 
EndAccountTest_bar <- dfmultitouch1 %>%
  select(End.Account.Pseudo.ID, Sub.Delivery.Mechanism, makedate, Lic.Dollars) %>%
  group_by(End.Account.Pseudo.ID, Sub.Delivery.Mechanism, makedate) %>%
  summarize(count=n()) %>%
  filter(End.Account.Pseudo.ID %in% c("ANGUSR7N", "BANGKO1T", "KAOCON1T")) %>%
  ggplot(aes(x = End.Account.Pseudo.ID, y = count, fill = Sub.Delivery.Mechanism)) +
  geom_bar(stat = 'identity')

EndAccountTest_bar

# stacked bar chart for each ID by date
EndAccountTest_bar_date <- dfmultitouch1 %>%
  select(End.Account.Pseudo.ID, Sub.Delivery.Mechanism, makedate, Lic.Dollars) %>%
  group_by(End.Account.Pseudo.ID, Sub.Delivery.Mechanism, makedate) %>%
  summarize(count=n()) %>%
  filter(End.Account.Pseudo.ID %in% c("ANGUSR7N", "BANGKO1T", "KAOCON1T")) %>%
  ggplot(aes(x = makedate, y = count, fill = Sub.Delivery.Mechanism)) +
  geom_bar(stat = 'identity')

EndAccountTest_bar_date

# line plot scorecard product breakdown by date
EndAccountTest <- dfmultitouch1 %>%
  select(End.Account.Pseudo.ID, Scorecard.Product, Sub.Delivery.Mechanism, makedate, Lic.Dollars) %>%
  group_by(End.Account.Pseudo.ID, Scorecard.Product, Sub.Delivery.Mechanism, makedate) %>%
  summarize(count=n()) %>%
  ggplot(aes(x = makedate, color = Scorecard.Product)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

EndAccountTest

# country table by ID
EndAccountSummary <- dfmultitouch1 %>%
  filter(End.Account.Pseudo.ID == "ANGUSR7N") %>%
  group_by(End.Account.Pseudo.ID, Scorecard.Product, Lead.Account.Country) %>%
  summarise(n = n())


rpivotTable(EndAccountSummary)
