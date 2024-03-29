library(XLConnect)
library(tidyverse)
library(data.table)
library(rpivotTable)
library(lubridate)
library(readxl)

# replace file name with name of appropriate source file 
# df <- readWorksheetFromFile("results-GAData.xlsx", sheet = 1)
df <- read_excel("results-GAData.xlsx")
df_bigquery <- as_tibble(df)

df_bigquery1 <- df_bigquery %>%
  mutate(date = ymd(date)) %>%
  mutate(visitStartTime_stamp = ymd_hms(as.POSIXct(visitStartTime, origin="1970-01-01"))) %>%
  mutate(day = wday(date, label = TRUE, abbr = FALSE)) %>%
  mutate(month = month(date, label = TRUE, abbr = FALSE)) %>%
  mutate(hour = hour(visitStartTime_stamp)) %>%
  mutate(minute = minute(visitStartTime_stamp)) %>%
  mutate(second = second(visitStartTime_stamp)) %>%
  mutate(visitStartTime_hms = format(visitStartTime_stamp, "%H:%M:%S"))

mutate(departure = make_datetime(year, month, day, hour, minute))
rpivotTable(df_bigquery1)