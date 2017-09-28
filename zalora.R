library(lubridate)
library(tidyverse)
library(stringr)
library(googlesheets)
library(openxlsx)
library(readxl)

zalora <- read_xlsx('C:/Users/User/Desktop/Zalora/Zalora Week 3 Data.xlsx', sheet = 'By Completion Rate')
zalora <- zalora %>%
  spread(Progress,`Number of Users`) %>%
  mutate(key = paste(`Device Type`, Region, `Clip Name`)) %>%
  select(1:3, 9, 4:8) %>%
  rename(`0%` =`0`,
         `25%` = `0.25`,
         `50%` = `0.5`,
         `75%` = `0.75`,
         `100%` = `1`)

# need to re-write this portion. select columns 1:4, sum product views and buy clicks, group by 1:4
zalora_product <- read_xlsx('C:/Users/User/Desktop/Zalora/Zalora Week 3 Data.xlsx', sheet = 'By Product')
zalora_product <- zalora_product %>%
  mutate(key = paste(`Device Type`, Region, `Clip Name`)) %>%
  select(1:4, key, 5:14) %>%
  left_join(zalora[4:9], by = "key" )

wb <- loadWorkbook('C:/Users/User/Desktop/Zalora/Zalora Week 3 Data.xlsx')
addWorksheet(wb, 'By Completion Rate 3', gridLines = FALSE)
addWorksheet(wb, 'By Product 1', gridLines = FALSE)
writeData(wb, sheet = 'By Completion Rate 3', zalora)
writeData(wb, sheet = 'By Product 1', zalora_product)
saveWorkbook(wb,'C:/Users/User/Desktop/Zalora/Zalora Week 3 Data.xlsx',overwrite = T)

