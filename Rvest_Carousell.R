library(rvest)
library(tidyverse)
library(stringr)

url <-"https://sg.carousell.com/categories/mobile-phones-215/iphones-1235/iphone-6-series-1298/?cc_id=1700&collection_id=1298&mobile_model=MOBILE_MODEL_IPHONE_6S_PLUS&mobile_storage=MOBILE_STORAGE_64_GB&sort_by=a"

getPostContent <- function(url){
  read_html(url) %>% 
    html_nodes(".productCardPrice")%>%           
    html_text()
}

r <- getPostContent(url)

a <- read_html(url) %>%
  html_nodes(xpath='//*[@id="productCardThumbnail"]/dl/dd[1]') %>%
  html_text() %>%
  str_replace("[S$]", "") %>%
  str_replace("[/$]", "") %>%
  as.numeric(a)

