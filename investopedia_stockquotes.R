library(rvest)
library(tidyverse)
library(XML)
library(RCurl)
library(httr)

urls <- c("http://www.investopedia.com/markets/stocks/nvda", 
          "http://www.investopedia.com/markets/stocks/aapl")

h1 <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

prop.info <- vector("list", length = 0)
for (j in 1:length(urls)) {
  prop.info <- c(prop.info, urls[[j]] %>% # Recursively builds the list using each url
                   GET(add_headers("user-agent" = "h1")) %>%
                   read_html() %>% # creates the html object
                   html_nodes(xpath="//td[@id='quotePrice']") %>% # grabs appropriate html element
                   html_text()) # converts it to a text vector
}