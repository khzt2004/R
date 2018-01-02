library(rvest)
library(tidyverse)
library(stringr)

url <-"https://sg.carousell.com/categories/mobile-phones-215/iphones-1235/iphone-6-series-1298/?cc_id=1700&collection_id=1298&mobile_model=MOBILE_MODEL_IPHONE_6S_PLUS&mobile_storage=MOBILE_STORAGE_64_GB&sort_by=a"
price <- read.csv("6splus_price.csv")
price <- as.data.frame(price)
price1 <- as.character(price$Price)
price1 <- price1 %>%
  str_replace("[S$]", "") %>%
  str_replace("[/$]", "") %>%
  str_replace("[/,]", "") %>%
  as.numeric() %>%
  as.data.frame

colnames(price1) <- c("Price")
price1 <- price1 %>%
  filter(Price < 5000)


getPostNameDate <- function(url){
  hyperlink <- read_html(url)
  postname <- hyperlink %>%
    html_nodes(xpath='//*[@class="r-w media-heading"]') %>%
    html_text()
  postname <- as.list(postname)
  postdate <- hyperlink %>%
    html_nodes(xpath='//*[@class="r-R media-body"]/time/span') %>%
    html_text()
  postdate <- as.list(postdate)
  postprice <- hyperlink %>%
    html_nodes(xpath='//*[@class="r-K"]/a/dl/dd[1]') %>%
    html_text() %>%
    str_replace("[S$]", "") %>%
    str_replace("[/$]", "") %>%
    str_replace("[/,]", "") %>%
    as.numeric()
  postprice <- as.list(postprice)
  posttable <- t(rbind(postname, postdate, postprice))
  #posttable <- as_tibble(posttable)
  #posttable$postprice <- as.numeric(posttable$postprice)
  #posttable$postname <- as.character(posttable$postname)
  #posttable$postdate <- as.character(posttable$postdate)
}


getNextUrl <- function(url) {
  read_html(url) %>% 
    html_node(xpath='//*[@class="pagination-next pagination-btn"]/a') %>%
    html_attr("href")
}



scrapeBackApply <- function(url, n) {
  sapply(1:n, function(x) {
    r <- getPostNameDate(url)
    # Overwrite global 'url'
    Sys.sleep(3)
    url <<- paste0("https://sg.carousell.com", getNextUrl(url))
    r
  })
}

res <- scrapeBackApply(url, 5)
res1 <- as_tibble(res)
df <- data.frame(Reduce(rbind, res))

# histogram of price distribution
ggplot(data=price1, aes(x=price1$Price)) + 
  geom_histogram(binwidth=20) +
  scale_x_continuous(breaks = seq(0, max(price1$Price)+100, 50)) + 
  labs(title="Histogram for Price", x="Price", y="Count")