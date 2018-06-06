library(tidyverse)
library(bigrquery)
library(lubridate)
library(googlesheets)
library(reshape2)
library(zoo)
library(rvest)
library(RCurl)
library(XML)
library(xml2)

# get data from Googlesheets - what if owner of google sheet is different
my_sheets <- gs_ls()

Etalase_news <- gs_key("1dD18QMp3_VCVpFwKYeoG87RAM6_qlAEWM41NKVJgbrA")
L6_googletrends <- gs_key("1IkPRSwzq-QfDEwvIqBWhsxy3ye2eY5k7Rv8qn6wt01I")
Trend_monitor_Indo <- gs_key("1mV1X6xt9vf1L7VEsWafkVuWh1sXfDMp9dYNWXpL5HtQ")
liputan6_tag <- "http://liputan6.com/tag/"
googlenews_urlsnippet_start <- "https://news.google.com/news/search/section/q/"
googlenews_urlsnippet_end <- "?hl=id&gl=ID&ned=id_id"
googletrends_urlsnippet_start <- "https://trends.google.com/trends/explore?q="
googletrends_urlsnippet_end <- "&geo=ID&date=now%201-d#RELATED_QUERIES"
googlenews_url_wksheet4 <- "https://news.google.com/news/?ned=id_id&gl=ID&hl=id"

Etalase_news_topiclist <- Etalase_news %>% 
  gs_read_cellfeed(ws = '[RAW] OTHER', range = "A40:A42") %>%
  select(value)

worksheet2_L6_googletrends <- L6_googletrends %>% 
  gs_read_cellfeed(ws = '2', range = "A1:E500") %>%
  select(col, row, value) %>%
  spread(col, value) %>%
  select(ALL = "1", NEWS = "2", ENTERTAINMENT = "3",
         LIFESTYLE = "4", SPORT = "5") %>%
  filter(!((ALL=="ALL" & NEWS=="NEWS" & ENTERTAINMENT == "ENTERTAINMENT" 
            & LIFESTYLE == "LIFESTYLE" & SPORT == "SPORT")))

Etalase_news_topiclist_tbl <- do.call(rbind, str_split(Etalase_news_topiclist$value, '\n')) %>%
  t()
colnames(Etalase_news_topiclist_tbl)<- c("topics")
Etalase_news_topiclist_tbl <- as.data.frame(Etalase_news_topiclist_tbl)
Etalase_news_topiclist_tbl <- Etalase_news_topiclist_tbl %>%
  arrange(topics) %>%
  filter(grepl("[a-zA-Z0-9]", topics, ignore.case = TRUE))

Etalase_news_topiclist_tbl_trim <- as.data.frame(str_trim(Etalase_news_topiclist_tbl$topics))
colnames(Etalase_news_topiclist_tbl_trim)<- c("topics")
Etalase_news_topiclist_tbl_trim <- Etalase_news_topiclist_tbl_trim %>%
  # filter the first 19 topics
  filter(grepl('^1.|^2\\.|^3.|^4.|^5.|^6.|^7.|^8.|^9.', topics)) %>% 
  mutate(topics = str_replace(topics, '. ', ':')) %>%
  separate(topics, c('key', 'topics'), ":", extra= "merge") %>%
  select(topics)

# get data from google sheets until xpath method works
worksheet4_L6_googletrends <- L6_googletrends %>% 
  gs_read_cellfeed(ws = '4', range = "A4:A11") %>%
  select(col, row, value) %>%
  spread(col, value)

# https://stackoverflow.com/questions/6442430/xpath-to-get-node-containing-text
# g_news_wksheet4_url <- "https://news.google.com/news/?ned=id_id&gl=ID&hl=id"
# g_news_wksheet4_url1 <- read_html(g_news_wksheet4_url, encoding = "Windows-1252")
# googlenews_url1_extract <- xml_text(xml_find_all(g_news_wksheet4_url1, "//div/div/div/div/div/div/span/span"))

a <- as.data.frame(salary_links)

rbind

Etalase_news_topiclist_tbl_trim$lowertopics <- tolower(Etalase_news_topiclist_tbl_trim$topics)
Etalase_news_topiclist_tbl_trim <- Etalase_news_topiclist_tbl_trim %>%
  mutate(lowertopics = str_replace_all(lowertopics, ' ', '-')) %>%
  mutate(tag_url = paste0(liputan6_tag, lowertopics)) %>%
  mutate(googlenews_url = paste0(googlenews_urlsnippet_start,
                                 lowertopics,
                                 googlenews_urlsnippet_end),
         googletrends_url = paste0(googletrends_urlsnippet_start,
                                   lowertopics,
                                   googletrends_urlsnippet_end))

mydata <- lapply(Etalase_news_topiclist_tbl_trim$googlenews_url, function(x) {
  url1 <- read_html(x)
  googlenews_img_extract <- xml_text(xml_find_all(url1, '//img/@src'))[1]
  return(googlenews_img_extract)
})

mydata_extracted <- as.data.frame(unlist(mydata))
colnames(mydata_extracted)<- c("extracted_images")

Etalase_news_topiclist_tbl_trim_extracted <- cbind(Etalase_news_topiclist_tbl_trim,
                                                   mydata_extracted)

Etalase_news_topiclist_tbl_imagecheck <-  Etalase_news_topiclist_tbl_trim_extracted %>% 
  mutate(imagecheck = case_when(!is.na(extracted_images) ~ as.character(extracted_images),
                                is.na(extracted_images) ~ "https://scontent.fcgk12-1.fna.fbcdn.net/v/t1.0-9/27750288_10155438543847712_8698226800673513911_n.jpg?_nc_eui2=v1%3AAeEnhO1cv42QrSzREQ_thhuQebjgnuOmDGp-K0sq90qrewZ94AbQQYJ-LJPI5vWCDzM1MMb6GlA0OtUT-Vug9ajj-XwDDCyNHAciM8vGnffZhQ&oh=6b1ac1025f915065af80fac0c8fe0681&oe=5B4A8D96")) %>%
  #  mutate(topics_regex = paste0('\\b', topics, '\\b'))
  #Etalase_news_topiclist_tbl_imagecheck$topics_regex = gsub(' ', '\\\\b|\\\\b', Etalase_news_topiclist_tbl_imagecheck$topics_regex)
  mutate(topics_regex = paste0('^', topics))
Etalase_news_topiclist_tbl_imagecheck$topics_regex = gsub(' ', '|^', Etalase_news_topiclist_tbl_imagecheck$topics_regex)



cat_classification_table <- lapply(worksheet2_L6_googletrends[1:5], function(y) {
  lapply(Etalase_news_topiclist_tbl_imagecheck$topics_regex, function(x) {
  ifelse(grep(x, y, ignore.case = TRUE,
                       fixed = FALSE),
                  "TRUE", "FALSE")
}) 
})