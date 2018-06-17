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
trends_monitoring_dash <- gs_key("1mV1X6xt9vf1L7VEsWafkVuWh1sXfDMp9dYNWXpL5HtQ")


Etalase_news_topiclist <- trends_monitoring_dash %>% 
  gs_read_cellfeed(ws = 'Sheet1', range = "B7:B7") %>%
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
  gs_read_cellfeed(ws = '4', range = "A4:A9") %>%
  select(col, row, value) %>%
  spread(col, value) %>%
  select(topics ="1")

# https://stackoverflow.com/questions/6442430/xpath-to-get-node-containing-text
# g_news_wksheet4_url <- "https://news.google.com/news/?ned=id_id&gl=ID&hl=id"
# g_news_wksheet4_url1 <- read_html(g_news_wksheet4_url, encoding = "Windows-1252")
# googlenews_url1_extract <- xml_text(xml_find_all(g_news_wksheet4_url1, "//div/div/div/div/div/div/span/span"))

Etalase_news_topiclist_tbl_trim <- rbind(Etalase_news_topiclist_tbl_trim, 
                                                worksheet4_L6_googletrends)

Etalase_news_topiclist_tbl_trim$lowertopics <- tolower(Etalase_news_topiclist_tbl_trim$topics)
Etalase_news_topiclist_tbl_trim <- Etalase_news_topiclist_tbl_trim %>%
  mutate(lowertopics = str_replace_all(lowertopics, ' ', '-')) %>%
  mutate(tag_url = paste0(liputan6_tag, lowertopics)) %>%
  mutate(competitor_googlenews_url = paste0(googlenews_urlsnippet_start,
                                 lowertopics,
                                 googlenews_urlsnippet_end),
         googletrends_url = paste0(googletrends_urlsnippet_start,
                                   lowertopics,
                                   googletrends_urlsnippet_end))

mydata <- lapply(Etalase_news_topiclist_tbl_trim$competitor_googlenews_url, function(x) {
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

cat_table <- as_data_frame(cat_classification_table) 
cat_table <- cat_table %>%
  mutate(status = case_when(grepl('true|TRUE', ALL) ~ 'ALL',
                            grepl('true|TRUE', NEWS) ~ 'NEWS',
                            grepl('true|TRUE', ENTERTAINMENT) ~ 'ENTERTAINMENT',
                            grepl('true|TRUE', LIFESTYLE) ~ 'LIFESTYLE',
                            grepl('true|TRUE', SPORT) ~ 'SPORT',
                            TRUE ~ 'NEW ENTRY'),
         ALL = case_when(ALL != 'TRUE' ~ 'FALSE',
                         TRUE ~ 'TRUE'),
         NEWS = case_when(NEWS != 'TRUE' ~ 'FALSE',
                          TRUE ~ 'TRUE'),
         ENTERTAINMENT = case_when(ENTERTAINMENT != 'TRUE' ~ 'FALSE',
                                   TRUE ~ 'TRUE'),
         LIFESTYLE = case_when(LIFESTYLE != 'TRUE' ~ 'FALSE',
                               TRUE ~ 'TRUE'),
         SPORT = case_when(SPORT != 'TRUE' ~ 'FALSE',
                           TRUE ~ 'TRUE'))

  
Etalase_news_topiclist_tbl_statuscheck <- cbind(Etalase_news_topiclist_tbl_imagecheck,
                                                cat_table)

Etalase_news_topiclist_tbl_statuscheck <- Etalase_news_topiclist_tbl_statuscheck %>%
  mutate(image_display = case_when(imagecheck == 'https://lh3.googleusercontent.com/JDFOyo903E9WGstK0YhI2ZFOKR3h4qDxBngX5M8XJVBZFKzOBoxLmk3OVlgNw9SOE-HfkNgb=w48' ~ 'https://www.airportrampequipment.com/8903167/assets/images/product/image-250x250.gif',
                                   TRUE ~ imagecheck))

# regex for info-2
readUrl <- function(url) {
  tryCatch(
  xml_text(xml_find_all(read_html(url), '//*[@id="main"]/div/div/div[1]/div/p/text()'))[1]
  , error = function(e){NA}
  )
}

# ***** 15 June: need to force into character*****
mydata_info2 <- lapply(Etalase_news_topiclist_tbl_statuscheck$tag_url, readUrl)
mydata_extracted_info2 <- as.data.frame(unlist(mydata_info2))
mydata_extracted_info2 <- mydata_extracted_info2 %>% 
  select(info_2 = `unlist(mydata_info2)`)

mydata_extracted_info2$info_2 <- as.character(mydata_extracted_info2$info_2)

Etalase_news_topiclist_tbl_statuscheck <- cbind(Etalase_news_topiclist_tbl_statuscheck,
                                                mydata_extracted_info2)


# create table for popular topic rankings
# ***** 15 June: check if need to scrape from original source or reference spreadsheets*****

url_detik_berita <- read_html("http://news.detik.com/berita")
detik_berita_extract <- as.data.frame(xml_text(xml_find_all(url_detik_berita, "//span[@class='normal']")))
detik_berita_href_extract <- as.data.frame(xml_text(xml_find_all(url_detik_berita, "//*[@id='box-pop']/ul/li/article/a/@href")))


Topic_ranking_top3 <- Etalase_news %>% 
  gs_read_cellfeed(ws = '[RAW] DETIK', range = "A2:D4") %>%
  select(value)

Topic_ranking_top3_tbl <- Topic_ranking_top3 %>% 
  mutate(Type = case_when(grepl('https', value) ~ 'Link',
                          TRUE ~ 'Title'),
         number = 1:6) %>%
  spread(Type, value) %>%
  select(Title, Link) %>%
  mutate(Link = lead(Link)) %>%
  filter(!is.na(Title))





# upload to Bigquery
# Variables for the BigQuery upload portion
destinationProject <- "analisis-production"
destinationDataset <- "sparkline"
contentreportName <- 'keywords_dashboard_content'
rankingsreportName <- 'keywords_dasboard_ranking'


# Check if the table exists, if table exists, then delete the table
tryCatch(bq_table_delete(bq_table(destinationProject, destinationDataset, contentreportName)),
         error = function(e){
           print(paste0(contentreportName, " not available for deletion"))
         })

tryCatch(bq_table_delete(bq_table(destinationProject, destinationDataset, rankingsreportName)),
         error = function(e){
           print(paste0(rankingsreportName, " not available for deletion"))
         })

# Upload the table into big query
tryCatch(insert_upload_job(destinationProject, destinationDataset, contentreportName, Etalase_news_topiclist_tbl_statuscheck[18]),
         error = function(e){
           print(paste0(contentreportName, " failed to upload"))
         })

tryCatch(insert_upload_job(destinationProject, destinationDataset, rankingsreportName, XXX),
         error = function(e){
           print(paste0(rankingsreportName, " failed to upload"))
         })

