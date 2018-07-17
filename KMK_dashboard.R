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
library(googleAnalyticsR)


# get data from Googlesheets - what if owner of google sheet is different ------
my_sheets <- gs_ls()

# authenticate google analytics access
ga_auth()
account_list <- ga_account_list()

etalase_news <-
  gs_key("1dD18QMp3_VCVpFwKYeoG87RAM6_qlAEWM41NKVJgbrA")
L6_googletrends <-
  gs_key("1IkPRSwzq-QfDEwvIqBWhsxy3ye2eY5k7Rv8qn6wt01I")
trend_monitor_indo <-
  gs_key("1mV1X6xt9vf1L7VEsWafkVuWh1sXfDMp9dYNWXpL5HtQ")
liputan6_tag <- "http://liputan6.com/tag/"
googlenews_urlsnippet_start <-
  "https://news.google.com/news/search/section/q/"
googlenews_urlsnippet_end <- "?hl=id&gl=ID&ned=id_id"
googletrends_urlsnippet_start <-
  "https://trends.google.com/trends/explore?q="
googletrends_urlsnippet_end <-
  "&geo=ID&date=now%201-d#RELATED_QUERIES"
googlenews_url_wksheet4 <-
  "https://news.google.com/news/?ned=id_id&gl=ID&hl=id"
trends_monitoring_dash <-
  gs_key("1mV1X6xt9vf1L7VEsWafkVuWh1sXfDMp9dYNWXpL5HtQ")

view_id <-
  account_list$viewId[account_list$viewName == "1. All Liputan6 Site Data"]

# get list of topics from trends monitoring dashboard ----------------------
etalase_news_topiclist <- trends_monitoring_dash %>%
  gs_read_cellfeed(ws = "Sheet1", range = "B7:B7") %>%
  select(value)

# get list of keywords to classify urls against ----------------------------
worksheet2_L6_googletrends <- L6_googletrends %>%
  gs_read_cellfeed(ws = "2", range = "A1:E500") %>%
  select(col, row, value) %>%
  spread(col, value) %>%
  select(
    ALL = "1",
    NEWS = "2",
    ENTERTAINMENT = "3",
    LIFESTYLE = "4",
    SPORT = "5"
  ) %>%
  filter(!(
    (
      ALL == "ALL" & NEWS == "NEWS" & ENTERTAINMENT == "ENTERTAINMENT"
      & LIFESTYLE == "LIFESTYLE" & SPORT == "SPORT"
    )
  ))

# wrangle topic list into dataframe format ----------------------------------
etalase_news_topiclist_tbl <-
  do.call(rbind, str_split(etalase_news_topiclist$value, "\n")) %>%
  t()
colnames(etalase_news_topiclist_tbl) <- c("topics")
etalase_news_topiclist_tbl <-
  as.data.frame(etalase_news_topiclist_tbl)
etalase_news_topiclist_tbl <- etalase_news_topiclist_tbl %>%
  arrange(topics) %>%
  filter(grepl("[a-zA-Z0-9]", topics, ignore.case = TRUE))

etalase_news_topiclist_tbltrim <-
  as.data.frame(str_trim(etalase_news_topiclist_tbl$topics))
colnames(etalase_news_topiclist_tbltrim) <- c("topics")
etalase_news_topiclist_tbltrim <-
  etalase_news_topiclist_tbltrim %>%
  # filter the first 19 topics
  filter(grepl("^1.|^2\\.|^3.|^4.|^5.|^6.|^7.|^8.|^9.", topics)) %>%
  mutate(topics = str_replace(topics, ". ", ":")) %>%
  separate(topics, c("key", "topics"), ":", extra = "merge") %>%
  select(topics)

# get data from google sheets until xpath method works ----------------------
worksheet4_L6_googletrends <- L6_googletrends %>%
  gs_read_cellfeed(ws = "4", range = "A4:A9") %>%
  select(col, row, value) %>%
  spread(col, value) %>%
  select(topics = "1")


# add google news and trends urls to topic list ------------------------------
etalase_news_topiclist_tbltrim <-
  rbind(etalase_news_topiclist_tbltrim,
        worksheet4_L6_googletrends)

etalase_news_topiclist_tbltrim$lowertopics <-
  tolower(etalase_news_topiclist_tbltrim$topics)
etalase_news_topiclist_tbltrim <-
  etalase_news_topiclist_tbltrim %>%
  mutate(lowertopics = str_replace_all(lowertopics, " ", "-")) %>%
  mutate(slashtopics = str_replace_all(lowertopics, "-", "%20")) %>%
  mutate(tag_url = paste0(liputan6_tag, lowertopics)) %>%
  mutate(
    competitor_googlenews_url = paste0(
      googlenews_urlsnippet_start,
      slashtopics,
      googlenews_urlsnippet_end
    ),
    googletrends_url = paste0(
      googletrends_urlsnippet_start,
      slashtopics,
      googletrends_urlsnippet_end
    )
  )

# get image urls from google news urls -------------------------------------------
mydata <-
  lapply(etalase_news_topiclist_tbltrim$competitor_googlenews_url, function(x) {
    url1 <- read_html(x)
    googlenews_img_extract <-
      xml_text(xml_find_all(url1, "//img/@src"))[1]
    return(googlenews_img_extract)
  })

mydata_extracted <- as.data.frame(unlist(mydata))
colnames(mydata_extracted) <- c("extracted_images")

etalase_news_topiclist_tbltrim_extracted <-
  cbind(etalase_news_topiclist_tbltrim,
        mydata_extracted)

# add regex to extract topics from url --------------------------------------
etalase_news_topiclist_tbl_imagecheck <-
  etalase_news_topiclist_tbltrim_extracted %>%
  mutate(
    imagecheck = case_when(
      !is.na(extracted_images) ~ as.character(extracted_images),
      is.na(extracted_images) ~ "https://scontent.fcgk12-1.fna.fbcdn.net/v/t1.0-9/27750288_10155438543847712_8698226800673513911_n.jpg?_nc_eui2=v1%3AAeEnhO1cv42QrSzREQ_thhuQebjgnuOmDGp-K0sq90qrewZ94AbQQYJ-LJPI5vWCDzM1MMb6GlA0OtUT-Vug9ajj-XwDDCyNHAciM8vGnffZhQ&oh=6b1ac1025f915065af80fac0c8fe0681&oe=5B4A8D96"
    )
  ) %>%
  mutate(topics_regex = paste0("^", topics))
etalase_news_topiclist_tbl_imagecheck$topics_regex <-
  gsub(" ", "|^",
       etalase_news_topiclist_tbl_imagecheck$topics_regex)


# classify topics against list of keywords ------------------------------------
cat_classification_table <-
  lapply(worksheet2_L6_googletrends[1:5], function(y) {
    lapply(etalase_news_topiclist_tbl_imagecheck$topics_regex, function(x) {
      ifelse(grep(x, y, ignore.case = TRUE,
                  fixed = FALSE),
             "TRUE", "FALSE")
    })
  })

cat_table <- as_data_frame(cat_classification_table)
cat_table <- cat_table %>%
  mutate(
    status = case_when(
      grepl("true|TRUE", ALL) ~ "ALL",
      grepl("true|TRUE", NEWS) ~ "NEWS",
      grepl("true|TRUE", ENTERTAINMENT) ~ "ENTERTAINMENT",
      grepl("true|TRUE", LIFESTYLE) ~ "LIFESTYLE",
      grepl("true|TRUE", SPORT) ~ "SPORT",
      TRUE ~ "NEW ENTRY"
    ),
    ALL = case_when(ALL != "TRUE" ~ "FALSE",
                    TRUE ~ "TRUE"),
    NEWS = case_when(NEWS != "TRUE" ~ "FALSE",
                     TRUE ~ "TRUE"),
    ENTERTAINMENT = case_when(ENTERTAINMENT != "TRUE" ~ "FALSE",
                              TRUE ~ "TRUE"),
    LIFESTYLE = case_when(LIFESTYLE != "TRUE" ~ "FALSE",
                          TRUE ~ "TRUE"),
    SPORT = case_when(SPORT != "TRUE" ~ "FALSE",
                      TRUE ~ "TRUE")
  )


etalase_news_topic_statuscheck <-
  cbind(etalase_news_topiclist_tbl_imagecheck,
        cat_table)

# add logic for conditional image displays via URL of each image --------------
etalase_news_topic_statuscheck <-
  etalase_news_topic_statuscheck %>%
  mutate(
    image_display = case_when(
      imagecheck == "https://lh3.googleusercontent.com/JDFOyo903E9WGstK0YhI2ZFOKR3h4qDxBngX5M8XJVBZFKzOBoxLmk3OVlgNw9SOE-HfkNgb=w48" ~ "https://www.airportrampequipment.com/8903167/assets/images/product/image-250x250.gif",
      TRUE ~ imagecheck
    )
  )

# regex for info-2 - scrape data from tag url to find article titles ----------------------------
readUrl <- function(url) {
  tryCatch(
    xml_text(
      xml_find_all(
        read_html(url),
        '//*[@id="main"]/div/div/div[1]/div/p/text()'
      )
    )[1]
    ,
    error = function(e) {
      NA
    }
  )
}

mydata_info2 <-
  lapply(etalase_news_topic_statuscheck$tag_url, readUrl)
mydata_extracted_info2 <- as.data.frame(unlist(mydata_info2))
mydata_extracted_info2 <- mydata_extracted_info2 %>%
  select(info_2 = `unlist(mydata_info2)`)

mydata_extracted_info2$info_2 <-
  as.character(mydata_extracted_info2$info_2)

etalase_news_topic_statuscheck <-
  cbind(etalase_news_topic_statuscheck,
        mydata_extracted_info2)


# create table for popular topic rankings (desktop/mobile) -------------------
url_detik_berita <- read_html("http://news.detik.com/berita")
detik_berita_extract <-
  unlist(xml_text(xml_find_all(
    url_detik_berita, "//span[@class='normal']"
  )))
detik_berita_extract <- as.data.frame(detik_berita_extract)
detik_berita_href_extract <-
  as.data.frame(xml_text(
    xml_find_all(
      url_detik_berita,
      "//*[@id='box-pop']/ul/li/article/a/@href"
    )
  ))
detik_berita_fulltable <-
  cbind(as.data.frame(detik_berita_extract[1:3, ])
        ,
        as.data.frame(detik_berita_href_extract[1:3, ]))

colnames(detik_berita_fulltable)[1:2] <- c("title", "link")

# create table for popular topic rankings (line) -----------------------------
line_url_detik_berita <-
  read_html("http://today.line.me/ID/pc/main/100271")
line_detik_berita_extract <-
  unlist(xml_text(
    xml_find_all(line_url_detik_berita, "//a/@href [contains(., 'article')]")
  ))
line_detik_berita_extract <-
  as.data.frame(line_detik_berita_extract)
line_detikberitaextract_tbl <- line_detik_berita_extract %>%
  mutate(clean1 = gsub("/id/pc/article/", "", line_detik_berita_extract)) %>%
  mutate(clean2 = gsub("\\+", " ", line_detik_berita_extract)) %>%
  separate(clean2, c("clean2_first", "clean2_second"), "-", extra = "merge") %>%
  mutate(clean3 = gsub("https://today.line.me/id/pc/article/",
                       "", clean2_first)) %>%
  select(title = "clean3", link = "line_detik_berita_extract") %>%
  filter(row_number() <= 2)

# create combined table for topic rankings -----------------------------------
popular_topics_fulltable <- rbind(detik_berita_fulltable,
                                  line_detikberitaextract_tbl)


# get tags and organic traffic data from google analytics --------------------
startdate <- Sys.Date() - 7
enddate <- Sys.Date()

segment_for_allusers <- "gaid::-1"
seg_allusers <-
  segment_ga4("All Users", segment_id = segment_for_allusers)

df_organic <-
  dim_filter(dimension = "channelGrouping",
             operator = "REGEXP",
             expressions = "Organic")
filter_organic <- filter_clause_ga4(list(df_organic))


ga_data_organic_keywords <-
  google_analytics(
    view_id,
    #=This is a (dynamic) ViewID parameter
    date_range = c(startdate, enddate),
    metrics = c("sessions"),
    dimensions = c("date", "dimension16", "channelGrouping"),
    dim_filters = filter_organic,
    segments = c(seg_allusers),
    anti_sample = TRUE,
    max = -1
  )

# get the sum of sessions for each organic keyword
keyword_sumsessions <-
  lapply(etalase_news_topic_statuscheck$topics, function(x) {
    keyword1 <- ga_data_organic_keywords %>%
      filter(grepl(x, dimension16, ignore.case = TRUE)) %>%
      group_by(date) %>%
      summarise(sessions = sum(sessions)) %>%
      mutate(keyword = x)
  })

keyword_sumsessions <- bind_rows(keyword_sumsessions)
keyword_sumsessions <- keyword_sumsessions %>%
  filter(grepl("[a-zA-Z0-9]", keyword, ignore.case = TRUE)) %>%
  group_by(date, keyword) %>%
  summarise(sessions = sum(sessions)) %>%
  spread(date, sessions) %>%
  rename_at(.vars = vars(contains("-")),
            .funs = funs(gsub("-", "_", .))) %>%
  rename_at(vars(-keyword), function(x)
    paste0("_", x)) %>% 
  mutate_at(vars(-keyword), funs(replace(., is.na(.), 0)))

# Create table for last updated timings --------------------------------------
readarticlelink <- function(hyperlink) {
  tryCatch(
    read_html(hyperlink) %>%
      html_node(".articles--iridescent-list--text-item__title-link") %>%
      html_attr("href")
  )
}

etalase_news_topic_timings <-
  etalase_news_topic_statuscheck %>%
  select(topics, tag_url, info_2)

etalase_news_topictimingfilter <-
  etalase_news_topic_timings %>%
  filter(!is.na(info_2))

link_timing_info2 <-
  lapply(etalase_news_topictimingfilter$tag_url,
         readarticlelink)
link_timing_extracted_info2 <-
  as.data.frame(unlist(link_timing_info2))
link_timing_extracted_info2 <- link_timing_extracted_info2 %>%
  select(firstarticlelink = `unlist(link_timing_info2)`) %>%
  mutate(firstarticlelink = as.character(firstarticlelink))

etalase_news_topictimingfilter <-
  cbind(etalase_news_topictimingfilter,
        link_timing_extracted_info2)

etalase_news_topictimingslink <-
  etalase_news_topic_timings %>%
  left_join(etalase_news_topictimingfilter,
            by = c("topics", "tag_url", "info_2"))

getarticletiming <- function(article_url) {
  tryCatch(
    read_html(article_url) %>%
      html_node(".read-page--header--author__datetime") %>%
      html_text(),
    error = function(e) {
      NA
    }
  )
}

article_timing_info2 <-
  lapply(etalase_news_topictimingslink$firstarticlelink,
         getarticletiming)
article_timing_info2 <- as.data.frame(unlist(article_timing_info2))
article_timing_info2 <- article_timing_info2 %>%
  select(firstarticletiming = `unlist(article_timing_info2)`) %>%
  mutate(firstarticletiming = as.character(firstarticletiming))

etalase_news_topiclist_tbl_articletimings <-
  cbind(etalase_news_topictimingslink,
        article_timing_info2)

etalase_news_topiclist_tbl_articletimings <-
  etalase_news_topiclist_tbl_articletimings %>%
  separate(firstarticletiming,
           c("ArticleDate", "ArticleTime"),
           ",",
           extra = "merge") %>%
  select(-tag_url, -info_2)

# process urls from articletimings table for use with Google Analytics tag CD request-----
trimarticlelink <- etalase_news_topiclist_tbl_articletimings %>%
  select(firstarticlelink) %>%
  filter(!is.na(firstarticlelink)) %>%
  mutate(
    firstarticlelink = gsub(
      "https://www.liputan6.com|http://www.liputan6.com",
      "",
      firstarticlelink
    )
  )

trimarticlelink_regex <-
  paste(unlist(trimarticlelink), collapse = "|")


# get tags from custom dimenions in Google Analytics and append to dataframe -----
df_page <- dim_filter(dimension = "pagePath",
                      operator = "REGEXP",
                      expressions = trimarticlelink_regex)
filter_page <- filter_clause_ga4(list(df_page))


ga_data_keyword_tag <-
  google_analytics(
    view_id,
    #=This is a (dynamic) ViewID parameter
    date_range = c(startdate, enddate),
    metrics = c("sessions"),
    dimensions = c("pagePath", "dimension16"),
    dim_filters = filter_page,
    segments = c(seg_allusers),
    anti_sample = TRUE,
    max = -1
  )

ga_data_keyword_tag <- ga_data_keyword_tag %>%
  mutate(pagePath = paste0("https://www.liputan6.com", pagePath)) %>%
  select(-segment, -sessions)

etalase_news_newstimetags <-
  etalase_news_topiclist_tbl_articletimings %>%
  left_join(ga_data_keyword_tag, by = c("firstarticlelink" = "pagePath"))

# upload to Bigquery ---------------------------------------------------------
# Variables for the BigQuery upload portion
destinationproject <- "analisis-production"
destinationdataset <- "sparkline"
contentreportname <- "keywords_dashboard_content"
rankingsreportname <- "popular_topics_fulltable"
articleupdatedreportname <- "article_updated_timings"
organictrafficreportname <- "keyword_organic_traffic"
lastupdatedreportname <- "last_updated_timings"


# Check if the table exists, if table exists, then delete the table ----------
tryCatch(
  bq_table_delete(
    bq_table(destinationproject, destinationdataset, contentreportname)
  ),
  error = function(e) {
    print(paste0(contentreportname, " not available for deletion"))
  }
)

tryCatch(
  bq_table_delete(
    bq_table(destinationproject, destinationdataset, rankingsreportname)
  ),
  error = function(e) {
    print(paste0(rankingsreportname, " not available for deletion"))
  }
)

tryCatch(
  bq_table_delete(
    bq_table(
      destinationproject,
      destinationdataset,
      organictrafficreportname
    )
  ),
  error = function(e) {
    print(paste0(organictrafficreportname, " not available for deletion"))
  }
)

tryCatch(
  bq_table_delete(
    bq_table(
      destinationproject,
      destinationdataset,
      articleupdatedreportname
    )
  ),
  error = function(e) {
    print(paste0(articleupdatedreportname, " not available for deletion"))
  }
)

# Upload the table into big query --------------------------------------------
tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    contentreportname,
    etalase_news_topic_statuscheck
  ),
  error = function(e) {
    print(paste0(contentreportname, " failed to upload"))
  }
)

tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    rankingsreportname,
    popular_topics_fulltable
  ),
  error = function(e) {
    print(paste0(rankingsreportname, " failed to upload"))
  }
)

tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    organictrafficreportname,
    keyword_sumsessions
  ),
  error = function(e) {
    print(paste0(organictrafficreportname, " failed to upload"))
  }
)

tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    articleupdatedreportname,
    etalase_news_newstimetags
  ),
  error = function(e) {
    print(paste0(articleupdatedreportname, " failed to upload"))
  }
)


# get last updated date and time in a dataframe and upload as a table into bigquery ------
ms_to_date <-  function(ms, t0 = "1970-01-01", timezone) {
  sec <- ms / 1000
  as.POSIXct(sec, origin = t0, tz = timezone)
}

Sys.sleep(10)


updated_times <-  data.frame(
  table_name = c(
    contentreportname,
    rankingsreportname,
    articleupdatedreportname,
    organictrafficreportname
  ),
  last_updated_timestamp =
    c(
      ms_to_date(as.numeric(bq_table_meta(
        bq_table(destinationproject,
                 destinationdataset,
                 contentreportname)
      )[["lastModifiedTime"]]),
      timezone =
        "Asia/Singapore"),
      ms_to_date(as.numeric(bq_table_meta(
        bq_table(destinationproject,
                 destinationdataset,
                 rankingsreportname)
      )[["lastModifiedTime"]]),
      timezone =
        "Asia/Singapore"),
      ms_to_date(as.numeric(bq_table_meta(
        bq_table(
          destinationproject,
          destinationdataset,
          articleupdatedreportname
        )
      )[["lastModifiedTime"]]),
      timezone = "Asia/Singapore"),
      ms_to_date(as.numeric(bq_table_meta(
        bq_table(
          destinationproject,
          destinationdataset,
          organictrafficreportname
        )
      )[["lastModifiedTime"]]),
      timezone = "Asia/Singapore")
    )
)



updated_times <- updated_times %>%
  mutate(last_updated_datetime = format.Date(last_updated_timestamp,
                                             "%d/%m/%Y %r")) %>%
  mutate(
    last_updated_date = format.Date(last_updated_timestamp,
                                    "%d/%m/%Y"),
    last_updated_time = format.Date(last_updated_timestamp,
                                    "%r")
  )

tryCatch(
  bq_table_delete(
    bq_table(
      destinationproject,
      destinationdataset,
      lastupdatedreportname
    )
  ),
  error = function(e) {
    print(paste0(lastupdatedreportname, " not available for deletion"))
  }
)

tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    lastupdatedreportname,
    updated_times
  ),
  error = function(e) {
    print(paste0(lastupdatedreportname, " failed to upload"))
  }
)
