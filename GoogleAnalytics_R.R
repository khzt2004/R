library(googleAnalyticsR)
library(future.apply)
library(tidyverse)

## setup multisession R for your parallel data fetches 
plan(multisession)

# login as new_user = TRUE if switching accounts. Otherwise do not set new_user = true
ga_auth()
# ga_auth(new_user = TRUE)

Sys.setenv(GA_AUTH_FILE = "C:/Users/User/Documents/.httr-oauth")
# need alternative for mac

account_list <- ga_account_list()

## the ViewIds to fetch all at once
gaids <- c(account_list[2122,'viewId'], account_list[2125,'viewId'], account_list[2128,'viewId'])

# selecting segments
my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

my_fetch <- function(x) {
  google_analytics(x, 
                   date_range = c("2018-01-01","yesterday"), 
                   metrics = c("sessions", "transactions", "transactionRevenue"), 
                   dimensions = c("yearMonth", "deviceCategory", "userType"),
                   segments = c(seg_allUsers),
                   anti_sample = TRUE,
                   max = -1)
}

## makes 3 API calls at once
all_data <- future_lapply(gaids, my_fetch)
df1 <- data.frame(all_data[1])
df1 <- df1 %>% mutate(viewID = account_list[2122,'viewName'])
df2 <- data.frame(all_data[2])
df2 <- df2 %>% mutate(viewID = account_list[2125,'viewName'])
df3 <- data.frame(all_data[3])
df3 <- df3 %>% mutate(viewID = account_list[2128,'viewName'])
df_all <- rbind(df1,df2,df3)

# query multiple segments
segment_for_newusers <- "gaid::-2"
seg_newusers <- segment_ga4("new Users", segment_id = segment_for_newusers)
segment_for_returnusers <- "gaid::-3"
seg_returnusers <- segment_ga4("return Users", segment_id = segment_for_returnusers)
segment_for_paidusers <- "gaid::-4"
seg_paidusers <- segment_ga4("paid Users", segment_id = segment_for_paidusers)
segment_for_organicusers <- "gaid::-5"
seg_organicusers <- segment_ga4("organic Users", segment_id = segment_for_organicusers)
segment_for_searchusers <- "gaid::-6"
seg_searchusers <- segment_ga4("search Users", segment_id = segment_for_searchusers)
segment_for_directusers <- "gaid::-7"
seg_directusers <- segment_ga4("direct Users", segment_id = segment_for_directusers)
segment_for_referralusers <- "gaid::-8"
seg_referralusers <- segment_ga4("referral Users", segment_id = segment_for_referralusers)
segment_for_convusers <- "gaid::-9"
seg_convusers <- segment_ga4("conv Users", segment_id = segment_for_convusers)
segment_for_transactionusers <- "gaid::-10"
seg_transactionusers <- segment_ga4("transaction Users", segment_id = segment_for_transactionusers)
segment_for_mobiletabletusers <- "gaid::-11"
seg_mobiletabletusers <- segment_ga4("mobiletablet Users", segment_id = segment_for_mobiletabletusers)

segmentlist <- c(seg_allUsers,
                 seg_newusers,
                 seg_returnusers,
                 seg_paidusers,
                 seg_organicusers,
                 seg_searchusers,
                 seg_directusers,
                 seg_referralusers,
                 seg_convusers) 

segmentlisting <- c(segmentlist[1:4], segmentlist[5:8])

ga_data_final_segment <- data.frame()

for (i in segmentlisting) {
  ga_data_segment_eg <- 
    google_analytics(view_id, #=This is a (dynamic) ViewID parameter
                     date_range = c(startDate2, endDate), 
                     metrics = c("sessions", "transactions", "transactionRevenue"), 
                     dimensions = c("yearMonth", "deviceCategory", "userType"),
                     segments = i,
                     anti_sample = TRUE,
                     max = -1)
  
  ga_data_final_segment <- rbind(ga_data_final_segment, ga_data_segment_eg)
}


## pick a profile with data to query
ga_id <- account_list[1123,'viewId']

## get a list of what metrics and dimensions you can use
ga_auth()
meta <- google_analytics_meta()

googleAnalyticsR:::gadget_GASegment()

## make two segment elements
se <- segment_element("sessions", 
                      operator = "GREATER_THAN", 
                      type = "METRIC", 
                      comparisonValue = 3, 
                      scope = "USER")

se3 <- segment_element("medium", 
                       operator = "REGEXP", 
                       type = "DIMENSION", 
                       expressions = "^(email|referral)$",
                       scope = "SESSION")

sv_simple <- segment_vector_simple(list(list(segment_ga_google5sec)))
seg_defined <- segment_define(sv_simple)
segment4 <- segment_ga4("simple", user_segment = seg_defined)


# segments: semicolon is "AND", a comma is "OR"

segment_def_medium <- "sessions::condition::ga:medium=~^(email|referral)$"
seg_obj_medium <- segment_ga4("test", segment_id = segment_def_medium)

segment_def_google30sec <- "sessions::condition::ga:source=~^(google)$;ga:timeOnPage>30"
seg_obj_google30sec <- segment_ga4("test", segment_id = segment_def_google30sec)

segment_def_morethan3sessions <- "sessions::condition::ga:sessions>3"
seg_obj_morethan3sessions <- segment_ga4("test", segment_id = segment_def_morethan3sessions)

segment_def_orgtraffic_w_conversions <- "sessions::condition::ga:medium=~^(organic)$;ga:goal11Completions>0"
seg_obj_orgtraffic_w_conversions <- segment_ga4("test", segment_id = segment_def_orgtraffic_w_conversions)

segment_seq_example <- google_analytics_4(ga_id, 
                                          date_range = c("2017-01-01","2017-03-01"), 
                                          dimensions = c('source','country'), 
                                          segments = seg_obj_orgtraffic_w_conversions,
                                          metrics = c('sessions','bounceRate', 'timeOnPage', 'goal11Completions')
)

segment_seq_example

segment_def_mktids <- "sessions::condition::ga:dimension2=@mktid"
seg_obj_mktids <- segment_ga4("test", segment_id = segment_def_mktids)

segment_seq_mktids <- google_analytics_4(ga_id, 
                                         date_range = c("2017-01-01","2017-03-01"), 
                                         dimensions = c('source','dimension2'), 
                                         segments = seg_obj_mktids,
                                         metrics = c('sessions','bounceRate', 'timeOnPage', 'goal11Completions')
)

segment_seq_mktids


google_analytics_4(ga_id, #=This is a (dynamic) ViewID parameter
                   date_range = c("2018-01-01","2018-01-30"), 
                   metrics = c("sessions", "users"), 
                   dimensions = c("deviceCategory", "sourceMedium", "date"),
                   #anti_sample = TRUE,
                   max = -1,
                   useResourceQuotas = TRUE)


