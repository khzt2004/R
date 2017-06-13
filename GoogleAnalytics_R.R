## setup
library(googleAnalyticsR)

## This should send you to your browser to authenticate your email.
## Authenticate with an email that has access to the Google Analytics View you want to use.
ga_auth()

## get your accounts
account_list <- ga_account_list()

## pick a profile with data to query
ga_id <- account_list[92,'viewId']

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
