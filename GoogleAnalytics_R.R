## setup
library(googleAnalyticsR)

## This should send you to your browser to authenticate your email.
## Authenticate with an email that has access to the Google Analytics View you want to use.
ga_auth()

## get your accounts
account_list <- ga_account_list()

## pick a profile with data to query
ga_id <- account_list[106,'viewId']

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

sv_simple <- segment_vector_simple(list(list(se3)))
seg_defined <- segment_define(sv_simple)
segment4 <- segment_ga4("simple", user_segment = seg_defined)

segment_def_for_call <- "sessions::condition::ga:medium=~^(email|referral)$"
seg_obj <- segment_ga4("test", segment_id = segment_def_for_call)

segment_seq_example <- google_analytics_4(ga_id, 
                                          date_range = c("2017-01-01","2017-03-01"), 
                                          dimensions = c('medium','country'), 
                                          segments = seg_obj,
                                          metrics = c('sessions','bounces')
)

segment_seq_example