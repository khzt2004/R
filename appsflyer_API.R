library(tidyverse)
library(RPostgres)
library(DBI)
library("aws.s3")

library(httr)
library(jsonlite)
library(sqldf)
library(summarytools)
library(lubridate)
library(zoo)
library(Rcpp)
library(redshiftTools)

# install.packages(c('devtools', 'httr', 'aws.s3'))
# install.packages("Rcpp")
# devtools::install_github("r-dbi/DBI")
# devtools::install_github("sicarul/redshiftTools")
# https://github.com/sicarul/redshiftTools

credentials <- read.delim("Redshift_AWS_Credentials.txt", sep= "", header = TRUE)
credentials <- credentials %>% mutate_if(is.factor, as.character)

# connect to database --------------------------------------------------------
# Redshift_AWS_Credentials.txt --> import text file into R as dataframe
con <- dbConnect(RPostgres::Postgres(), 
                 dbname=as.character(credentials[credentials$Details =='dbname',][2]),
                 host=as.character(credentials[credentials$Details =='host',][2]), 
                 port=credentials[credentials$Details =='port',][2],
                 user=as.character(credentials[credentials$Details =='user',][2]), 
                 password=as.character(credentials[credentials$Details =='password',][2]),
                 sslmode='require')

# S3 bucket credentials -------------------------------------------------------------------
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = as.character(credentials[credentials$Details =='AWS_ACCESS_KEY_ID',][2]),
  "AWS_SECRET_ACCESS_KEY" = as.character(credentials[credentials$Details =='AWS_SECRET_ACCESS_KEY',][2]),
  "AWS_DEFAULT_REGION" = as.character(credentials[credentials$Details =='AWS_DEFAULT_REGION',][2])
)

# view schema
# schema <- dbFetch(dbSendQuery(con, "select * from information_schema.tables;"))

# view columns in a table
# colnames <- dbFetch(dbSendQuery(con, "SELECT
#                       *  FROM information_schema.COLUMNS;"))

# Appsflyer - revenue breakdown by week --------------------------------------------
android_ios_id_revenue <- dbFetch(dbSendQuery(con, "select currency, to_date(payment_date, 'YYYY-MM-DD') as date, 
                                              EXTRACT(WEEK FROM to_date(payment_date, 'YYYY-MM-DD')) as week, payment_method,
                                              sum(payments.payment_amount) from evergent.payments where 
                                              payment_status = 'Posted' and 
                                              payment_amount < 10000 and  
                                              to_date(payment_date, 'YYYY-MM-DD') 
                                              between to_date(validity_start_date, 'YYYY-MM-DD') and 
                                              to_date(validity_end_date, 'YYYY-MM-DD') group by currency, date, week, payment_method;"))


# define date range -------------------------------------------------------------
# 5-22 to 6-22 then 6-23 to current date
end_date <- Sys.Date()
start_date <- "2018-06-23"
end_date <- "2018-08-24"

# Apps Download stage -----------------------------------------------------------

# Get Raw Data from Appsflyer API -----------------------------------------------
ios_url  <- paste0("https://hq.appsflyer.com/export/id1116595369/installs_report/v5?api_token=",
                   as.character(credentials[credentials$Details =='Appsflyer_Token',][2]),
                   "&from=",
                   start_date, "&to=", end_date)

android_url  <- paste0("https://hq.appsflyer.com/export/com.fng.foxplus/installs_report/v5?api_token=",
                       as.character(credentials[credentials$Details =='Appsflyer_Token',][2]),
                       "&from=",
                       start_date, "&to=", end_date, "&additional_fields=install_app_store,match_type,contributor1_match_type,contributor2_match_type,contributor3_match_type,device_category")

ios_raw_url <- read_csv(ios_url)
android_raw_url <- read_csv(android_url)
android_raw_url1 <- android_raw_url %>%
  select(-`Install App Store`,
         -`Match Type`,
         -`Contributor 1 Match Type`,
         -`Contributor 2 Match Type`,
         -`Contributor 3 Match Type`,
         -`Device Category`
  )

# merge raw data
ios_android_raw_url <- rbind(ios_raw_url, android_raw_url1)


# Get Daily Data from Appsflyer API ----------------------------------------------
ios_daily_url <- paste0("https://hq.appsflyer.com/export/id1116595369/daily_report/v5?api_token=",
                        as.character(credentials[credentials$Details =='Appsflyer_Token',][2]),
                        "&from=",
                        start_date, "&to=", end_date)

android_daily_url <- paste0("https://hq.appsflyer.com/export/com.fng.foxplus/daily_report/v5?api_token=",
                            as.character(credentials[credentials$Details =='Appsflyer_Token',][2]),
                            "&from=",
                            start_date, "&to=", end_date)

# add column to define which platform the data came from (ios)
ios_daily <- read_csv(ios_daily_url)
ios_daily <- ios_daily %>%
  mutate(platform = "ios")

# add column to define which platform the data came from (android)
android_daily <- read_csv(android_daily_url)
android_daily <- android_daily %>%
  mutate(platform = "android")

# merge daily data
ios_android_daily <- rbind(ios_daily, android_daily)


# Get Geo Data from Appsflyer API ------------------------------------------------
ios_geo_url <- paste0("https://hq.appsflyer.com/export/id1116595369/geo_by_date_report/v5?api_token=",
                      as.character(credentials[credentials$Details =='Appsflyer_Token',][2]),
                      "&from=",
                      start_date, "&to=", end_date)

android_geo_url <- paste0("https://hq.appsflyer.com/export/com.fng.foxplus/geo_by_date_report/v5?api_token=", 
                          as.character(credentials[credentials$Details =='Appsflyer_Token',][2]),
                          "&from=",
                          start_date, "&to=", end_date)

# add column to define which platform the data came from (ios)
ios_geo <- read_csv(ios_geo_url)
ios_geo <- ios_geo %>%
  select(Date, Country, `Media Source (pid)`, `Campaign (c)`, Installs) %>%
  mutate(platform = "ios")

# add column to define which platform the data came from (android)
android_geo <- read_csv(android_geo_url)
android_geo <- android_geo %>%
  select(Date, Country, `Media Source (pid)`, `Campaign (c)`, Installs) %>%
  mutate(platform = "android")


# merge daily data
ios_android_geo <- rbind(ios_geo, android_geo)

# Iteration 1 
ios_android_raw_url1 <- ios_android_raw_url
ios_android_daily1 <- ios_android_daily  
ios_android_geo1 <- ios_android_geo

# Iteration 2 
ios_android_raw_url2 <- ios_android_raw_url
ios_android_daily2 <- ios_android_daily  
ios_android_geo2 <- ios_android_geo



ios_android_raw_url  <- rbind(ios_android_raw_url1,ios_android_raw_url2 )
ios_android_daily <- rbind(ios_android_daily1,ios_android_daily2)
ios_android_geo <- rbind(ios_android_geo1,ios_android_geo2  )


# aggregate raw data + build funnel output for App Installs(organic) -------------
ios_android_installs_organic <- ios_android_geo %>% 
  mutate(date = format.Date(`Date`, "%Y-%m-%d")) %>%
  filter(Country == 'PH' & `Media Source (pid)` == 'Organic') %>% 
  group_by(date, `Media Source (pid)`, `Campaign (c)`, Country, platform) %>%
  summarise(installs = sum(Installs, na.rm=TRUE)) %>%
  mutate(ad = NA) %>%
  select(date, Country, Platform = "platform", media_source = "Media Source (pid)",  Campaign = "Campaign (c)",
         Ad = "ad", no_users = "installs") %>%
  mutate(LifeStage = "Stage 1 : App Installs") %>% 
  ungroup() %>% 
  mutate(media_source = case_when(media_source == 'Organic' ~ 'non-attributable'))

# build funnel output for App Installs(paid) ---------------------------------
ios_android_installs_paid <- ios_android_raw_url %>%
  mutate(date = format.Date(`Install Time`, "%Y-%m-%d")) %>%
  filter(`Country Code` == 'PH') %>% 
  group_by(date, `Media Source`, Campaign, `Country Code`,Platform, Ad) %>%
  summarise(installs = length(`Install Time`)) %>%
  select(date, Country = "Country Code", Platform, media_source = "Media Source",  Campaign,
         Ad, no_users = "installs") %>%
  mutate(LifeStage = "Stage 1 : App Installs") %>% 
  ungroup()



#### Account Creation stage --------------------------------------------------

# query evergent data from Redshift ------------------------------------------
Evergent_account <- dbFetch(dbSendQuery(con, "select cp_customer_id as cp_cust_id_acct_creation, create_date from Evergent.account;"))

Evergent_subs <- dbFetch(dbSendQuery(con, "SELECT cp_customer_id as cp_cust_id_subscriber, validity_start_date, validity_end_date, status, price, promotion_name, promotion_code, promotion_type, 
                                     promotion_start_date, promotion_end_date, cancellation_date, cancellation_reason, last_modify_date
                                     FROM evergent.subscriptions"))


Evergent_subs_rank <- Evergent_subs %>% 
  group_by(cp_cust_id_subscriber) %>%
  mutate(rank = rank(desc(as.POSIXct(last_modify_date)))) %>% 
  filter(rank == 1) %>%
  mutate(cp_cust_id_subscriber2 = cp_cust_id_subscriber)

Evergent_base <- Evergent_account %>% 
  left_join(Evergent_subs_rank, by = c("cp_cust_id_acct_creation" = "cp_cust_id_subscriber")) %>% 
  select(1:15, cp_cust_id_subscriber = "cp_cust_id_subscriber2")


#### Trialist stage ----------------------------------------------------------

# query data from redshift from app_launch tables ----------------------------
android_ios_id_union <- dbFetch(dbSendQuery(con, "SELECT context_traits_account_id, Context_device_advertising_id , context_device_type, context_traits_country_of_registration
                                            FROM fox_android.app_launch
                                            GROUP BY 1,2,3,4
                                            union all
                                            SELECT  context_traits_account_id , Context_device_advertising_id , context_device_type, context_traits_country_of_registration
                                            FROM fox_ios.app_launch
                                            GROUP BY 1,2,3,4;"))


# From Appsflyer (IDFA-IOS,Adevertising ID) w context_device_advertising_id in app_launch --------
attributable_trialists_base1 <- ios_android_raw_url %>%
  mutate(merged_id = coalesce(IDFA, `Advertising ID`)) %>%
  left_join(android_ios_id_union, by = c("merged_id" = "context_device_advertising_id"))



#From app_launch (context_traits_account_id) join w Evergent (cp_cust_id_acct_creation --------------
attributable_trialists_base2 <- attributable_trialists_base1[c("Media Source" , "Channel", "Platform","Campaign",
                                                               "Adset",  "Ad", "Ad Type", "Install Time", "Country Code", 
                                                               "merged_id", "context_traits_account_id")]

rm(attributable_trialists_base1)

# create attributable trialists base table -------------------------------------
attributable_trialists_base2_slice <- attributable_trialists_base2 %>%
  select(`Install Time`, context_traits_account_id,
         `Media Source`, Channel,Campaign, Adset, Ad, `Ad Type`, 
         Platform, merged_id,
         `Country Code`) %>% 
  group_by(  context_traits_account_id) %>% 
  slice(which.max(as.POSIXct(`Install Time`))) %>%
  ungroup()

attributable_trialists_base2_slice <- attributable_trialists_base2_slice %>%
  mutate(context_traits_account_id2 = context_traits_account_id)

# view(dfSummary(attributable_trialists_base2_slice), 
# file = "attributable_trialists_base2_slice.html")

attributable_trialists_base2_sliced_join <- Evergent_base %>%
  mutate(cp_cust_id_acct_creation2 = cp_cust_id_acct_creation) %>%
  left_join(attributable_trialists_base2_slice, by = c("cp_cust_id_acct_creation" = "context_traits_account_id"))

rm(attributable_trialists_base2)

# calculate paid users by date -------------------------------------------
usercount_check_slicedJoin <- attributable_trialists_base2_sliced_join %>%
  mutate(date = format.Date(create_date, "%Y-%m-%d")) %>%
  group_by(date) %>% 
  summarise(users = length(unique(context_traits_account_id2)))

# calculate total users by date from evergent ----------------------------
ios_android_acctcreation_allslicedjoin <- Evergent_base %>%
  mutate(date = format.Date(create_date, "%Y-%m-%d")) %>%
  #filter(!is.na(`Media Source`)) %>%
  group_by(date) %>%
  summarise(users = length(unique(cp_cust_id_acct_creation)))

# calculate organic users by date as difference of Evergent users and paid users ------
ios_android_acctcreation_base_slicedjoin <- ios_android_acctcreation_allslicedjoin %>% 
  left_join(usercount_check_slicedJoin, by = "date") %>%
  select(date, all = 'users.x', paid = 'users.y') %>% 
  mutate(difference = all - paid) %>%
  gather(channel, no_users, 2:4) %>%
  # filter difference as organic traffic, broken down by date
  filter(channel == 'difference') %>%
  mutate(channel = case_when(channel == 'difference' ~ 'organic'))

# build funnel output for account creation ----------------------------------
attributable_trialists_base2_sliced_join1 <- attributable_trialists_base2_sliced_join %>% 
  group_by_all() %>%
  summarise(users = length(unique(cp_cust_id_acct_creation))) %>%
  mutate(date = format.Date(create_date, "%Y-%m-%d"),
         Country = 'PH',
         LifeStage = 'Stage 2 : Account Creation')

ios_android_acctcreation <- attributable_trialists_base2_sliced_join1[c("date", "Country", "Platform", 
                                                                        "Media Source", "Campaign",
                                                                        "Ad","LifeStage", "users")]

ios_android_acctcreation <- ios_android_acctcreation %>% 
  group_by_at(vars(-users)) %>% 
  summarise(users = sum(users)) %>%
  ungroup() %>% 
  mutate(Platform = case_when(is.na(Platform) & is.na(`Media Source`)
                              & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Platform)),
         `Media Source` = case_when(is.na(`Media Source`)
                                    & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                                    TRUE ~ as.character(`Media Source`)),
         Campaign = case_when(is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Campaign)))

names(ios_android_acctcreation) <- c("date", "Country", "Platform", 
                                     "media_source", "Campaign",
                                     "Ad", "LifeStage", "no_users")



# build funnel output for trialist -----------------------------------------------
attributable_trialists_base2_sliced_join_trialist <- attributable_trialists_base2_sliced_join %>% 
  filter(!is.na(cp_cust_id_subscriber)) %>% 
  group_by_all() %>%
  summarise(users = length(unique(cp_cust_id_subscriber))) %>%
  # may be needed to change to promotion_start_date
  mutate(date = format.Date(validity_start_date, "%Y-%m-%d"),
         Country = 'PH',
         LifeStage = 'Stage 3 : Trials')

ios_android_acctcreation_subs <- attributable_trialists_base2_sliced_join_trialist[c("date", "Country", "Platform", 
                                                                                     "Media Source", "Campaign",
                                                                                     "Ad", "LifeStage", "users")]
ios_android_acctcreation_subs <- ios_android_acctcreation_subs %>% 
  group_by_at(vars(-users)) %>% 
  summarise(users = sum(users)) %>%
  ungroup() %>% 
  mutate(Platform = case_when(is.na(Platform) & is.na(`Media Source`)
                              & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Platform)),
         `Media Source` = case_when(is.na(`Media Source`)
                                    & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                                    TRUE ~ as.character(`Media Source`)),
         Campaign = case_when(is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Campaign)))

names(ios_android_acctcreation_subs) <- c("date", "Country", "Platform", 
                                          "media_source", "Campaign",
                                          "Ad", "LifeStage", "no_users")

# build funnel output for month1 -----------------------------------------------

attributable_trialists_subset <- attributable_trialists_base2_sliced_join1[c("date", "validity_start_date",
                                                                             "cancellation_date", "cp_cust_id_subscriber",
                                                                             "Platform",  "Media Source", "Campaign", "Ad")]



ios_android_month1 <- attributable_trialists_subset %>% 
  mutate(user_life_days = Sys.Date() - as.Date(validity_start_date)) %>% 
  mutate(trialist_date = as.Date(validity_start_date)) %>% 
  mutate(month1date = case_when(user_life_days > 31 & is.na(cancellation_date) ~ (as.Date(validity_start_date) + 32),
                                user_life_days > 31 & 
                                  !is.na(cancellation_date) & 
                                  as.Date(cancellation_date) >= as.Date(validity_start_date) &
                                  as.Date(cancellation_date) <= as.Date(validity_start_date) + 31 ~ as.Date("1999-9-9"),
                                user_life_days > 31 & 
                                  !is.na(cancellation_date) & 
                                  as.Date(cancellation_date) > as.Date(validity_start_date) + 31 ~ (as.Date(validity_start_date) + 32))) %>% 
  filter(!is.na(cp_cust_id_subscriber)) %>% 
  group_by_all() %>%
  summarise(users = length(unique(cp_cust_id_subscriber))) %>%
  ungroup() %>% 
  # may be needed to change to promotion_start_date
  mutate(date = format.Date(month1date, "%Y-%m-%d"),
         Country = 'PH',
         LifeStage = 'Stage 4 : Month 1') %>% 
  filter(date != "1999-09-09")

ios_android_month1<- ios_android_month1[c("date", "Country", "Platform", 
                                          "Media Source", "Campaign",
                                          "Ad", "LifeStage", "users")]


ios_android_month1 <- ios_android_month1 %>% 
  group_by_at(vars(-users)) %>% 
  summarise(users = sum(users)) %>%
  ungroup() %>% 
  mutate(Platform = case_when(is.na(Platform) & is.na(`Media Source`)
                              & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Platform)),
         `Media Source` = case_when(is.na(`Media Source`)
                                    & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                                    TRUE ~ as.character(`Media Source`)),
         Campaign = case_when(is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Campaign)))

names(ios_android_month1) <- c("date", "Country", "Platform", 
                               "media_source", "Campaign",
                               "Ad", "LifeStage", "no_users")


# Combine and Export funnel for User Conversion Funnel Performance------------------------------------------------------------------
ios_android_funnel_master_bind <- rbind(ios_android_installs_organic, ios_android_installs_paid,
                                        ios_android_acctcreation, ios_android_acctcreation_subs,ios_android_month1)

ios_android_funnel_master <- ios_android_funnel_master_bind %>% 
  filter(media_source != 'test') %>% 
  mutate(Campaign = case_when(Campaign == 'non-attributable'|Campaign == 'None' ~ 'NA',
                              TRUE ~ as.character(Campaign))) %>% 
  mutate(media_source = case_when(media_source == 'Affle_int'|media_source == 'affle_int' ~ 'Affle',
                                  media_source == 'Email' ~ 'Email',
                                  media_source == 'Facebook Ads' ~ 'FB Ads',
                                  media_source == 'Fox_sports'|media_source == 'fox_sports' ~ 'Fox Sports',
                                  media_source == 'googleadwords_int' ~ 'Adwords',
                                  media_source == 'Mall_activations'|media_source == 'mall_activation_showdown' ~ 'Malls',
                                  media_source == 'Malls'|media_source == 'malls' ~ 'Malls',
                                  media_source == 'national_geographic' ~ 'Nat Geo',
                                  media_source == 'non-attributable'|is.na(media_source) ~ 'NA',
                                  media_source == 'Social' ~ 'FB Social',
                                  TRUE ~ as.character(media_source)))




# Cohort Funnel for User Lifestage ------------------------------------------------------------
attributable_trialists_base2_sliced_join_month1 <- attributable_trialists_base2_sliced_join %>% 
  filter(!is.na(cp_cust_id_subscriber)) %>%
  filter(!is.na(validity_start_date)) %>% 
  mutate(current_date = Sys.time(),
         user_life = as.Date(Sys.time()) - as.Date(validity_start_date)) %>% 
  mutate(LifeStage_cohort_trialist = case_when(user_life > 0 & 
                                                 !is.na(cancellation_date) &
                                                 as.Date(cancellation_date) >= as.Date(validity_start_date) &
                                                 as.Date(cancellation_date) <= (as.Date(validity_start_date) + 31) ~ 'churn_trialist',
                                               TRUE ~ 'trialist')) %>% 
  mutate(LifeStage_cohort_m1 = case_when(##user_life > 31 & 
                                           !is.na(cancellation_date) &
                                           as.Date(cancellation_date) > (as.Date(validity_start_date) + 31) &
                                           as.Date(cancellation_date) <= (as.Date(validity_start_date) + 61) ~ 'churn_month1',
                                           as.Date(cancellation_date) > (as.Date(validity_start_date) + 61) |
                                           (is.na(cancellation_date) &
                                           user_life > 31)  ~ 'month1',
                                         TRUE ~ as.character('na'))) %>% 
  mutate(LifeStage_cohort_m2 = case_when(##user_life > 61 & 
                                           !is.na(cancellation_date) &
                                           as.Date(cancellation_date) > (as.Date(validity_start_date) + 61) &
                                           as.Date(cancellation_date) <= (as.Date(validity_start_date) + 91) ~ 'churn_month2',
                                           as.Date(cancellation_date) > (as.Date(validity_start_date) + 91) |
                                           (is.na(cancellation_date) &
                                           user_life > 61)  ~ 'month2',
                                         TRUE ~ as.character('na'))) %>% 
  mutate(LifeStage_cohort_m3 = case_when(##user_life > 91 & 
                                           !is.na(cancellation_date) & 
                                           as.Date(cancellation_date) > (as.Date(validity_start_date) + 91) ~ 'churn_month3',
                                           as.Date(cancellation_date) > (as.Date(validity_start_date) + 121) |
                                           (is.na(cancellation_date) &
                                           user_life > 91)  ~ 'month3+',
                                         TRUE ~ as.character('na'))) %>% 
  group_by_all() %>%
  summarise(users = length(unique(cp_cust_id_subscriber))) %>%
  mutate(date = format.Date(create_date, "%Y-%m-%d"),
         Country = 'PH') %>% 
  ungroup() %>% 
  select(date, cp_cust_id_acct_creation, cp_cust_id_subscriber, media_source = "Media Source", Channel,
         Campaign, Ad, Platform, Country, LifeStage_cohort_trialist, LifeStage_cohort_m1, 
         LifeStage_cohort_m2, LifeStage_cohort_m3, users) %>% 
  gather(stage, stage_value, 10:14) %>% 
  filter(stage != 'users') %>% 
  mutate(media_source = case_when(media_source == 'Affle_int'|media_source == 'affle_int' ~ 'Affle',
                                  media_source == 'Email' ~ 'Email',
                                  media_source == 'Facebook Ads' ~ 'FB Ads',
                                  media_source == 'Fox_sports'|media_source == 'fox_sports' ~ 'Fox Sports',
                                  media_source == 'googleadwords_int' ~ 'Adwords',
                                  media_source == 'Mall_activations'|media_source == 'mall_activation_showdown' ~ 'Malls',
                                  media_source == 'Malls'|media_source == 'malls' ~ 'Malls',
                                  media_source == 'national_geographic' ~ 'Nat Geo',
                                  media_source == 'non-attributable'|is.na(media_source) ~ 'NA',
                                  media_source == 'Social' ~ 'FB Social',
                                  TRUE ~ as.character(media_source))) %>% 
  filter(media_source != 'test')




# calculation for valid users ---------------------------------------------------------------
ios_android_lifestage <- attributable_trialists_base2_sliced_join_trialist %>% 
  group_by_at(vars(-users)) %>%
  summarise(users = sum(users)) %>%
  ungroup() %>% 
  mutate(user_life_days = Sys.Date() - as.Date(validity_start_date)) %>% 
  mutate(trialist_date = as.Date(validity_start_date)) %>% 
  mutate(month1date = case_when(user_life_days > 31 & is.na(cancellation_date) ~ (as.Date(validity_start_date) + 32),
                                user_life_days > 31 & 
                                  !is.na(cancellation_date) & 
                                  as.Date(cancellation_date) >= as.Date(validity_start_date) &
                                  as.Date(cancellation_date) <= as.Date(validity_start_date) + 31 ~ as.Date("1999-9-9"),
                                user_life_days > 31 & 
                                  !is.na(cancellation_date) & 
                                  as.Date(cancellation_date) > as.Date(validity_start_date) + 31 ~ (as.Date(validity_start_date) + 32))) %>% 
  mutate(month2date = case_when(user_life_days > 61 & is.na(cancellation_date) ~ (as.Date(validity_start_date) + 62),
                                user_life_days > 61 & 
                                  !is.na(cancellation_date) & 
                                  as.Date(cancellation_date) >= as.Date(validity_start_date) &
                                  as.Date(cancellation_date) <= as.Date(validity_start_date) + 61 ~ as.Date("1999-9-9"),
                                user_life_days > 61 & 
                                  !is.na(cancellation_date) & 
                                  as.Date(cancellation_date) > as.Date(validity_start_date) + 61 ~ (as.Date(validity_start_date) + 62))) %>% 
  mutate(month3date = case_when(user_life_days > 91 & is.na(cancellation_date) ~ (as.Date(validity_start_date) + 92),
                                user_life_days > 91 & 
                                  !is.na(cancellation_date) & 
                                  as.Date(cancellation_date) >= as.Date(validity_start_date) &
                                  as.Date(cancellation_date) <= as.Date(validity_start_date) + 91 ~ as.Date("1999-9-9"),
                                user_life_days > 91 & 
                                  !is.na(cancellation_date) & 
                                  as.Date(cancellation_date) > as.Date(validity_start_date) + 91 ~ (as.Date(validity_start_date) + 92))) %>% 
  mutate(cancellation_datediff = as.Date(cancellation_date) - as.Date(validity_start_date)) %>% 
  mutate(trialist_churn_date = case_when(cancellation_datediff >= 0 & cancellation_datediff <= 31 ~ as.Date(cancellation_date)),
         month1_churn_date = case_when(cancellation_datediff >= 32 & cancellation_datediff <= 61 ~ as.Date(cancellation_date)),
         month2_churn_date = case_when(cancellation_datediff >= 62 & cancellation_datediff <= 91 ~ as.Date(cancellation_date)),
         month3_churn_date = case_when(cancellation_datediff >= 92 ~ as.Date(cancellation_date)))

ios_android_trialist_distinct <- ios_android_lifestage %>% 
  group_by(trialist_date) %>% 
  summarise(trialist_count = length(unique(cp_cust_id_subscriber))) %>% 
  select(date = "trialist_date", trialist_count)

ios_android_month1_distinct <- ios_android_lifestage %>% 
  group_by(month1date) %>% 
  summarise(month1_count = length(unique(cp_cust_id_subscriber))) %>% 
  select(date = "month1date", month1_count)

ios_android_month2_distinct <- ios_android_lifestage %>% 
  group_by(month2date) %>% 
  summarise(month2_count = length(unique(cp_cust_id_subscriber))) %>% 
  select(date = "month2date", month2_count)

ios_android_month3_distinct <- ios_android_lifestage %>% 
  group_by(month3date) %>% 
  summarise(month3_count = length(unique(cp_cust_id_subscriber))) %>% 
  select(date = "month3date", month3_count)

ios_android_trialistchurn_distinct <- ios_android_lifestage %>% 
  group_by(trialist_churn_date) %>% 
  summarise(trialist_churn_count = length(unique(cp_cust_id_subscriber))) %>% 
  select(date = "trialist_churn_date", trialist_churn_count)

ios_android_month1churn_distinct <- ios_android_lifestage %>% 
  group_by(month1_churn_date) %>% 
  summarise(month1_churn_count = length(unique(cp_cust_id_subscriber))) %>% 
  select(date = "month1_churn_date", month1_churn_count)

ios_android_month2churn_distinct <- ios_android_lifestage %>% 
  group_by(month2_churn_date) %>% 
  summarise(month2_churn_count = length(unique(cp_cust_id_subscriber))) %>% 
  select(date = "month2_churn_date", month2_churn_count)

ios_android_month3churn_distinct <- ios_android_lifestage %>% 
  group_by(month3_churn_date) %>% 
  summarise(month3_churn_count = length(unique(cp_cust_id_subscriber))) %>% 
  select(date = "month3_churn_date", month3_churn_count)

ios_android_validuser_master <- ios_android_trialist_distinct %>% 
  left_join(ios_android_month1_distinct, by = c("date")) %>% 
  left_join(ios_android_month2_distinct, by = c("date")) %>% 
  left_join(ios_android_month3_distinct, by = c("date")) %>% 
  left_join(ios_android_trialistchurn_distinct, by = c("date")) %>%
  left_join(ios_android_month1churn_distinct, by = c("date")) %>% 
  left_join(ios_android_month2churn_distinct, by = c("date")) %>% 
  left_join(ios_android_month3churn_distinct, by = c("date")) %>% 
  mutate_at(vars(-date), funs(replace(., is.na(.), 0))) %>% 
  mutate(valid_trialists = trialist_count - trialist_churn_count,
         valid_month1 = month1_count - month1_churn_count,
         valid_month2 = month2_count - month2_churn_count,
         valid_month3 = month3_count - month3_churn_count) %>% 
  select(date, valid_trialists, valid_month1, valid_month2, valid_month3) %>% 
  mutate_at(vars(-date), funs(replace(., is.na(.), 0))) %>% 
  mutate_at(vars(-date), cumsum) %>% 
  gather(lifestage, valid_users, 2:5) %>% 
  mutate(lifestage = case_when(lifestage == "valid_trialists" ~ "Trialist",
                               lifestage == "valid_month1" ~ "Trialist + Month 1",
                               lifestage == "valid_month2" ~ "Trialist + Month 2",
                               lifestage == "valid_month3" ~ "Trialist + Month 3+"))

# unpivot funnel master table for souce breakdown table ------------------------
ios_android_funnel_master_stagepivot <- ios_android_funnel_master %>% 
  mutate_at(c("Ad"), funs(replace(., is.na(.), "NA"))) %>% 
  group_by_at(vars(-no_users)) %>% 
  summarise(no_users = sum(no_users)) %>% 
  ungroup() %>% 
  spread(LifeStage, no_users) %>% 
  select(date, Country, Platform, media_source, Campaign, Ad, 
         stage1_appinstalls = "Stage 1 : App Installs",
         stage2_accountcreation = "Stage 2 : Account Creation",
         stage3_trials = "Stage 3 : Trials") %>% 
  mutate_at(c("stage1_appinstalls", "stage2_accountcreation", "stage3_trials"), funs(replace(., is.na(.), 0))) %>% 
  mutate_at(c("stage1_appinstalls", "stage2_accountcreation", "stage3_trials"), funs( as.integer(.))) 

# build funnel output for month 1,2,3 ------------------------------------------
attributable_trialists_base2_month123 <- attributable_trialists_base2_sliced_join_trialist %>% 
  mutate(current_date = Sys.time()) %>% 
  mutate(days_since_promo_end = as.numeric(round(difftime(current_date,validity_start_date, 
                                                          unit = "days"), digits =2))) %>% 
  mutate(months_since_trial = case_when(between(days_since_promo_end, 32, 61) ~ 'Trialist + Month 1',
                                        between(days_since_promo_end, 62, 91) ~ 'Trialist + Month 2',
                                        days_since_promo_end > 91 ~ 'Trialist + Month 3+',
                                        days_since_promo_end < 32 ~ 'Trialist'),
         members_churned = case_when(!is.na(cancellation_date) ~ '1',
                                     TRUE ~ '0'))


ios_android_trialists_month123 <- attributable_trialists_base2_month123[c("date", "Country", "Platform", 
                                                                          "Media Source", "Campaign",
                                                                          "Ad", "months_since_trial", 
                                                                          "users")]
ios_android_trialists_month123 <- ios_android_trialists_month123 %>% 
  group_by_at(vars(-users)) %>% 
  summarise(users = sum(users)) %>%
  ungroup() %>% 
  mutate(date = as.Date(date),
         Platform = case_when(is.na(Platform) & is.na(`Media Source`)
                              & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Platform)),
         `Media Source` = case_when(is.na(`Media Source`)
                                    & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                                    TRUE ~ as.character(`Media Source`)),
         Campaign = case_when(is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Campaign)))


names(ios_android_trialists_month123) <- c("date", "Country", "Platform", 
                                           "media_source", "Campaign",
                                           "Ad", "LifeStage", "no_users")


# join aggregated video views with evergent - find b2c users ----------------------------
# calculate churn --------------------------------------------------------------------------------------

# evergent table of subscriber ids and no. of cancellations
Evergent_churn_idtable <- Evergent_subs %>% 
  group_by(cp_cust_id_subscriber) %>% 
  summarise(count_of_cancellation = length(unique(na.omit(cancellation_date))))

# evergent churn master table, joined with no. of cancellations
Evergent_calc_churn <- Evergent_subs %>% 
  select(-last_modify_date) %>% 
  mutate(current_date = Sys.Date()) %>% 
  mutate(days_since_trial = as.numeric(round(difftime(current_date,validity_start_date, 
                                                      unit = "days"), digits =2))) %>% 
  mutate(months_since_trial = round(days_since_trial / 30, digits = 2),
         lifestage = case_when(!is.na(cancellation_date) ~ as.numeric(round(difftime(as.Date(cancellation_date),as.Date(validity_start_date), 
                                                                                     unit = "days"), digits =2))),
         lifestage2 = case_when(is.na(cancellation_date) ~ as.numeric(round(difftime(as.Date(current_date),as.Date(validity_start_date), 
                                                                                     unit = "days"), digits =2)))) %>% 
  mutate(lifestage_final = coalesce(lifestage, lifestage2)) %>% 
  select(-lifestage, -lifestage2) %>% 
  mutate(lifestage_months = floor(lifestage_final/30)) %>% 
  select(cp_cust_id_subscriber, validity_start_date, cancellation_date, 
         cancellation_reason) %>% 
  filter(!is.na(cancellation_date)) %>% 
  unique() %>% 
  mutate(lifestage = case_when(!is.na(cancellation_date) ~ as.numeric(round(difftime(as.Date(cancellation_date),as.Date(validity_start_date), 
                                                                                     unit = "days"), digits =2)))) %>% 
  mutate(lifestage_months = floor(lifestage/30),
         lifestage_label = case_when(lifestage_months == 0 ~ 'Trialist',
                                     lifestage_months == 1 ~ 'Trialist + Month 1',
                                     lifestage_months == 2 ~ 'Trialist + Month 2',
                                     lifestage_months == 3 ~ 'Trialist + Month 3+'))



attributable_trialists_base2_month123 <- attributable_trialists_base2_sliced_join_trialist %>% 
  mutate(current_date = Sys.time()) %>% 
  mutate(days_since_promo_end = as.numeric(round(difftime(current_date,promotion_end_date, 
                                                          unit = "days"), digits =2))) %>% 
  mutate(months_since_trial = case_when(between(days_since_promo_end, 0, 30) ~ 'Month 1',
                                        between(days_since_promo_end, 31, 60) ~ 'Month 2',
                                        days_since_promo_end > 60 ~ 'Month 3+',
                                        days_since_promo_end < 0 ~ 'Trialist'))

attributable_trialists_base2_month123 <- attributable_trialists_base2_month123[c("create_date", "cp_cust_id_subscriber",
                                                                                 "cancellation_date",
                                                                                 "Media Source", "Channel",
                                                                                 "Campaign", "Adset", "Ad", "Platform", "months_since_trial")]  


# this table contains cancellation date ------------------------------------------
churned_user_joined_month123 <- Evergent_calc_churn %>% 
  left_join(attributable_trialists_base2_month123, by = "cp_cust_id_subscriber") %>% 
  select(-cancellation_date.y, date = cancellation_date.x, 
         media_source = "Media Source", LifeStage = "lifestage_label") %>% 
  mutate(Country = 'PH', date = as.Date(date)) %>% 
  group_by_at(vars(-cp_cust_id_subscriber)) %>% 
  summarise(members_churned = length(cp_cust_id_subscriber))


ios_android_churnedusers <- churned_user_joined_month123[c("date", "Country", "Platform", 
                                                           "media_source", "Campaign",
                                                           "Ad", "LifeStage", 
                                                           "members_churned")]

ios_android_churned_month123 <- ios_android_churnedusers %>% 
  group_by_at(vars(-members_churned)) %>% 
  summarise(members_churned = sum(members_churned)) %>%
  ungroup() %>% 
  mutate(Platform = case_when(is.na(Platform) & is.na(media_source)
                              & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Platform)),
         `Media Source` = case_when(is.na(media_source)
                                    & is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                                    TRUE ~ as.character(media_source)),
         Campaign = case_when(is.na(Campaign) & is.na(Ad) ~ 'non-attributable',
                              TRUE ~ as.character(Campaign))) %>% 
  select(date, Country, Platform, media_source = `Media Source`, Campaign, Ad, LifeStage, members_churned) %>% 
  filter(media_source != 'test') %>% 
  mutate(media_source = case_when(media_source == 'Affle_int'|media_source == 'affle_int' ~ 'Affle',
                                  media_source == 'Email' ~ 'Email',
                                  media_source == 'Facebook Ads' ~ 'FB Ads',
                                  media_source == 'Fox_sports'|media_source == 'fox_sports' ~ 'Fox Sports',
                                  media_source == 'googleadwords_int' ~ 'Adwords',
                                  media_source == 'Mall_activations'|media_source == 'mall_activation_showdown' ~ 'Malls',
                                  media_source == 'Malls'|media_source == 'malls' ~ 'Malls',
                                  media_source == 'national_geographic' ~ 'Nat Geo',
                                  media_source == 'non-attributable'|is.na(media_source) ~ 'NA',
                                  media_source == 'Social' ~ 'FB Social',
                                  TRUE ~ as.character(media_source)))


# youbora data ------------------------------------------------------------
youbora_userfinishedwatching <- dbFetch(dbSendQuery(con, "SELECT metadata_channel_code, type, end_time,
                                                    title, start_time, metadata_id, original_timestamp, user_id, playtime, buffering_time, jointime, channel_code,
                                                    media_duration, guid, genre, series_title, program_type
                                                    FROM fox_youbora.user_finished_watching where date(original_timestamp) > '2018-05-21'
                                                    group by
                                                    metadata_channel_code, type, end_time,
                                                    title, start_time, metadata_id, original_timestamp, user_id, playtime, buffering_time, jointime, channel_code,
                                                    media_duration, guid, genre, series_title, program_type ;"))

# create table with youbora video details ------------------------------------------
youbora_userfinishedwatching_base <- youbora_userfinishedwatching %>% 
  select(user_id, playtime, original_timestamp, media_duration, 
         title, channel_code, program_type, series_title) %>% 
  mutate(playtime = as.numeric(playtime),
         vid_views = case_when(playtime >= 60 ~ '1',
                               playtime < 60 ~ '0')) %>% 
  group_by(user_id, playtime,  original_timestamp, media_duration, 
           title, channel_code, program_type, series_title) %>% 
  summarise(vid_watchtime = sum(as.numeric(playtime)),
            vid_views = sum(as.numeric(vid_views)))

# create table with aggregated video views -----------------------------------------
youbora_userfinishedwatching_base_agg <- youbora_userfinishedwatching_base %>% 
  ungroup() %>% 
  mutate(original_timestamp = as.Date(original_timestamp)) %>% 
  group_by(original_timestamp, user_id) %>% 
  summarise(vid_watchtime = sum(vid_watchtime),
            vid_views = sum(vid_views)) %>% 
  mutate(month = month(original_timestamp),
         week = week(original_timestamp))


rm(youbora_userfinishedwatching)

# get profile id and account id from app launch tables ----------------------------------
android_ios_profileid_union <- dbFetch(dbSendQuery(con, "SELECT context_traits_profile_id, context_traits_account_id FROM fox_android.app_launch
                                                   GROUP BY 1,2
                                                   union all
                                                   SELECT  context_traits_profile_id, context_traits_account_id
                                                   FROM fox_ios.app_launch
                                                   GROUP BY 1,2;"))

android_ios_profileid_union <- android_ios_profileid_union %>% 
  mutate(context_traits_profile_id2 = context_traits_profile_id,
         context_traits_account_id2 = context_traits_account_id)

Evergent_subscriber_table <- Evergent_base %>% 
  select(create_date, cp_cust_id_subscriber, validity_start_date, validity_end_date, 
         promotion_start_date, promotion_end_date, cancellation_date) %>% 
  mutate(cp_cust_id_subscriber2 = cp_cust_id_subscriber) %>% 
  filter(!is.na(cp_cust_id_subscriber))


# user engagement performance by life stage cohort - get youbora base table  -------------------
youbora_userfinishedwatching_joined <- youbora_userfinishedwatching_base_agg %>% 
  left_join(android_ios_profileid_union, by = c("user_id" = "context_traits_profile_id2")) %>% 
  left_join(Evergent_subscriber_table, by = c("context_traits_account_id2" = "cp_cust_id_subscriber2")) %>%
  filter(!is.na(cp_cust_id_subscriber)) %>% 
  mutate(days_since_promo_end = as.numeric(round(difftime(original_timestamp,promotion_end_date, 
                                                          unit = "days"), digits =2))) %>% 
  mutate(months_since_trial = case_when(between(days_since_promo_end, 0, 30) ~ 'Trialist + Month 1',
                                        between(days_since_promo_end, 31, 60) ~ 'Trialist + Month 2',
                                        days_since_promo_end > 60 ~ 'Trialist + Month 3+',
                                        days_since_promo_end < 0 ~ 'Trialist'))

# user engagement performance by life stage cohort - weekly active users ----------------------
youbora_weeklyactive <- youbora_userfinishedwatching_joined %>% 
  filter(!is.na(months_since_trial)) %>% 
  mutate(createdate_week = week(create_date)) 

youbora_weeklyactive_union <- sqldf('select original_timestamp, "Trialist" as lifestage,
                                    (select count(distinct user_id) from youbora_weeklyactive 
                                    where (original_timestamp between y.original_timestamp -6
                                    AND y.original_timestamp ) AND months_since_trial = "Trialist"
                                    ) as weekly_active_users
                                    from youbora_weeklyactive y
                                    group by 1,2
                                    
                                    UNION ALL 
                                    
                                    select original_timestamp, "Trialist + Month 1" as lifestage,
                                    (select count(distinct user_id) from youbora_weeklyactive 
                                    where (original_timestamp between y.original_timestamp -6
                                    AND y.original_timestamp ) AND months_since_trial = "Trialist + Month 1"
                                    ) as weekly_active_users
                                    from youbora_weeklyactive y
                                    group by 1,2
                                    UNION ALL
                                    
                                    select original_timestamp, "Trialist + Month 2" as lifestage,
                                    (select count(distinct user_id) from youbora_weeklyactive 
                                    where (original_timestamp between y.original_timestamp -6
                                    AND y.original_timestamp ) AND months_since_trial = "Trialist + Month 2"
                                    ) as weekly_active_users
                                    from youbora_weeklyactive y
                                    group by 1,2
                                    
                                    UNION ALL
                                    select original_timestamp, "Trialist + Month 3+" as lifestage,
                                    (select count(distinct user_id) from youbora_weeklyactive 
                                    where (original_timestamp between y.original_timestamp -6
                                    AND y.original_timestamp ) AND months_since_trial = "Trialist + Month 3+"
                                    ) as weekly_active_users
                                    from youbora_weeklyactive y
                                    group by 1,2
                                    ')

youbora_weeklyactive_union <- youbora_weeklyactive_union %>% 
  filter(weekly_active_users != 0) %>% 
  select(date = "original_timestamp", lifestage, weekly_active_users)

# user engagement performance by life stage cohort - video watch time and plays
youbora_userfinishedwatching_sum <- youbora_userfinishedwatching_joined %>% 
  select(-month, -week) %>% 
  group_by(original_timestamp, months_since_trial) %>% 
  summarise(vid_watchtime = sum(vid_watchtime),
            users = n_distinct(user_id),
            vid_views = sum(vid_views)) %>% 
  filter(!is.na(months_since_trial)) %>% 
  left_join(youbora_weeklyactive_union, by = c("original_timestamp" = "date",
                                               "months_since_trial" = "lifestage")) %>% 
  left_join(ios_android_validuser_master, by = c("original_timestamp" = "date",
                                                 "months_since_trial" = "lifestage"))

youbora_userfinishedwatching_sum_split <- split(youbora_userfinishedwatching_sum, 
                                                youbora_userfinishedwatching_sum$months_since_trial) 

youbora_userfinishedwatching_sum_split$Trialist <- youbora_userfinishedwatching_sum_split$Trialist %>% 
  filter(as.Date(original_timestamp) >= '2018-05-28')
youbora_userfinishedwatching_sum_split$`Trialist + Month 1` <- youbora_userfinishedwatching_sum_split$`Trialist + Month 1` %>% 
  filter(as.Date(original_timestamp) >= '2018-06-29')
youbora_userfinishedwatching_sum_split$`Trialist + Month 2` <- youbora_userfinishedwatching_sum_split$`Trialist + Month 2` %>% 
  filter(as.Date(original_timestamp) >= '2018-07-29')
youbora_userfinishedwatching_sum_split$`Trialist + Month 3` <- youbora_userfinishedwatching_sum_split$`Trialist + Month 3` %>% 
  filter(as.Date(original_timestamp) >= '2018-08-29')

youbora_userfinishedwatching_sum <- bind_rows(youbora_userfinishedwatching_sum_split)


# connect to database --------------------------------------------------------
con <- dbConnect(RPostgres::Postgres(), 
                 dbname="foxplusbeta",
                 host='foxplusbeta.cvacqrqwmflb.ap-southeast-1.redshift.amazonaws.com', 
                 port='32545',
                 user='sparkline', 
                 password='394$$m_Rsm=X',
                 sslmode='require')


#aws.s3::delete_object("ios_android_churned_month123", bucket = "fox-sparkline")


bucket_objects <- get_bucket(
  bucket = as.character(credentials[credentials$Details =='s3_bucket',][2]),
  key = as.character(credentials[credentials$Details =='s3_bucket_key',][2]),
  secret = as.character(credentials[credentials$Details =='s3_bucket_secret',][2])
)


# ---- Push data into S3
# save an in-memory R object into S3
write_csv(android_ios_id_revenue,"android_ios_id_revenue.csv")
write_csv(ios_android_funnel_master,"ios_android_funnel_master.csv")
write_csv(youbora_userfinishedwatching_sum,"youbora_userfinishedwatching_sum.csv")
write_csv(ios_android_churned_month123,"ios_android_churned_month123.csv")
write_csv(attributable_trialists_base2_sliced_join_month1,"attributable_trialists_base2_sliced_join_month1.csv")
write_csv(ios_android_funnel_master_stagepivot,"ios_android_funnel_master_stagepivot.csv")


put_object(file = "android_ios_id_revenue.csv",
           bucket = "fox-sparkline", object = "android_ios_id_revenue.csv")

put_object(file = "ios_android_funnel_master.csv",
           bucket = "fox-sparkline", object = "ios_android_funnel_master.csv")

put_object(file = "youbora_userfinishedwatching_sum.csv",
           bucket = "fox-sparkline", object = "youbora_userfinishedwatching_sum.csv")

put_object(file = "ios_android_churned_month123.csv",
           bucket = "fox-sparkline", object = "ios_android_churned_month123.csv")

put_object(file = "attributable_trialists_base2_sliced_join_month1.csv",
           bucket = "fox-sparkline", object = "attributable_trialists_base2_sliced_join_month1.csv")

put_object(file = "ios_android_funnel_master_stagepivot.csv",
           bucket = "fox-sparkline", object = "ios_android_funnel_master_stagepivot.csv")

dbSendQuery(con, "DELETE FROM fox_sparkline.android_ios_id_revenue;")
dbSendQuery(con, "DELETE FROM fox_sparkline.ios_android_funnel_master;")
dbSendQuery(con, "DELETE FROM fox_sparkline.ios_android_funnel_master_stagepivot;")
dbSendQuery(con, "DELETE FROM fox_sparkline.youbora_userfinishedwatching_sum;")
dbSendQuery(con, "DELETE FROM fox_sparkline.ios_android_churned_month123;")
dbSendQuery(con, "DELETE FROM fox_sparkline.attributable_trialists_base2_sliced_join_month1;")

dbSendQuery(con, paste0("COPY fox_sparkline.android_ios_id_revenue from 's3://fox-sparkline/android_ios_id_revenue.csv' credentials ", 
                        "'aws_access_key_id=",
                        as.character(credentials[credentials$Details =='AWS_ACCESS_KEY_ID',][2]), ";",
                        "aws_secret_access_key=",
                        as.character(credentials[credentials$Details =='AWS_SECRET_ACCESS_KEY',][2]), "'",
                        " CSV IGNOREHEADER 1;"))

dbSendQuery(con, paste0("COPY fox_sparkline.ios_android_funnel_master from 's3://fox-sparkline/ios_android_funnel_master.csv' credentials ", 
                        "'aws_access_key_id=",
                        as.character(credentials[credentials$Details =='AWS_ACCESS_KEY_ID',][2]), ";",
                        "aws_secret_access_key=",
                        as.character(credentials[credentials$Details =='AWS_SECRET_ACCESS_KEY',][2]), "'",
                        " CSV IGNOREHEADER 1;"))

dbSendQuery(con, paste0("COPY fox_sparkline.youbora_userfinishedwatching_sum from 's3://fox-sparkline/youbora_userfinishedwatching_sum.csv' credentials ", 
                        "'aws_access_key_id=",
                        as.character(credentials[credentials$Details =='AWS_ACCESS_KEY_ID',][2]), ";",
                        "aws_secret_access_key=",
                        as.character(credentials[credentials$Details =='AWS_SECRET_ACCESS_KEY',][2]), "'",
                        " CSV IGNOREHEADER 1;"))

dbSendQuery(con, paste0("COPY fox_sparkline.ios_android_churned_month123 from 's3://fox-sparkline/ios_android_churned_month123.csv' credentials ", 
                        "'aws_access_key_id=",
                        as.character(credentials[credentials$Details =='AWS_ACCESS_KEY_ID',][2]), ";",
                        "aws_secret_access_key=",
                        as.character(credentials[credentials$Details =='AWS_SECRET_ACCESS_KEY',][2]), "'",
                        " CSV IGNOREHEADER 1;"))

dbSendQuery(con, paste0("COPY fox_sparkline.attributable_trialists_base2_sliced_join_month1 from 's3://fox-sparkline/attributable_trialists_base2_sliced_join_month1.csv' credentials ", 
                        "'aws_access_key_id=",
                        as.character(credentials[credentials$Details =='AWS_ACCESS_KEY_ID',][2]), ";",
                        "aws_secret_access_key=",
                        as.character(credentials[credentials$Details =='AWS_SECRET_ACCESS_KEY',][2]), "'",
                        " CSV IGNOREHEADER 1;"))

dbSendQuery(con, paste0("COPY fox_sparkline.ios_android_funnel_master_stagepivot from 's3://fox-sparkline/ios_android_funnel_master_stagepivot.csv' credentials ", 
                        "'aws_access_key_id=",
                        as.character(credentials[credentials$Details =='AWS_ACCESS_KEY_ID',][2]), ";",
                        "aws_secret_access_key=",
                        as.character(credentials[credentials$Details =='AWS_SECRET_ACCESS_KEY',][2]), "'",
                        " CSV IGNOREHEADER 1;"))

dbDisconnect(con)



# # Get the specific error that causes file upload into Redshift to fail
# errorlog <- dbFetch(dbSendQuery(con, "SELECT TRUNC(starttime) AS day,
#        colname,
#                     err_reason,
#                     COUNT(*) AS amount
#                     FROM stl_load_errors
#                     WHERE starttime >sysdate -3
#                     GROUP BY day,
#                     colname,
#                     err_reason
#                     ORDER BY day,
#                     amount DESC;;"))


#KEEP FOR REFERENCE ------------------------------------------------------------------------------------

# if the upload doesnt work,  need to drop table (using code in team sql) in Redshift then create table using code in the reference section below (not literally below)
#rs_replace_table(ios_android_funnel_master, dbcon=con,table_name='fox_sparkline.ios_android_funnel_master', bucket="fox-sparkline")
#rs_replace_table(android_ios_id_revenue, dbcon=con, table_name='fox_sparkline.android_ios_id_revenue',bucket="fox-sparkline")
#rs_replace_table(youbora_userfinishedwatching_sum, dbcon=con, table_name='fox_sparkline.youbora_userfinishedwatching_sum',bucket="fox-sparkline")
#rs_replace_table(ios_android_churned_month123, dbcon=con, table_name='fox_sparkline.ios_android_churned_month123_new',bucket="fox-sparkline")
#rs_replace_table(attributable_trialists_base2_sliced_join_month1, dbcon=con, table_name='fox_sparkline.attributable_trialists_base2_sliced_join_month1',bucket="fox-sparkline")
#rs_replace_table(ios_android_funnel_master_stagepivot, dbcon=con, table_name='fox_sparkline.ios_android_funnel_master_stagepivot',bucket="fox-sparkline")
# # sample query for reference
# sample_total_trialists <- dbFetch(dbSendQuery(con, "SELECT date(subscriptions.promotion_start_date::text) AS date,
#                                               count(DISTINCT subscriptions.cp_customer_id) AS new_free_trialist from
#                                               evergent.subscriptions 
#                                               WHERE subscriptions.promotion_start_date::text >= '2018-05-22 00:00:00'::text
#                                               GROUP BY date(subscriptions.promotion_start_date::text);"))
# 
# 
# android_ios_id_union %>% group_by(context_device_advertising_id) %>% 
#   summarise(distinct = n_distinct(context_device_advertising_id))
# funnel_raw <- sqldf("
#                     SELECT A.context_traits_account_id AS context_traits_account_id, C.cp_customer_id AS customer_id, A.device_type AS platform, A.install_time AS install_time, A.media_source AS media_source, A.af_channel AS channel, A.campaign AS campaign, A.ad AS ad, 
#                     C.create_date as create_date, C.validity_start_date AS validity_start_date, C.validity_end_date AS validity_end_date, C.status AS status, C.price AS price, C.promotion_name AS promotion_name, C.promotion_code AS promotion_code, C.promotion_type AS promotion_type, 
#                     C.promotion_start_date AS promotion_start_date, C.promotion_end_date AS promotion_end_date, C.cancellation_date AS cancellation_date, C.cancellation_reason AS cancellation_reason
#                     
#                     FROM
#                     
#                     (appsflyer_installs) as A
#                     LEFT JOIN 
#                     (Evergent_base ) as C
#                     
#                     ON 
#                     
#                     A.context_traits_account_id=C.cp_customer_id")
# 
# funnel_raw$install_time <- as.character(funnel_raw$install_time)
# funnel_raw$validity_end_date <- as.character(funnel_raw$validity_end_date)
# 
# funnel_table <- funnel_raw %>%
#   mutate(current_date = Sys.Date()) %>%
#   mutate(account_creation = case_when(!is.na(create_date) ~ "TRUE"),
#          subs_trialist_lifestage = case_when(current_date - as.Date(cancellation_date) > 0 
#                                              & current_date - as.Date(validity_start_date) > 0 &
#                                                current_date - as.Date(validity_start_date) <= 30 ~ "Trialist"),
#          month1_lifestage = case_when(current_date - as.Date(cancellation_date) > 0 & 
#                                         current_date - as.Date(validity_start_date) >= 31 &
#                                         current_date - as.Date(validity_start_date) <= 60 ~ "Month 1"),
#          month2_lifestage = case_when(current_date - as.Date(cancellation_date) > 0 & 
#                                         current_date - as.Date(validity_start_date) >= 61 &
#                                         current_date - as.Date(validity_start_date) <= 90 ~ "Month 2"),                               
#          month3_lifestage = case_when(current_date - as.Date(cancellation_date) > 0 & 
#                                         current_date - as.Date(validity_start_date) >= 91 &
#                                         current_date - as.Date(validity_start_date) <= 120 ~ "Month 3"),                                 
#          month3plus_lifestage = case_when(current_date - as.Date(cancellation_date) > 0 & 
#                                             current_date - as.Date(validity_start_date) >= 121 &
#                                             current_date - as.Date(validity_start_date) <= 150 ~ "Month 3+"))
# 
# funnel_output <- 
#   sqldf('select date(install_time) as date, 
#         case when install_time is not null then count (install_time) end as installs,
#         case when account_creation is not null then count(install_time) end as account_creation,
#         case when subs_trialist_lifestage is not null then count(distinct customer_id) end as subs_trialist_lifestage,
#         case when month1_lifestage is not null then count(distinct customer_id) end as month1_lifestage, 
#         case when month2_lifestage is not null then count(distinct customer_id) end as month2_lifestage, 
#         case when month3_lifestage is not null then count(distinct customer_id) end as month3_lifestage,
#         case when month3plus_lifestage is not null then count(distinct customer_id) end as month3plus_lifestage
#         from funnel_table group by date')
# 
# write_csv(funnel_table, "funnel_table.csv")
# 
# funnel_exported <- read_csv("Book5.csv")
# funnel_exported$Install_Date <- as.Date(funnel_exported$Install_Date)
# funnel_exported1 <- funnel_exported %>%
#   gather(stage, no_users, 5:8)
# 
# write_csv(funnel_exported1, "funnel_exported1.csv")
# 
# funnel_table %>%
#   group_by(as.Date(install_time)) %>%
#   summarise(installs = case_when(!is.na(install_time) ~ length(install_time)))

# Logic to get data from appsflyer api and check redshift --------------------------------------------
appsflyer_date_check <- dbFetch(dbSendQuery(con, "select to_date(event_time, 'YYYY-MM-DD') as date 
                                            from fox_sparkline.ios_android_raw_url
                                            group by date;"))

# query the date column from ios android raw url table 
# in redshift find the max date (event_time)
daysdiference <- as.numeric(difftime(Sys.Date()-1, max(appsflyer_date_check$date), units = "days"))
days_for_datapull <- case_when(daysdiference <= 0 ~ 0,
                               daysdiference > 0 ~ daysdiference)

# if days for datapull > 8, split it into groups of 7

max_startdate_check <- as.Date(ifelse(as.Date(max(appsflyer_date_check$date) + days_for_datapull) >= as.Date(Sys.Date()-1),
                              Sys.Date()-1,
                              as.Date(max(appsflyer_date_check$date) + days_for_datapull)))

# create list of start and end dates
start_date_list <- seq(
  as.Date(max(appsflyer_date_check$date) + 1), 
  max_startdate_check,
  by = 8)

end_date_list <- as.Date(ifelse(as.Date(start_date_list + 7) >= as.Date(Sys.Date() - 1),
                        as.Date(Sys.Date() - 1),
                        as.Date(start_date_list + 7)))

# merge start and end date lists into a dataframe
date_table <- do.call("rbind", mapply(data.frame,
                     start_date = as.list(start_date_list),
                     end_date = as.list(end_date_list),
                     SIMPLIFY = FALSE))

# for each row of the dataframe, run function to pull appsflyer data
appsflyer_data_pull <- function(x) {
  ios_url  <-
    paste0(
      "https://hq.appsflyer.com/export/id1116595369/installs_report/v5?api_token=",
      as.character(credentials[credentials$Details =='Appsflyer_Token',][2]),
      "&from=",
      x[1],
      "&to=",
      x[2]
    )
  
  android_url  <-
    paste0(
      "https://hq.appsflyer.com/export/com.fng.foxplus/installs_report/v5?api_token=",
      as.character(credentials[credentials$Details =='Appsflyer_Token',][2]),
      "&from=",
      x[1],
      "&to=",
      x[2],
      "&additional_fields=install_app_store,match_type,contributor1_match_type,contributor2_match_type,contributor3_match_type,device_category")
  
  tryCatch(
    # Get Raw Data from Appsflyer API -----------------------------------------------
    ios_raw_url <- read_csv(ios_url),
    error = function(e) {
      NA
    }
  )
  
  tryCatch(
    android_raw_url1 <- read_csv(android_url) %>%
      select(-`Install App Store`,
             -`Match Type`,
             -`Contributor 1 Match Type`,
             -`Contributor 2 Match Type`,
             -`Contributor 3 Match Type`,
             -`Device Category`),
    error = function(e) {
      NA
    }
  )
  ios_android_raw_datapull  <- rbind(ios_raw_url,android_raw_url1 )
}
  
ios_android_raw_bind <- as.data.frame(apply(date_table, 1, appsflyer_data_pull))
ios_android_raw_bind <- ios_android_raw_bind %>%
  rename_all(tolower) %>% 
  rename_at(.vars = vars(contains(".")),
            .funs = funs(gsub("\\.", "_", .)))

  # get full dataset from redshift
  ios_android_raw_fulldata <- dbFetch(dbSendQuery(con, "select * from fox_sparkline.ios_android_raw_url;"))
  
  # rbind ios_android_raw_bind to existing full dataset
  # if table ios_android_raw_bind exists then check if redshift max date >= max event time in data pull from API
  # if table ios_android_raw_bind doesnt exist then check if redshift max date >= yesterday's date
  
  conditional_control <- as.data.frame(ifelse(exists("ios_android_raw_bind"), 
                       as.data.frame(ifelse(as.Date(max(appsflyer_date_check$date)) >= as.Date(max(ios_android_raw_bind$event_time)),
                                            ios_android_raw_url <- ios_android_raw_fulldata,
                                            ios_android_raw_url <- rbind(ios_android_raw_fulldata, 
                                                                         ios_android_raw_bind))),
                       as.data.frame(ifelse(as.Date(max(appsflyer_date_check$date)) >= as.Date(Sys.Date() - 1),
                              ios_android_raw_url <- ios_android_raw_fulldata,
                              ios_android_raw_url <- rbind(ios_android_raw_fulldata, 
                                                           ios_android_raw_bind)))))
  
  # Prepare data for re-upload to Redshift via append - only up to previous day
  # Current date would not contain the full day's data hence this is not uploaded yet
  ios_android_raw_bind_maxdate <- as.Date(max(ios_android_raw_bind$event_time))
  ios_android_raw_bind <- ios_android_raw_bind %>% 
    filter(as.Date(event_time) <= ios_android_raw_bind_maxdate)
  
  # re-upload via append, ios_android_raw_bind dataset to redshift
  write_csv(ios_android_raw_bind,"ios_android_raw_bind.csv")
  put_object(file = "ios_android_raw_bind.csv",
             bucket = "fox-sparkline", object = "ios_android_raw_bind.csv")
  dbSendQuery(con, paste0("COPY fox_sparkline.ios_android_raw_url from 's3://fox-sparkline/ios_android_raw_bind.csv' credentials ", 
                          "'aws_access_key_id=",
                          as.character(credentials[credentials$Details =='AWS_ACCESS_KEY_ID',][2]), ";",
                          "aws_secret_access_key=",
                          as.character(credentials[credentials$Details =='AWS_SECRET_ACCESS_KEY',][2]), "'",
                          " CSV IGNOREHEADER 1;"))
  