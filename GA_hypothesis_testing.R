library(tidyverse)
library(googleAnalyticsR)

ga_auth(new_user = TRUE)

## get your accounts
account_list <- ga_account_list()

startDate <- "2018-01-13"
endDate <- "2018-01-30"

rollup <- account_list$viewId[account_list$viewName=='Roll-up All (filtered)']

landing_A_filter <- dim_filter(dimension="landingPagePath",operator="EXACT",expressions="/sg")

my_filter_A_clause <- filter_clause_ga4(list(landing_A_filter))

landing_B_filter <- dim_filter(dimension="landingPagePath",operator="EXACT",expressions="/cn")

my_filter_B_clause <- filter_clause_ga4(list(landing_B_filter))

sessionsA <- google_analytics(rollup, 
                              date_range = c(startDate, endDate), 
                              metrics = c("entrances"),
                              dimensions = c("landingPagePath"),
                              dim_filters = my_filter_A_clause,
                              anti_sample = TRUE,
                              max = -1)


conversionsA <- google_analytics(rollup, 
                                 date_range = c(startDate, endDate),
                                 metrics = "transactions",
                                 dimensions = "landingPagePath",
                                 dim_filters = my_filter_A_clause,
                                 anti_sample = TRUE,
                                 max = -1)

sessionsB <- google_analytics(rollup, 
                              date_range = c(startDate, endDate),
                              metrics = "entrances",
                              dimensions = "landingPagePath",
                              dim_filters = my_filter_B_clause,
                              anti_sample = TRUE,
                              max = -1)

conversionsB <- google_analytics(rollup, 
                                 date_range = c(startDate, endDate),
                                 metrics = "transactions",
                                 dimensions = "landingPagePath",
                                 dim_filters = my_filter_B_clause,
                                 anti_sample = TRUE,
                                 max = -1)

# A lower p-value gives you more confidence that there is 
# a real difference between the two pages (checking for p-values under 5% is typical)
prop.test(c(conversionsA$transactions, conversionsB$transactions), 
          c(sessionsA$entrances, sessionsB$entrances))


# Using the "power" of the test in this scenario 
# we can tell if we have enough data to just 
# call the current leader as the winner
# The output from this calculation tells us with one treatment converting 
# at 4% and the other at 5%, we need 2,324 observations per treatment (or 4,648 in total). The conventional approach to make the call based on 95% confidence requires a total of 18,598 observations before we can make the call on the test.

power.prop.test(n = NULL, 
                p1 = 0.04, 
                p2 = 0.05, 
                sig.level = 0.5, 
                power = .95, 
                alternative ="one.sided", 
                strict = FALSE)