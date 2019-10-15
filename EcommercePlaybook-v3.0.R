# install packages
install.packages(c("RGA","plyr","dplyr","sqldf","reshape","reshape2","tidyr","googlesheets","httpuv"))

# load library
# authenticate RGA and Google Sheet by clicking "Allow" when you see pop-up browser
library (RGA)
authorize()
library (plyr)
library (dplyr)
library (sqldf)
library (reshape)
library (reshape2)
library (tidyr)
library("googlesheets")
gs_auth()

# SPECIFY PARAMETER. Once done, run the code from here till the end  
# Fill in the GA view ID of your client

profile_id <- '87843854'

# Fill in the start & end date of less than 90 days since some questions will involve segment at users level 
# Choose the period when you have accurate data point

s_date_1 <- '2015-11-01'
e_date_1 <- '2015-12-31'
date_range <- seq(as.Date(s_date_1), as.Date(e_date_1), by = "days")

# Specify the key of your google sheet output file- You can find it in the URL as instructed
sheet <- gs_key('1gn8uptGVlMa6fk78NQotPw7tYpYye8OKJVyoj-BUl_c')

# Specify the percentile revenue cut-off for high/medium/low users

top_pct <- 0.80
bottom_pct <- 0.20

# SPECIFY PARAMETER DONE

# Q1.1: Overview of current status
# a. Monthly trend data

trend_raw <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                      metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                      dimensions = "ga:year, ga:month, ga:deviceCategory",
                      sort = "ga:year, ga:month, ga:deviceCategory",
                      filter = NULL,
                      samplingLevel = "higher_precision",
                      max.results = NULL)

colnames(trend_raw)[c(3,6)] <- c("device", "revenue")

# Aggregated KPIs

no_of_days <- difftime(e_date_1,s_date_1)

total_rev <- sum(trend_raw$revenue)

total_CR <- sum(trend_raw$transactions)/sum(trend_raw$sessions)

total_AOV <- sum(trend_raw$revenue)/sum(trend_raw$transactions)

total_sum <- sqldf('select sum(sessions) as sessions, sum(transactions) as transactions, sum(revenue) as revenue,
                   sum(revenue)/sum(transactions) as AOV
                   from trend_raw')

total_sum_final <- transform(total_sum, CR = transactions/sessions)


device_pct <- sqldf('select device,
                        sum(revenue) as revenue,
                        sum(transactions) as transactions,
                        sum(sessions) as sessions,
                        sum(revenue)/sum(transactions) as AOV,
                        sum(revenue)/(select sum(revenue) from trend_raw) as pct_rev
                        from trend_raw group by device ') 

device_pct_final <- transform(device_pct, CR = transactions/sessions, pct_sessions = sessions/sum(sessions), pct_transactions = transactions/sum(transactions)) 

# Trend by device

trend_1 <- trend_raw %>% group_by(year, month) %>% mutate(pct_rev = revenue/sum(revenue), pct_sessions = sessions/sum(sessions), CR = transactions/sessions, AOV = revenue/transactions)

trend_pct_sessions_final <- dcast(trend_1, year + month ~ device, value.var = "pct_sessions", fun.aggregate = sum)
trend_pct_cr_final <- dcast(trend_1, year + month ~ device, value.var = "CR", fun.aggregate = sum)
trend_pct_aov_final <- dcast(trend_1, year + month ~ device, value.var = "AOV", fun.aggregate = sum)


# b. Incremental revenue based on different scenarios
# Modify these parameter based on your needs

sce1 <- 0.02
sce2 <- 0.05
sce3 <- 0.10
sce4 <- 0.25

sce <- rbind(sce1, sce2, sce3, sce4)
sce_1 <- cbind(sce, total_sum_final$CR, total_sum_final$AOV, total_sum_final$sessions, total_sum_final$revenue)
colnames(sce_1)[c(1:5)] <- c("Sce_Imp","CR_Act","AOV_Act", "Sessions_Act","Rev_Act")
sce_final <- transform(sce_1, rev_sce = CR_Act*(1+Sce_Imp)*AOV_Act*Sessions_Act, rev_inc_sce = CR_Act*(1+Sce_Imp)*AOV_Act*Sessions_Act-Rev_Act)

# c. How many days does it take to make 1st purchase?

mcf_timelag <- get_mcf(profile_id, start.date=s_date_1, end.date=e_date_1,
                       metrics = "mcf:totalConversions, mcf:totalConversionValue",
                       dimensions = "mcf:timeLagInDaysHistogram",
                       filter = "mcf:conversionType==Transaction")

colnames(mcf_timelag)[c(1:3)] <- c("Time_Lags_In_Days", "Total_Conv", "Total_Revenue")

mcf_timelag_cum_final <- transform(mcf_timelag, cumulative_pct = round(cumsum(mcf_timelag$Total_Revenue)/sum(mcf_timelag$Total_Revenue),2))

# MCF Path Length Interaction

mcf_path_length <- get_mcf(profile_id, start.date=s_date_1, end.date=e_date_1,
                           metrics = "mcf:totalConversions, mcf:totalConversionValue",
                           dimensions = "mcf:pathLengthInInteractionsHistogram",
                           filter = "mcf:conversionType==Transaction")

colnames(mcf_path_length)[c(1:3)] <- c("Path_Length", "Total_Conv", "Total_Revenue")

mcf_path_length_cum_final <- transform(mcf_path_length, cumulative_pct = round(cumsum(mcf_path_length$Total_Revenue)/sum(mcf_path_length$Total_Revenue),2))

# Q1.2: Who are my low-, mid-, and high-value buyers (in a single session)? - Only focus on user for now

# Q1.3: Who are my low-, mid-, and high-value buyers (across sessions) - USERS?

# Define function to find the LOW revenue per user cut-off. Value stored in bottom_cut variable

bottom.pct.fun <- function(percentile, errorTerm = 0.01, startDate, endDate, viewID){
  
  # Get starting point
  usersTransaction <- get_ga(profileId = viewID, start.date = startDate, end.date = endDate,
         metrics = "ga:transactionRevenue" 
         #,dimensions = "NULL"
         )
  
  lol <- get_ga(profileId = viewID, start.date = startDate, end.date = endDate,
                                 metrics = "ga:users",
                                 #dimensions = "NULL",
                                 segment = 'users::condition::perUser::ga:transactionRevenue>0')
  
  # Get maximum no of users
  maxNoOfUsers <- lol[1, 1]
  
  # Getting starting point
  transactionValue <- usersTransaction[1, 1]
  transactionValue <- transactionValue/maxNoOfUsers
  
  # Percentile
  percentUsers <- 1 - percentile
  
  # Define current iteration
  currentPercentUsers <- 0
  
  # Error term
  errorTerm <- percentUsers - currentPercentUsers
  
  # Loop
  while(errorTerm >= 0.1){
    lol <- get_ga(profileId = viewID, start.date = startDate, end.date = endDate,
                                   metrics = "ga:users", 
                                   #dimensions = "NULL",
                                   segment = paste0('users::condition::perUser::ga:transactionRevenue>', transactionValue))
    currentPercentUsers <- lol[1, 1]/maxNoOfUsers
    print(currentPercentUsers)
    errorTerm <- percentUsers - currentPercentUsers
    print(errorTerm)
    print(transactionValue)
    bottom_cut <<- transactionValue
    transactionValue <- transactionValue + (transactionValue * -errorTerm)
  }
}

bottom.pct.fun(bottom_pct, errorTerm = 0.01, s_date_1, e_date_1, profile_id)

# Define function to find the TOP revenue per user cut-off. Value stored in top_cut variable

top.pct.fun <- function(percentile, errorTerm = 0.01, startDate, endDate, viewID){
  
  # Get starting point
  usersTransaction <- get_ga(profileId = viewID, start.date = startDate, end.date = endDate,
                             metrics = "ga:transactionRevenue" 
                             #,dimensions = "NULL"
  )
  
  lol <- get_ga(profileId = viewID, start.date = startDate, end.date = endDate,
                metrics = "ga:users",
                #dimensions = "NULL",
                segment = 'users::condition::perUser::ga:transactionRevenue>0')
  
  # Get maximum no of users
  maxNoOfUsers <- lol[1, 1]
  
  # Getting starting point
  transactionValue <- usersTransaction[1, 1]
  transactionValue <- transactionValue/maxNoOfUsers
  
  # Percentile
  percentUsers <- 1 - percentile
  
  # Define current iteration
  currentPercentUsers <- 0
  
  # Error term
  errorTerm <- percentUsers - currentPercentUsers
  
  # Loop
  while(errorTerm >= 0.1){
    lol <- get_ga(profileId = viewID, start.date = startDate, end.date = endDate,
                  metrics = "ga:users", 
                  #dimensions = "NULL",
                  segment = paste0('users::condition::perUser::ga:transactionRevenue>', transactionValue))
    currentPercentUsers <- lol[1, 1]/maxNoOfUsers
    print(currentPercentUsers)
    errorTerm <- percentUsers - currentPercentUsers
    print(errorTerm)
    print(transactionValue)
    top_cut <<- transactionValue
    transactionValue <- transactionValue + (transactionValue * -errorTerm)
  }
}

top.pct.fun(top_pct, errorTerm = 0.01, s_date_1, e_date_1, profile_id)

# combine 2 cut-off 

cut_off_final <- rbind(bottom_cut, top_cut)

# b. Create segment at user level based on the revenue cut off

SHVU <- paste('users::condition::perUser::ga:transactionRevenue>=', top_cut, sep='')

SMVU <- paste('users::condition::perUser::ga:transactionRevenue>=', bottom_cut ,';ga:transactionRevenue<', top_cut,sep='')

SLVU <- paste('users::condition::perUser::ga:transactionRevenue>0;ga:transactionRevenue<',bottom_cut, sep='')

# Get user segment key info

SHVU_info <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:sessions, ga:users, ga:transactions, ga:itemQuantity, ga:transactionRevenue", 
                    dimensions = "",
                    segment = paste(SHVU),
                    samplingLevel = "higher_precision",
                    max.results = NULL)

SMVU_info <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:sessions, ga:users, ga:transactions, ga:itemQuantity, ga:transactionRevenue", 
                    dimensions = "",
                    segment = paste(SMVU),
                    samplingLevel = "higher_precision",
                    max.results = NULL)

SLVU_info <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:sessions, ga:users, ga:transactions, ga:itemQuantity, ga:transactionRevenue", 
                    dimensions = "",
                    segment = paste(SLVU),
                    samplingLevel = "higher_precision",
                    max.results = NULL)

u_segment_info <- data.frame(rbind(SHVU_info, SMVU_info, SLVU_info))
u_segment_info_1 <- cbind(c("SHVU","SMVU","SLVU"), u_segment_info)
u_segment_info_final <- transform(u_segment_info_1, sessions_pct = sessions/sum(sessions), users_pct = users/sum(users),
                              revenue_pct = transactionRevenue/sum(transactionRevenue), trans_pct = transactions/sum(transactions), 
                              quantity_pct = itemQuantity/sum(itemQuantity))
 
colnames(u_segment_info_final)[c(1,5,6)] <- c("User_Value_Segment","Quantity","Revenue")

# c. Get top products bought by quantity for each user segment

# Customize the no of product to display

no_of_prod_display <- 5

Prod_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:itemRevenue, ga:itemQuantity, ga:revenuePerItem", 
                    dimensions = "ga:productName",
                    segment = paste(SHVU),
                    sort = "-ga:itemQuantity",
                    samplingLevel = "higher_precision",
                    max.results = NULL)

Prod_SHVU_1 <- cbind("SHVU",Prod_SHVU)
colnames(Prod_SHVU_1)[c(1:5)] <- c("User_Value_Segment","Product_Name","Revenue","Quantity","Ave_Price")
Prod_SHVU_2 <- transform(Prod_SHVU_1, segment_cr = Quantity/SHVU_info$sessions)

Prod_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:itemRevenue, ga:itemQuantity, ga:revenuePerItem", 
                    dimensions = "ga:productName",
                    segment = paste(SMVU),
                    sort = "-ga:itemQuantity",
                    samplingLevel = "higher_precision",
                    max.results = NULL)

Prod_SMVU_1 <- cbind("SMVU",Prod_SMVU)
colnames(Prod_SMVU_1)[c(1:5)] <- c("User_Value_Segment","Product_Name","Revenue","Quantity","Ave_Price")
Prod_SMVU_2 <- transform(Prod_SMVU_1, segment_cr = Quantity/SMVU_info$sessions)

Prod_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:itemRevenue, ga:itemQuantity, ga:revenuePerItem", 
                    dimensions = "ga:productName",
                    segment = paste(SLVU),
                    sort = "-ga:itemQuantity",
                    samplingLevel = "higher_precision",
                    max.results = NULL)

Prod_SLVU_1 <- cbind("SLVU",Prod_SLVU)
colnames(Prod_SLVU_1)[c(1:5)] <- c("User_Value_Segment","Product_Name","Revenue","Quantity","Ave_Price")
Prod_SLVU_2 <- transform(Prod_SLVU_1, segment_cr = Quantity/SLVU_info$sessions)

prod_u_segment_final <- rbind(head(Prod_SHVU_2,no_of_prod_display), head(Prod_SMVU_2,no_of_prod_display), head(Prod_SLVU_2,no_of_prod_display))

# d. What influences my high value buyers to buy?
# Time of day

Hour_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                    dimensions = "ga:hour",
                    segment = paste(SHVU),
                    samplingLevel = "higher_precision",
                    max.results = NULL)

Hour_SHVU_1 <- cbind("SHVU",Hour_SHVU)
colnames(Hour_SHVU_1)[c(1:5)] <- c("User_Value_Segment","Hour","Sessions","Transactions","Revenue")
Hour_SHVU_2 <- transform(Hour_SHVU_1, CR = Transactions/Sessions)

Hour_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                    dimensions = "ga:hour",
                    segment = paste(SMVU),
                    samplingLevel = "higher_precision",
                    max.results = NULL)

Hour_SMVU_1 <- cbind("SMVU",Hour_SMVU)
colnames(Hour_SMVU_1)[c(1:5)] <- c("User_Value_Segment","Hour","Sessions","Transactions","Revenue")
Hour_SMVU_2 <- transform(Hour_SMVU_1, CR = Transactions/Sessions)

Hour_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                    metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                    dimensions = "ga:hour",
                    segment = paste(SLVU),
                    samplingLevel = "higher_precision",
                    max.results = NULL)
  
Hour_SLVU_1 <- cbind("SLVU",Hour_SLVU)
colnames(Hour_SLVU_1)[c(1:5)] <- c("User_Value_Segment","Hour","Sessions","Transactions","Revenue")
Hour_SLVU_2 <- transform(Hour_SLVU_1, CR = Transactions/Sessions)

hour_u_segment <- rbind(Hour_SHVU_2, Hour_SMVU_2, Hour_SLVU_2)

hour_u_segment_1 <- hour_u_segment %>% group_by(Hour) %>% mutate(pct_sessions = Sessions / sum(Sessions))

hour_u_pct_sessions_final <- dcast(hour_u_segment_1, Hour ~ User_Value_Segment, value.var = "pct_sessions", fun.aggregate = sum)

hour_u_pct_cr_final <- dcast(hour_u_segment_1, Hour ~ User_Value_Segment, value.var = "CR", fun.aggregate = sum)  

hour_u_sessions <- dcast(hour_u_segment_1, Hour ~ User_Value_Segment, value.var = "Sessions", fun.aggregate = sum)
hour_u_sessions_final <- transform(hour_u_sessions, Total = SHVU + SMVU + SLVU)

# Age - Share of Sessions vs Revenue per visit

Age_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                   metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                   dimensions = "ga:userAgeBracket",
                   segment = paste(SHVU),
                   samplingLevel = "higher_precision",
                   max.results = NULL)
Age_SHVU_1 <- transform(Age_SHVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Age_SHVU_2 <- cbind("SHVU", Age_SHVU_1)
colnames(Age_SHVU_2)[c(1,2,5)] <- c("User_Value_Segment","Age_Bracket","Revenue")

Age_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                   metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                   dimensions = "ga:userAgeBracket",
                   segment = paste(SMVU),
                   samplingLevel = "higher_precision",
                   max.results = NULL)
Age_SMVU_1 <- transform(Age_SMVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Age_SMVU_2 <- cbind("SMVU", Age_SMVU_1)
colnames(Age_SMVU_2)[c(1,2,5)] <- c("User_Value_Segment","Age_Bracket","Revenue")

Age_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                   metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                   dimensions = "ga:userAgeBracket",
                   segment = paste(SLVU),
                   samplingLevel = "higher_precision",
                   max.results = NULL)
Age_SLVU_1 <- transform(Age_SLVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Age_SLVU_2 <- cbind("SLVU", Age_SLVU_1)
colnames(Age_SLVU_2)[c(1,2,5)] <- c("User_Value_Segment","Age_Bracket","Revenue")

age_u_segment <- rbind(Age_SHVU_2,Age_SMVU_2,Age_SLVU_2)

age_u_segment_1 <- age_u_segment %>% group_by(User_Value_Segment) %>% mutate(pct_sessions = sessions / sum(sessions))

# Share of sessions by age group for each user value segment

age_u_pct_sessions_final <- dcast(age_u_segment_1, Age_Bracket ~ User_Value_Segment, value.var = "pct_sessions", fun.aggregate = sum)  

# RPV for each user value segment

age_u_revenue_per_visit_final <- dcast(age_u_segment_1, Age_Bracket ~ User_Value_Segment, value.var = "revenue_per_visit", fun.aggregate = sum)  

# Gender

Gender_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                   metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                   dimensions = "ga:userGender",
                   segment = paste(SHVU),
                   samplingLevel = "higher_precision",
                   max.results = NULL)
Gender_SHVU_1 <- transform(Gender_SHVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Gender_SHVU_2 <- cbind("SHVU", Gender_SHVU_1)
colnames(Gender_SHVU_2)[c(1,2,5)] <- c("User_Value_Segment","Gender","Revenue")

Gender_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                   metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                   dimensions = "ga:userGender",
                   segment = paste(SMVU),
                   samplingLevel = "higher_precision",
                   max.results = NULL)
Gender_SMVU_1 <- transform(Gender_SMVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Gender_SMVU_2 <- cbind("SMVU", Gender_SMVU_1)
colnames(Gender_SMVU_2)[c(1,2,5)] <- c("User_Value_Segment","Gender","Revenue")

Gender_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                   metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                   dimensions = "ga:userGender",
                   segment = paste(SLVU),
                   samplingLevel = "higher_precision",
                   max.results = NULL)
Gender_SLVU_1 <- transform(Gender_SLVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Gender_SLVU_2 <- cbind("SLVU", Gender_SLVU_1)
colnames(Gender_SLVU_2)[c(1,2,5)] <- c("User_Value_Segment","Gender","Revenue")

Gender_u_segment <- rbind(Gender_SHVU_2,Gender_SMVU_2,Gender_SLVU_2)

Gender_u_segment_1 <- Gender_u_segment %>% group_by(User_Value_Segment) %>% mutate(pct_sessions = sessions / sum(sessions))

# Share of sessions by Gender group for each user value segment

Gender_u_pct_sessions_final <- dcast(Gender_u_segment_1, Gender ~ User_Value_Segment, value.var = "pct_sessions", fun.aggregate = sum)  

# RPV for each user value segment

Gender_u_revenue_per_visit_final <- dcast(Gender_u_segment_1, Gender ~ User_Value_Segment, value.var = "revenue_per_visit", fun.aggregate = sum)  

# Affinity

# The percentile level to cut-off sessions and rank the rest
aff_pct_lvl <- 50

# No of top results to display
aff_display <- 10

# SHVU  
Affinity_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                      metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                      dimensions = "ga:interestAffinityCategory",
                      segment = paste(SHVU),
                      samplingLevel = "higher_precision",
                      max.results = NULL)
Affinity_SHVU_1 <- transform(Affinity_SHVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Affinity_SHVU_2 <- cbind("SHVU", Affinity_SHVU_1)
colnames(Affinity_SHVU_2)[c(1,2,5)] <- c("User_Value_Segment","Affinity","Revenue")

aff_SHVU_sessions_ave <- mean(Affinity_SHVU_2$sessions)
aff_SHVU_sessions_pct <- quantile(Affinity_SHVU_2$sessions, probs = aff_pct_lvl/100, na.rm = TRUE)
aff_SHVU_revenue_per_visit_pct <- quantile(Affinity_SHVU_2$revenue_per_visit, probs = aff_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

Affinity_SHVU_3 <- sqldf(paste('select User_Value_Segment, Affinity, sessions, revenue_per_visit, transactions, Revenue, CR from Affinity_SHVU_2 where sessions >=', aff_SHVU_sessions_pct, 
                               ' and revenue_per_visit >=', aff_SHVU_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

Affinity_SHVU_4 <- cbind(Affinity_SHVU_3, aff_SHVU_sessions_pct, aff_SHVU_revenue_per_visit_pct)
colnames(Affinity_SHVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

Affinity_SHVU_final <- head(Affinity_SHVU_4, aff_display)

# SMVU
Affinity_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                        metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                        dimensions = "ga:interestAffinityCategory",
                        segment = paste(SMVU),
                        samplingLevel = "higher_precision",
                        max.results = NULL)
Affinity_SMVU_1 <- transform(Affinity_SMVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Affinity_SMVU_2 <- cbind("SMVU", Affinity_SMVU_1)
colnames(Affinity_SMVU_2)[c(1,2,5)] <- c("User_Value_Segment","Affinity","Revenue")

aff_SMVU_sessions_ave <- mean(Affinity_SMVU_2$sessions)
aff_SMVU_sessions_pct <- quantile(Affinity_SMVU_2$sessions, probs = aff_pct_lvl/100, na.rm = TRUE)
aff_SMVU_revenue_per_visit_pct <- quantile(Affinity_SMVU_2$revenue_per_visit, probs = aff_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

Affinity_SMVU_3 <- sqldf(paste('select User_Value_Segment, Affinity, sessions, revenue_per_visit, transactions, Revenue, CR from Affinity_SMVU_2 where sessions >=', aff_SMVU_sessions_pct, 
                               ' and revenue_per_visit >=', aff_SMVU_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

Affinity_SMVU_4 <- cbind(Affinity_SMVU_3, aff_SMVU_sessions_pct, aff_SMVU_revenue_per_visit_pct)
colnames(Affinity_SMVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

Affinity_SMVU_final <- head(Affinity_SMVU_4, aff_display)

# SLVU
Affinity_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                        metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                        dimensions = "ga:interestAffinityCategory",
                        segment = paste(SLVU),
                        samplingLevel = "higher_precision",
                        max.results = NULL)
Affinity_SLVU_1 <- transform(Affinity_SLVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Affinity_SLVU_2 <- cbind("SLVU", Affinity_SLVU_1)
colnames(Affinity_SLVU_2)[c(1,2,5)] <- c("User_Value_Segment","Affinity","Revenue")

aff_SLVU_sessions_ave <- mean(Affinity_SLVU_2$sessions)
aff_SLVU_sessions_pct <- quantile(Affinity_SLVU_2$sessions, probs = aff_pct_lvl/100, na.rm = TRUE)
aff_SLVU_revenue_per_visit_pct <- quantile(Affinity_SLVU_2$revenue_per_visit, probs = aff_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

Affinity_SLVU_3 <- sqldf(paste('select User_Value_Segment, Affinity, sessions, revenue_per_visit, transactions, Revenue, CR from Affinity_SLVU_2 where sessions >=', aff_SLVU_sessions_pct, 
                               ' and revenue_per_visit >=', aff_SLVU_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

Affinity_SLVU_4 <- cbind(Affinity_SLVU_3, aff_SLVU_sessions_pct, aff_SLVU_revenue_per_visit_pct)
colnames(Affinity_SLVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

Affinity_SLVU_final <- head(Affinity_SLVU_4, aff_display)

# -----

# In-market segment

# SHVU  
Inmarket_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                          metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                          dimensions = "ga:interestInMarketCategory",
                          segment = paste(SHVU),
                          samplingLevel = "higher_precision",
                          max.results = NULL)
Inmarket_SHVU_1 <- transform(Inmarket_SHVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Inmarket_SHVU_2 <- cbind("SHVU", Inmarket_SHVU_1)
colnames(Inmarket_SHVU_2)[c(1,2,5)] <- c("User_Value_Segment","Inmarket","Revenue")

Inmarket_SHVU_sessions_ave <- mean(Inmarket_SHVU_2$sessions)
Inmarket_SHVU_sessions_pct <- quantile(Inmarket_SHVU_2$sessions, probs = aff_pct_lvl/100, na.rm = TRUE)
Inmarket_SHVU_ave_revenue_per_visit_pct <- quantile(Inmarket_SHVU_2$revenue_per_visit, probs = aff_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

Inmarket_SHVU_3 <- sqldf(paste('select User_Value_Segment, Inmarket, sessions, revenue_per_visit, transactions, Revenue, CR from Inmarket_SHVU_2 where sessions >=', Inmarket_SHVU_sessions_pct, 
                               ' and revenue_per_visit >=', Inmarket_SHVU_ave_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

Inmarket_SHVU_4 <- cbind(Inmarket_SHVU_3, Inmarket_SHVU_sessions_pct, Inmarket_SHVU_ave_revenue_per_visit_pct)
colnames(Inmarket_SHVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

Inmarket_SHVU_final <- head(Inmarket_SHVU_4, aff_display)

# SMVU  
Inmarket_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                        metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                        dimensions = "ga:interestInMarketCategory",
                        segment = paste(SMVU),
                        samplingLevel = "higher_precision",
                        max.results = NULL)
Inmarket_SMVU_1 <- transform(Inmarket_SMVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Inmarket_SMVU_2 <- cbind("SMVU", Inmarket_SMVU_1)
colnames(Inmarket_SMVU_2)[c(1,2,5)] <- c("User_Value_Segment","Inmarket","Revenue")

Inmarket_SMVU_sessions_ave <- mean(Inmarket_SMVU_2$sessions)
Inmarket_SMVU_sessions_pct <- quantile(Inmarket_SMVU_2$sessions, probs = aff_pct_lvl/100, na.rm = TRUE)
Inmarket_SMVU_ave_revenue_per_visit_pct <- quantile(Inmarket_SMVU_2$revenue_per_visit, probs = aff_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

Inmarket_SMVU_3 <- sqldf(paste('select User_Value_Segment, Inmarket, sessions, revenue_per_visit, transactions, Revenue, CR from Inmarket_SMVU_2 where sessions >=', Inmarket_SMVU_sessions_pct, 
                               ' and revenue_per_visit >=', Inmarket_SMVU_ave_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

Inmarket_SMVU_4 <- cbind(Inmarket_SMVU_3, Inmarket_SMVU_sessions_pct, Inmarket_SMVU_ave_revenue_per_visit_pct)
colnames(Inmarket_SMVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

Inmarket_SMVU_final <- head(Inmarket_SMVU_4, aff_display)

# SLVU  
Inmarket_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                        metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                        dimensions = "ga:interestInMarketCategory",
                        segment = paste(SLVU),
                        samplingLevel = "higher_precision",
                        max.results = NULL)
Inmarket_SLVU_1 <- transform(Inmarket_SLVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
Inmarket_SLVU_2 <- cbind("SLVU", Inmarket_SLVU_1)
colnames(Inmarket_SLVU_2)[c(1,2,5)] <- c("User_Value_Segment","Inmarket","Revenue")

Inmarket_SLVU_sessions_ave <- mean(Inmarket_SLVU_2$sessions)
Inmarket_SLVU_sessions_pct <- quantile(Inmarket_SLVU_2$sessions, probs = aff_pct_lvl/100, na.rm = TRUE)
Inmarket_SLVU_ave_revenue_per_visit_pct <- quantile(Inmarket_SLVU_2$revenue_per_visit, probs = aff_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

Inmarket_SLVU_3 <- sqldf(paste('select User_Value_Segment, Inmarket, sessions, revenue_per_visit, transactions, Revenue, CR from Inmarket_SLVU_2 where sessions >=', Inmarket_SLVU_sessions_pct, 
                               ' and revenue_per_visit >=', Inmarket_SLVU_ave_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

Inmarket_SLVU_4 <- cbind(Inmarket_SLVU_3, Inmarket_SLVU_sessions_pct, Inmarket_SLVU_ave_revenue_per_visit_pct)
colnames(Inmarket_SLVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

Inmarket_SLVU_final <- head(Inmarket_SLVU_4, aff_display)

# --------
# Country & City
# no. of country to display
country_display <- 10

# SHVU  
Country_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                          metrics = "ga:sessions", 
                          dimensions = "ga:country",
                          segment = paste(SHVU),
                          sort = "-ga:sessions",
                          samplingLevel = "higher_precision",
                          max.results = NULL)

Country_SHVU_1 <- transform(Country_SHVU, pct_sessions = sessions/sum(sessions))
Country_SHVU_2 <- cbind("SHVU", Country_SHVU_1)
colnames(Country_SHVU_2)[1] <- "User_Value_Segment"

# SMVU
Country_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                       metrics = "ga:sessions", 
                       dimensions = "ga:country",
                       segment = paste(SMVU),
                       sort = "-ga:sessions",
                       samplingLevel = "higher_precision",
                       max.results = NULL)

Country_SMVU_1 <- transform(Country_SMVU, pct_sessions = sessions/sum(sessions))
Country_SMVU_2 <- cbind("SMVU", Country_SMVU_1)
colnames(Country_SMVU_2)[1] <- "User_Value_Segment"

# SLVU
Country_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                       metrics = "ga:sessions", 
                       dimensions = "ga:country",
                       segment = paste(SLVU),
                       sort = "-ga:sessions",
                       samplingLevel = "higher_precision",
                       max.results = NULL)

Country_SLVU_1 <- transform(Country_SLVU, pct_sessions = sessions/sum(sessions))
Country_SLVU_2 <- cbind("SLVU", Country_SLVU_1)
colnames(Country_SLVU_2)[1] <- "User_Value_Segment"

Country_SHVU_top <- head(Country_SHVU_2, country_display)

country_SHVU_compare <- sqldf('select a.User_Value_Segment, a.country, a.sessions, a.pct_sessions as pct_SHVU_sessions , b.pct_sessions as pct_SMVU_sessions from Country_SHVU_top as a left join Country_SMVU_2 as b
                              on a.country = b.country')

country_SHVU_compare_final <- sqldf('select a.User_Value_Segment, a.country, a.sessions, a.pct_SHVU_sessions, a.pct_SMVU_sessions, b.pct_sessions as pct_SLVU_sessions from country_SHVU_compare as a left join Country_SLVU_2 as b
                              on a.country = b.country')
# --------
# Device
  
# SHVU  
Device_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                          metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                          dimensions = "ga:deviceCategory",
                          segment = paste(SHVU),
                          samplingLevel = "higher_precision",
                          max.results = NULL)
Device_SHVU_1 <- cbind("SHVU", Device_SHVU)
colnames(Device_SHVU_1)[c(1,2,5)] <- c("User_Value_Segment","Device","Revenue")
Device_SHVU_2 <- transform(Device_SHVU_1, pct_sessions = sessions/sum(sessions), pct_transactions = transactions/sum(transactions))

# SMVU  
Device_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                      metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                      dimensions = "ga:deviceCategory",
                      segment = paste(SMVU),
                      samplingLevel = "higher_precision",
                      max.results = NULL)
Device_SMVU_1 <- cbind("SMVU", Device_SMVU)
colnames(Device_SMVU_1)[c(1,2,5)] <- c("User_Value_Segment","Device","Revenue")
Device_SMVU_2 <- transform(Device_SMVU_1, pct_sessions = sessions/sum(sessions), pct_transactions = transactions/sum(transactions))

# SLVU  
Device_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                      metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                      dimensions = "ga:deviceCategory",
                      segment = paste(SLVU),
                      samplingLevel = "higher_precision",
                      max.results = NULL)
Device_SLVU_1 <- cbind("SLVU", Device_SLVU)
colnames(Device_SLVU_1)[c(1,2,5)] <- c("User_Value_Segment","Device","Revenue")
Device_SLVU_2 <- transform(Device_SLVU_1, pct_sessions = sessions/sum(sessions), pct_transactions = transactions/sum(transactions))

device_u_segment <- rbind(Device_SHVU_2, Device_SMVU_2, Device_SLVU_2)

device_u_pct_sessions_final <- dcast(device_u_segment, Device ~ User_Value_Segment , value.var = "pct_sessions", fun.aggregate = sum)
device_u_pct_transactions_final <- dcast(device_u_segment, Device ~ User_Value_Segment , value.var = "pct_transactions", fun.aggregate = sum)

# ----------
# Source/medium

# The percentile level to cut-off sessions and rank the rest
SM_pct_lvl <- 70

# No of top results to display
SM_display <- 10

# SHVU  
SM_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                        metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                        dimensions = "ga:sourceMedium",
                        segment = paste(SHVU),
                        sort = "-ga:sessions",
                        samplingLevel = "higher_precision",
                        max.results = NULL)

SM_SHVU_1 <- transform(SM_SHVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
SM_SHVU_2 <- cbind("SHVU", SM_SHVU_1)
colnames(SM_SHVU_2)[c(1,2,5)] <- c("User_Value_Segment","Source_Medium","Revenue")

SM_SHVU_sessions_ave <- mean(SM_SHVU_2$sessions)
SM_SHVU_sessions_pct <- quantile(SM_SHVU_2$sessions, probs = SM_pct_lvl/100, na.rm = TRUE)
SM_SHVU_revenue_per_visit_pct <- quantile(SM_SHVU_2$revenue_per_visit, probs = SM_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

SM_SHVU_3 <- sqldf(paste('select User_Value_Segment, Source_Medium, sessions, revenue_per_visit, transactions, Revenue, CR from SM_SHVU_2 where sessions >=', SM_SHVU_sessions_pct, 
                               ' and revenue_per_visit >=', SM_SHVU_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

SM_SHVU_4 <- cbind(SM_SHVU_3, SM_SHVU_sessions_pct, SM_SHVU_revenue_per_visit_pct)
colnames(SM_SHVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

SM_SHVU_final <- head(SM_SHVU_4, SM_display)

# SMVU  
SM_SMVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                  metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                  dimensions = "ga:sourceMedium",
                  segment = paste(SMVU),
                  sort = "-ga:sessions",
                  samplingLevel = "higher_precision",
                  max.results = NULL)

SM_SMVU_1 <- transform(SM_SMVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
SM_SMVU_2 <- cbind("SMVU", SM_SMVU_1)
colnames(SM_SMVU_2)[c(1,2,5)] <- c("User_Value_Segment","Source_Medium","Revenue")

SM_SMVU_sessions_ave <- mean(SM_SMVU_2$sessions)
SM_SMVU_sessions_pct <- quantile(SM_SMVU_2$sessions, probs = SM_pct_lvl/100, na.rm = TRUE)
SM_SMVU_revenue_per_visit_pct <- quantile(SM_SMVU_2$revenue_per_visit, probs = SM_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

SM_SMVU_3 <- sqldf(paste('select User_Value_Segment, Source_Medium, sessions, revenue_per_visit, transactions, Revenue, CR from SM_SMVU_2 where sessions >=', SM_SMVU_sessions_pct, 
                         ' and revenue_per_visit >=', SM_SMVU_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

SM_SMVU_4 <- cbind(SM_SMVU_3, SM_SMVU_sessions_pct, SM_SMVU_revenue_per_visit_pct)
colnames(SM_SMVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

SM_SMVU_final <- head(SM_SMVU_4, SM_display)

# SLVU  
SM_SLVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                  metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                  dimensions = "ga:sourceMedium",
                  segment = paste(SLVU),
                  sort = "-ga:sessions",
                  samplingLevel = "higher_precision",
                  max.results = NULL)

SM_SLVU_1 <- transform(SM_SLVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
SM_SLVU_2 <- cbind("SLVU", SM_SLVU_1)
colnames(SM_SLVU_2)[c(1,2,5)] <- c("User_Value_Segment","Source_Medium","Revenue")

SM_SLVU_sessions_ave <- mean(SM_SLVU_2$sessions)
SM_SLVU_sessions_pct <- quantile(SM_SLVU_2$sessions, probs = SM_pct_lvl/100, na.rm = TRUE)
SM_SLVU_revenue_per_visit_pct <- quantile(SM_SLVU_2$revenue_per_visit, probs = SM_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

SM_SLVU_3 <- sqldf(paste('select User_Value_Segment, Source_Medium, sessions, revenue_per_visit, transactions, Revenue, CR from SM_SLVU_2 where sessions >=', SM_SLVU_sessions_pct, 
                         ' and revenue_per_visit >=', SM_SLVU_revenue_per_visit_pct, ' order by revenue_per_visit desc', sep=''))

SM_SLVU_4 <- cbind(SM_SLVU_3, SM_SLVU_sessions_pct, SM_SLVU_revenue_per_visit_pct)
colnames(SM_SLVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

SM_SLVU_final <- head(SM_SLVU_4, SM_display)

# ------------
# Landing pages

# The percentile level to cut-off sessions and rank the rest
LP_sessions_pct_lvl <- 95
LP_revenue_per_sessions_pct_lvl <-95

# No of top results to display
LP_display <- 10

# Page to filter out - TBD

# SHVU  
LP_SHVU <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                  metrics = "ga:sessions, ga:transactions, ga:transactionRevenue", 
                  dimensions = "ga:landingPagePath",
                  segment = paste(SHVU),
                  sort = "-ga:sessions",
                  filter = "ga:landingPagePath!~error404",
                  samplingLevel = "higher_precision",
                  max.results = NULL)

LP_SHVU_1 <- transform(LP_SHVU, CR = transactions/sessions, revenue_per_visit = transactionRevenue/sessions)
LP_SHVU_2 <- cbind("SHVU", LP_SHVU_1)
colnames(LP_SHVU_2)[c(1,2,5)] <- c("User_Value_Segment","Landing_Page","Revenue")

LP_SHVU_sessions_ave <- mean(LP_SHVU_2$sessions)
LP_SHVU_sessions_pct <- quantile(LP_SHVU_2$sessions, probs = LP_sessions_pct_lvl/100, na.rm = TRUE)
LP_SHVU_revenue_per_visit_pct <- quantile(LP_SHVU_2$revenue_per_visit, probs = LP_revenue_per_sessions_pct_lvl/100, na.rm = TRUE)

# Filter row with more than 50th percentile no. of sessions and revenue_per_visit

LP_SHVU_3 <- sqldf(paste('select User_Value_Segment, Landing_Page, sessions, revenue_per_visit, transactions, Revenue, CR from LP_SHVU_2 where sessions >=', LP_SHVU_sessions_pct, 
                         ' and revenue_per_visit >=', LP_SHVU_revenue_per_visit_pct, ' order by sessions desc', sep=''))

LP_SHVU_4 <- cbind(LP_SHVU_3, LP_SHVU_sessions_pct, LP_SHVU_revenue_per_visit_pct)
colnames(LP_SHVU_4)[c(8,9)] <- c("Ave_sessions","Ave_revenue_per_visit")

LP_SHVU_final <- head(LP_SHVU_4, LP_display)

# --------
# Remarket to users

# Top no of products to analyse for SHVU
  
remarket_no_prod <- 15

rmrk_top_SHVU <- head(Prod_SHVU, remarket_no_prod)
colnames(rmrk_top_SHVU)[1] <- "product_name"
rmrk_top_SMVU <- head(Prod_SMVU, remarket_no_prod)
colnames(rmrk_top_SMVU)[1] <- "product_name"

top_prod_list <- sqldf("select product_name from rmrk_top_SHVU union select product_name from rmrk_top_SMVU ")

top_prod_list_1 <- sqldf("select distinct(product_name) from top_prod_list")

prod_funnel <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                  metrics = "ga:productListViews, ga:productDetailViews, ga:productAddsToCart, ga:productCheckouts, ga:uniquePurchases", 
                  dimensions = "ga:productName",
                  sort = "-ga:productListViews, -ga:productDetailViews",
                  filter = NULL,
                  samplingLevel = "higher_precision",
                  max.results = NULL)

colnames(prod_funnel)[c(1:6)] <- c("product", "product_list_views", "product_detail_views", "add_to_cart", "check_out", "purchases")

# rank by product detail views
prod_funnel_1 <- sqldf("select * from prod_funnel order by product_detail_views desc") 

# merge with top product SHVU

top_prod_funnel_SHVU <- sqldf("select a.Product_Name, a.Ave_Price, b.product_list_views, b.product_detail_views, b.add_to_cart, b.check_out, b.purchases from Prod_SHVU_1 as a left join prod_funnel as b on 
                              a.Product_Name = b.product where b.product_detail_views > 0")

top_prod_funnel_SHVU_2 <- head(top_prod_funnel_SHVU, remarket_no_prod)

# add in funnel %
top_prod_funnel_SHVU_3 <- transform(top_prod_funnel_SHVU_2, Buy_2_Detail=purchases/product_detail_views , Buy_2_Cart=purchases/add_to_cart, Buy_2_Checkout=purchases/check_out)

Target_CPA_Remarketing <- transform(top_prod_funnel_SHVU_3, Viewed_Prod_CPA = Ave_Price*Buy_2_Detail,
                                    Add_to_Cart_CPA = Ave_Price*Buy_2_Cart,
                                    Checkout_CPA = Ave_Price*Buy_2_Checkout)

Target_CPA_Remarketing_Final <- Target_CPA_Remarketing[c(1,2,11,12,13)] 

# -------
# Cross product analysis - TBD. Export of trans_prod might be huge hence will be sampled per each fetch. For normal client, should we export to csv in the standard format then import here

no_of_rules_display <- 20

# Export - walk by date
trans_prod <- do.call(rbind, lapply(date_range,function(d){get_ga(profileId = profile_id, start.date = d, end.date = d,
                                                                 metrics = "ga:itemQuantity", 
                                                                 dimensions = "ga:transactionId, ga:productName",
                                                                 filter = NULL,
                                                                 samplingLevel = "higher_precision",
                                                                 max.results = NULL)
}))

colnames(trans_prod)[c(1:2)] <- c("transID","product")

# # Export - Normal - Sample
# trans_prod <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
#                      metrics = "ga:itemQuantity", 
#                      dimensions = "ga:transactionId, ga:productName",
#                      sort = "-ga:itemQuantity",
#                      filter = NULL,
#                      samplingLevel = "higher_precision",
#                      max.results = NULL) 


#Number of transaction per products
tr_per_prod <- sqldf('select product, count(distinct transID) as nb_trans 
                     from trans_prod
                     group by product')

#Cross product analysis 
cross <- sqldf('select p1.transID, p1.product as p1, p2.product as p2
               from trans_prod as p1, trans_prod as p2 on p1.transID=p2.transID')

cross2 <- sqldf('select p1, p2, count(distinct transID) as nb_cross_trans
                from cross
                group by p1, p2')

cross3 <- sqldf('select c.p1, c.p2, c.nb_cross_trans, t.nb_trans as p1_trans
                from cross2 as c left join tr_per_prod as t on c.p1=t.product')

cross4 <- sqldf('select c.p1, c.p2, c.nb_cross_trans, c.p1_trans, t.nb_trans as p2_trans
                from cross3 as c left join tr_per_prod as t on c.p2=t.product')

#Nb total of users
nb_total_trans <- sqldf('select count(distinct transID) as nb_total_trans
                       from trans_prod')

#Support, confidence, lift computation
#p1_support: prob of ppl buying p1 overall population
#confidence: conditional prob of ppl buying p1 given that they buy p2
support <- sqldf('select p1, p2, nb_total_trans, nb_cross_trans, p1_trans, p2_trans,
                 (cast(p1_trans as real)/nb_total_trans) as support_p1,
                 (cast(p2_trans as real)/nb_total_trans) as support_p2,
                 (cast(nb_cross_trans as real)/nb_total_trans) as support_p1_p2
                 from cross4, nb_total_trans')

# Currently this file might be too big to push to sheet. So will write to csv to refer to as appendix & push only top xx rules
cross_product_analysis_full_final <- sqldf('select A.*, 
                                A.support_p1_p2/A.support_p1 as confidence_p2,
                                A.support_p1_p2/A.support_p2 as confidence_p1,
                                A.support_p1_p2/(A.support_p1*A.support_p2) as lift
                                from support as A WHERE p1 <> p2 ORDER BY lift DESC
                                ')
# how about mean instead
ave_cross_trans <- mean(cross_product_analysis_full_final$nb_cross_trans, na.rm = FALSE)
ave_lift <- mean(cross_product_analysis_full_final$lift, na.rm = FALSE)

# These 
cross_product_analysis_top_final <- sqldf(paste('select p1, p2, p1_trans, p2_trans, nb_cross_trans, lift from cross_product_analysis_full_final where nb_cross_trans >= ',ave_cross_trans, 
                                          ' and lift >= ', ave_lift, sep=''))

cross_product_analysis_display_final <- head(cross_product_analysis_top_final, no_of_rules_display)

# --------
# Checkout funnel step - Need to break it down by device category - TBD

checkout_raw <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                                      metrics = "ga:sessions", 
                                      dimensions = "ga:shoppingStage",
                                      sort = "-ga:sessions",
                                      filter = NULL,
                                      samplingLevel = "higher_precision",
                                      max.results = NULL)

# State - the drop off = Stage_Next <= Stage - Since there can be additional traffic going directly there. This is prone to the no. of checkout step they have - How to make this dynamic

ALL_VISITS_NEXT <- data.frame(shopping.stage = 'ALL_VISITS_NEXT', sessions = checkout_raw[checkout_raw$shopping.stage == 'ALL_VISITS',2] - checkout_raw[checkout_raw$shopping.stage == 'NO_SHOPPING_ACTIVITY',2])
PRODUCT_VIEW_NEXT <- data.frame(shopping.stage = 'PRODUCT_VIEW_NEXT', sessions = checkout_raw[checkout_raw$shopping.stage == 'PRODUCT_VIEW',2] - checkout_raw[checkout_raw$shopping.stage == 'NO_CART_ADDITION',2])
ADD_TO_CART_NEXT <- data.frame(shopping.stage = 'ADD_TO_CART_NEXT', sessions = checkout_raw[checkout_raw$shopping.stage == 'ADD_TO_CART',2] - checkout_raw[checkout_raw$shopping.stage == 'CART_ABANDONMENT',2])
CHECKOUT_NEXT <- data.frame(shopping.stage = 'CHECKOUT_NEXT', sessions = checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT',2] - checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_ABANDONMENT',2])
CHECKOUT_1_NEXT <- data.frame(shopping.stage = 'CHECKOUT_1_NEXT', sessions = checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_1',2] - checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_1_ABANDONMENT',2])
CHECKOUT_2_NEXT <- data.frame(shopping.stage = 'CHECKOUT_2_NEXT', sessions = checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_2',2] - checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_2_ABANDONMENT',2])

checkout_raw_2 <- rbind(checkout_raw, ALL_VISITS_NEXT, PRODUCT_VIEW_NEXT, ADD_TO_CART_NEXT, CHECKOUT_NEXT, CHECKOUT_1_NEXT, CHECKOUT_2_NEXT)

# Get the completion rate and drop off
ALL_VISITS_2_PRODUCT_VIEW <- data.frame(checkout_funnel = 'ALL_VISITS_2_PRODUCT_VIEW', 
                                        completion_rate = checkout_raw_2[checkout_raw_2$shopping.stage == 'ALL_VISITS_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'ALL_VISITS',2],
                                        dropoff_rate = 1- checkout_raw_2[checkout_raw_2$shopping.stage == 'ALL_VISITS_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'ALL_VISITS',2])

PRODUCT_VIEW_2_ADD_TO_CART <- data.frame(checkout_funnel = 'PRODUCT_VIEW_2_ADD_TO_CART', 
                                         completion_rate = checkout_raw_2[checkout_raw_2$shopping.stage == 'PRODUCT_VIEW_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'PRODUCT_VIEW',2],
                                         dropoff_rate = 1- checkout_raw_2[checkout_raw_2$shopping.stage == 'PRODUCT_VIEW_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'PRODUCT_VIEW',2])

ADD_TO_CART_2_CHECKOUT <- data.frame(checkout_funnel = 'ADD_TO_CART_2_CHECKOUT', 
                                     completion_rate = checkout_raw_2[checkout_raw_2$shopping.stage == 'ADD_TO_CART_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'ADD_TO_CART',2],
                                     dropoff_rate = 1- checkout_raw_2[checkout_raw_2$shopping.stage == 'ADD_TO_CART_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'ADD_TO_CART',2])

CHECKOUT_2_CHECKOUT_1 <- data.frame(checkout_funnel = 'CHECKOUT_2_CHECKOUT_1', 
                                    completion_rate = checkout_raw_2[checkout_raw_2$shopping.stage == 'CHECKOUT_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT',2],
                                    dropoff_rate = 1- checkout_raw_2[checkout_raw_2$shopping.stage == 'CHECKOUT_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT',2])

CHECKOUT_1_2_CHECKOUT_2 <- data.frame(checkout_funnel = 'CHECKOUT_1_2_CHECKOUT_2', 
                                      completion_rate = checkout_raw_2[checkout_raw_2$shopping.stage == 'CHECKOUT_1_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_1',2],
                                      dropoff_rate = 1- checkout_raw_2[checkout_raw_2$shopping.stage == 'CHECKOUT_1_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_1',2])   


CHECKOUT_2_2_TRANSACTION <- data.frame(checkout_funnel = 'CHECKOUT_2_2_TRANSACTION', 
                                      completion_rate = checkout_raw_2[checkout_raw_2$shopping.stage == 'CHECKOUT_2_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_2',2],
                                      dropoff_rate = 1- checkout_raw_2[checkout_raw_2$shopping.stage == 'CHECKOUT_2_NEXT',2]/checkout_raw[checkout_raw$shopping.stage == 'CHECKOUT_2',2])

checkout_funnel_combined <- rbind(ALL_VISITS_2_PRODUCT_VIEW, PRODUCT_VIEW_2_ADD_TO_CART, ADD_TO_CART_2_CHECKOUT, CHECKOUT_2_CHECKOUT_1, CHECKOUT_1_2_CHECKOUT_2, CHECKOUT_2_2_TRANSACTION)

checkout_funnel_combined_final <- sqldf("select * from checkout_funnel_combined order by dropoff_rate desc ")

# Onsite search
no_of_top_search <- 25

# mark up level from average no
pct_higher_than_ave_exit_rate <- 0.1
pct_higher_than_ave_result_pv <- 0.1
pct_higher_than_ave_search_value <- 0.1
no_of_top_search_value_user <- 25

search_raw <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                     metrics = "ga:searchUniques, ga:avgSearchResultViews, ga:searchExits, ga:transactions, ga:transactionRevenue, ga:revenuePerTransaction", 
                     dimensions = "ga:searchKeyword",
                     sort = "-ga:searchUniques",
                     filter = NULL,
                     samplingLevel = "higher_precision",
                     max.results = NULL)

if(is.null(nrow(search_raw)) == TRUE) {} else {
  
  colnames(search_raw)[c(1:7)] <- c("keyword", "unique_searches", "avg_search_result_views", "search_exit", "transactions", "revenue", "aov")
  
  search_raw_2 <- transform(search_raw, CR = transactions/unique_searches, search_exit_rate = search_exit/unique_searches, per_search_value = aov*transactions/unique_searches)
  
  ave_no_search <- mean(search_raw_2$unique_searches, na.rm = FALSE)
  ave_search_exit_rate <- sum(search_raw_2$search_exit)/sum(search_raw_2$unique_searches)
  ave_result_PV_per_search <- mean(search_raw_2$avg_search_result_views, na.rm = FALSE)
  ave_per_search_value <- sum(search_raw_2$revenue)/sum(search_raw_2$unique_searches)
  
  # top kw search with high exit rate
  
  top_search_exit_rate <- sqldf(paste('select keyword, unique_searches, search_exit_rate from search_raw_2 where search_exit_rate >= ', ave_search_exit_rate*(1+pct_higher_than_ave_exit_rate), sep=''))
  top_search_exit_rate_1 <- cbind(top_search_exit_rate, ave_search_exit_rate)
  top_search_exit_rate_final <- head(top_search_exit_rate_1, no_of_top_search)
  
  # top kw search with high results PV
  top_search_results_pv <- sqldf(paste('select keyword, unique_searches, avg_search_result_views  from search_raw_2 where avg_search_result_views >= ', ave_result_PV_per_search*(1+pct_higher_than_ave_result_pv), sep=''))
  top_search_results_pv_1 <- cbind(top_search_results_pv, ave_result_PV_per_search)
  top_search_results_pv_final <- head(top_search_results_pv_1, no_of_top_search)
  
  # top kw search with high per search value
  top_search_value <- sqldf(paste('select keyword, unique_searches, per_search_value  from search_raw_2 where per_search_value >= ', ave_per_search_value*(1+pct_higher_than_ave_search_value), sep=''))
  top_search_value_1 <- cbind(top_search_value, ave_per_search_value)
  top_search_value_final <- head(top_search_value_1, no_of_top_search)
  
  # SHVU
  
  search_SHVU_raw <- get_ga(profileId = profile_id, start.date = s_date_1, end.date = e_date_1,
                            metrics = "ga:searchUniques", 
                            dimensions = "ga:searchKeyword",
                            sort = "-ga:searchUniques",
                            segment = paste(SHVU),
                            filter = NULL,
                            samplingLevel = "higher_precision",
                            max.results = NULL)
  
  # top xx kw search of SHVU
  search_SHVU_final <- head(search_SHVU_raw, no_of_top_search_value_user)
  }

# Q6.1 - Write all data frame to sheet

tab1 <- '6.1.1 - Landscape'
dfset_1 <- list(total_sum_final, no_of_days, device_pct_final, sce_final, mcf_timelag_cum_final, mcf_path_length_cum_final, trend_pct_sessions_final, trend_pct_cr_final, trend_pct_aov_final)
Anchor_1 <- c("A2", "G2", "A6", "A18", "A25", "G25", "M25", "S25", "Y25")

i = 1
while(i <= length(dfset_1)){
  gs_edit_cells(sheet, tab1, dfset_1[[i]], Anchor_1[i])
  i <- i + 1
}

# Q6.1.3 - All data frame to sheet - The current reference will break if changing the no of items display

tab2 <- '6.1.3 - Value User'
dfset_2 <- list(u_segment_info_final, cut_off_final, age_u_pct_sessions_final, age_u_revenue_per_visit_final, Gender_u_pct_sessions_final, Gender_u_revenue_per_visit_final,
                device_u_pct_sessions_final, device_u_pct_transactions_final, prod_u_segment_final, hour_u_pct_sessions_final, hour_u_pct_cr_final, hour_u_sessions_final, Affinity_SHVU_final, Affinity_SMVU_final, 
                Affinity_SLVU_final, Inmarket_SHVU_final, Inmarket_SMVU_final, Inmarket_SLVU_final, SM_SHVU_final, SM_SMVU_final, SM_SLVU_final, 
                country_SHVU_compare_final, LP_SHVU_final)

Anchor_2 <- c("A2", "A8", "M2", "R2", "W2", "W6", "AB2", "AB7", "A13", "H13", "M13", "R13", "X13", "AH13", "AR13", "A91", "K91", "U91", "A141", "K141", "U141", "A191", "H191")

i = 1
while(i <= length(dfset_2)){
  gs_edit_cells(sheet, tab2, dfset_2[[i]], Anchor_2[i])
  i <- i + 1
}

# Q6.2 - All data frame to sheet 

tab3 <- '6.2.2 - RemarketingCPA'
dfset_3 <- list(Target_CPA_Remarketing_Final)

Anchor_3 <- c("A2")

i = 1
while(i <= length(dfset_3)){
  gs_edit_cells(sheet, tab3, dfset_3[[i]], Anchor_3[i])
  i <- i + 1
}


# Q6.3 - All data frame to sheet 

tab4 <- '6.3.1 - Cross Product'
dfset_4 <- list(cross_product_analysis_display_final)

Anchor_4 <- c("A2")

i = 1
while(i <= length(dfset_4)){
  gs_edit_cells(sheet, tab4, dfset_4[[i]], Anchor_4[i])
  i <- i + 1
}

#Q6.4 - All data frame to sheet

tab5 <- '6.4 - UI/UX'

dfset_5 <- list(checkout_funnel_combined_final)

Anchor_5 <- c("A2")

i = 1
while(i <= length(dfset_5)){
  gs_edit_cells(sheet, tab5, dfset_5[[i]], Anchor_5[i])
  i <- i + 1
}
                                                                

# Q6.4 - All data frame to sheet
  
dfset_6 <- list(top_search_exit_rate_final, top_search_results_pv_final, top_search_value_final, search_SHVU_final)
  
Anchor_6 <- c("E2", "I2", "M2", "Q2")
  
  i = 1
  while(i <= length(dfset_6)){
    gs_edit_cells(sheet, tab5, dfset_6[[i]], Anchor_6[i])
    i <- i + 1
  } 
  
  


# THE END

