library(RColorBrewer)
library(tidyverse)
library(viridis)
library(ggthemes)
library(googleAnalyticsR)
library(lubridate)
library(rpivotTable)
library(TTR)
library(googlesheets)
library(quantmod)
library(grid)
library(gridExtra)

#4.2. Reeceiving data from Google AdWords
#4.2.1. Describing the API query to Google AdWords.
body <- statement(select=c('CampaignId',
                           'Impressions',
                           'Clicks',
                           'Cost',
                           'Ctr',
                           'SearchBudgetLostImpressionShare',
                           'SearchRankLostImpressionShare  ',
                           'ContentBudgetLostImpressionShare',
                           'ContentRankLostImpressionShare'),
                  report="CAMPAIGN_PERFORMANCE_REPORT",
                  start=paste0(start_period["year"],start_period["month"],start_period["day"]),
                  end=paste0(end_period["year"],end_period["month"],end_period["day"]))

#4.2.2. Sending the query to Google AdWords
adwordsData <- getData(clientCustomerId = adwords_id,
                 google_auth = adwords_auth,
                 statement = body,
                 transformation = T,
                 apiVersion = "201509")

#5. Preparing the summary table.
#5.1. Combining data from Google Analytics and Google AdWords in one table
totalData <- merge(gaData, adwordsData, by.x = "adwordsCampaignID", by.y = "CampaignID", all.x = TRUE)

 #5.2. Replacing missed values with zeros
for (i in 1:length(totalData)){
  totalData[which(is.na(totalData[i])),i] <- 0
 }

 #5.3. Final calculations of the number of lost transactions and revenue.
totalData$lostImpressionByBudgetSearch  <- round(totalData$Impressions / (1-totalData$`SearchLostIS(budget)`) - totalData$Impressions,0)
totalData$lostImpressionByRankSearch    <- round(totalData$Impressions / (1-totalData$`SearchLostIS(rank)`) - totalData$Impressions,0)
totalData$lostImpressionByBudgetDisplay <- round(totalData$Impressions / (1-totalData$`ContentLostIS(budget)`) - totalData$Impressions,0)
totalData$lostImpressionByRankDisplay   <- round(totalData$Impressions / (1-totalData$`ContentLostIS(rank)`) - totalData$Impressions,0)
totalData$lostImpressionByBudget        <- totalData$lostImpressionByBudgetSearch + totalData$lostImpressionByBudgetDisplay
totalData$lostImpressionByRank          <- totalData$lostImpressionByRankSearch  + totalData$lostImpressionByRankDisplay
totalData$lostClicksByBudget            <- round(totalData$lostImpressionByBudget * (totalData$CTR),0)
totalData$lostClicksByRank              <- round(totalData$lostImpressionByRank * (totalData$CTR),0)
totalData$lostTransactionsByBudget      <- round(totalData$lostClicksByBudget * (totalData$transactions / totalData$Clicks),0)
totalData$lostTransactionsByRank        <- round(totalData$lostClicksByRank * (totalData$transactions / totalData$Clicks),0)
totalData$lostTransactions              <- totalData$lostTransactionsByBudget + totalData$lostTransactionsByRank
totalData$lostRevenueByBudget           <- round(totalData$lostTransactionsByBudget * (totalData$transactionRevenue / totalData$transactions),0)
totalData$lostRevenueByRank             <- round(totalData$lostTransactionsByRank * (totalData$transactionRevenue / totalData$transactions),0)
totalData$lostRevenue                   <- totalData$lostRevenueByBudget + totalData$lostRevenueByRank

#6. Unloading the calculated table in csv file
write.table(totalData, file='lostRevenue.csv', sep = ";", dec = ",", row.names = FALSE)
#7. Visualization in the form of a pie chart
lost_revenue <- c('received revenue' = sum(totalData$transactionRevenue), 'lost by budget' =sum(totalData$lostRevenueByBudget), 'lost by rank' = sum(totalData$lostRevenueByRank))
pie(lost_revenue,col = c("green", "red", "firebrick"))