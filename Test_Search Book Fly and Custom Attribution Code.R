library(tidyverse)
library (tidyr)
library (bigrquery)
library (sqldf)
library (zoo)
library(imputeTS)
library(data.table)
library (reshape2)

# Big Query Setting
project <- "airnz-ga-bigquery"
dataset <- "125557395"

# IMPORTANT: Refer to the BigQuery code here: https://drive.google.com/drive/folders/0B_6oGZcbKKqhS2xBWDN1Z0RuMDA for more context on how the table below are being produced
# After you have saved your visit and booking history data set in BigQuery
# These are the query to pulling out all of those tables into R for further data manipulation
visitQuery1 <- "SELECT * FROM [103502525.Correct_Visit_20160915_20161014]"
visitQuery2 <- "SELECT * FROM [103502525.Correct_Visit_20161015_20161114]"
visitQuery3 <- "SELECT * FROM [103502525.Correct_Visit_20161115_20170114]"
visitQuery4 <- "SELECT * FROM [103502525.Correct_Visit_20170115_20170215]"
transDataQuery <- "SELECT * FROM [103502525.Booking_20160915_20170215]"
transRevQuery <- "SELECT * FROM [103502525.BookingRevenue_20160915_20170215]"

# Run the query to extract data into R
# options("httr_oob_default" = TRUE)
visitData1 <- query_exec(visitQuery1, project, destination_table = NULL, max_pages = Inf)
visitData2 <- query_exec(visitQuery2, project, destination_table = NULL, max_pages = Inf)
visitData3 <- query_exec(visitQuery3, project, destination_table = NULL, max_pages = Inf)
visitData4 <- query_exec(visitQuery4, project, destination_table = NULL, max_pages = Inf)
transData <- query_exec(transDataQuery, project, destination_table = NULL, max_pages = Inf)
transRevenue <- query_exec(transRevQuery, project, destination_table = NULL, max_pages = Inf)

# Later we want to bucket no. of days into week ranges to aggregate and visualise. 
# Create the bucket mapping file
bucket <- data.frame(c(0:500))
colnames(bucket)[1] <- "lag"
bucket1 <- transform(bucket, weekNo = lag%/%7+1)
bucket2 <- transform(bucket1, bk1 = ifelse(weekNo <= 2, "Wk1-2",ifelse(weekNo <= 7, "W3-7",ifelse(weekNo <= 12, "W8-12","3Mths+"))))
bucket3 <- transform(bucket2, bk2 = ifelse(weekNo <= 2, "Wk1-2",ifelse(weekNo <= 7, "W3-7","2Mths+")))
bucket4 <- transform(bucket3, bk3 = ifelse(weekNo <= 2, "Wk1-2","2Wks+"))
bucket4 <- transform(bucket4, bk4 = ifelse(weekNo <= 12, paste0("Wk",as.character(weekNo)),"3Mths+"))

sourcefile <- load("visitData1.RData")
sourcefile <- load("visitData2.RData")
sourcefile <- load("visitData3.RData")
sourcefile <- load("visitData4.RData")
sourcefile <- load("transData.RData")
sourcefile <- load("transRevenue.RData")

# (Optional) If the channel grouping of your client is not clean. 
# You can discuss with client to create a channel mapping file then load into R to use
channelMap <- read.csv('/home/analyst2/channelMap.csv')
colnames(channelMap)[4] <- "ChannelCat3"

# If you already extract your revenue data along with the main trans info, then ignore this part 
transRevenue$vid <- as.character(transRevenue$vid)
transRevenue$sid <- as.character(transRevenue$sid)
# This is to create the proper session ID to merge with other dataset
transRevenue <- transform(transRevenue, sid = paste(vid,sid,sep="-"))
# formate date properly
transRevenue$date <- strptime(transRevenue$date,format="%Y%m%d")
transRevenue$date <- as.Date(transRevenue$date)
colnames(transRevenue)[4] <- "transID"
# calculate the average ticket fare
transRevenue <- transform(transRevenue, avgFare = flightRevenue/flightQuantity)

# once you extract from BQ, the column naming might look odd. Rename all of them below
renameCol <- c("vid", "sid", "date", "visitStartTime", "device", "sourceMedium", "channel", "visitNo", "checkFs", "noFs", "checkFlightTrans", "noFlightTrans")
colnames(visitData1) <- renameCol
colnames(visitData2) <- renameCol
colnames(visitData3) <- renameCol
colnames(visitData4) <- renameCol

# Since we extract visit history by batch, need to bind them again to have the complete dataset
visitData <- rbind(visitData1, visitData2, visitData3, visitData4)
# format all columns accordingly
visitData$date <- strptime(visitData$date,format="%Y%m%d")
visitData$date <- as.Date(visitData$date)
visitData$vid <- as.character(visitData$vid)
visitData$sid <- as.character(visitData$sid)
visitData$device <- as.character(visitData$device)
visitData$sourceMedium <- as.character(visitData$sourceMedium)
visitData$channel <- as.character(visitData$channel)

# rearrange the dataset: For each visitor ID, arrange their visits in decreasing order (latest vist first)
# we turn the dataset up side down like this because
# THE END GOAL: For each transaction ID, tie this transaction to all the previous visits of the users prior to this transaction ID
# or i.e: be able to populate down this trans ID information to all the rows below of that user
visitDataFinal <- arrange(visitData, vid, desc(visitNo))

# clean trans data
transData$date <- strptime(transData$date,format="%Y%m%d")
transData$date <- as.Date(transData$date)
transData$vid <- as.character(transData$vid)

# Join the trans info and visit history data
combineData <- left_join(visitDataFinal, transData, by = c("vid" = "vid","sid" = "sid"))

# add reference table of transIDs for joining table after lagging IDs
transID <- as.factor(combineData$transID)
TransID_Table <- as.data.frame(transID)
TransID_Table$numericID <- as.numeric(combineData$transID)
TransID_Table <- distinct(TransID_Table)
combineData$vid <- as.numeric(combineData$vid)
combineData$transID <- as.numeric(combineData$transID)

# For each visitor ID, lag down the trans ID to all the rows below of the same visitor ID to create a "window" for that transaction
start.time <- Sys.time()
#combineDataFill <- combineData[, transFill:=lapply(combineData[,trandFill], function(x) str_replace(x," ","_"))]
#dat <- stack(lapply(unstack(combineData1[,c("vid", "transID")], vid ~ transID), na.locf, na.remaining = "keep"))
combineDataFill <- transform(combineData, transFill = stack(lapply(unstack(combineData, transID ~ vid), na.locf, na.remaining = "keep")))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
colnames(combineDataFill)[3] <- "date"
colnames(combineDataFill)[13] <- "transDate"
colnames(combineDataFill)[22] <- "transIDFill"
combineDataFill <- combineDataFill[,-c(23)]

# Similarly, this is to lag down the transDate to calculate the search to book date difference
combineDataFill_1 <- transform(combineDataFill, transDateFill = stack(lapply(unstack(combineDataFill, transDate ~ vid), na.locf, na.remaining = "keep")))
colnames(combineDataFill_1)[23] <- "transDateFill"
combineDataFill_1$transDateFill <- as.Date(combineDataFill_1$transDateFill)
combineDataFill_1 <- combineDataFill_1[,-c(24)]

# remove rows with fill column value = NA as these are the visits after transactions
filterCombineDataFill_na <- subset(combineDataFill_1, is.na(transIDFill) == FALSE)

# convert the transIDs back to character type
filterCombineDataFill <- left_join(filterCombineDataFill_na, TransID_Table, by = c("transIDFill" = "numericID"))
filterCombineDataFill <- left_join(filterCombineDataFill, TransID_Table, by = c("transID.x" = "numericID"))
filterCombineDataFill <- filterCombineDataFill %>%
  select(1:13,25,15:21, transIDFill = transID.y, 23)

# since now we have a "window" leading to the booking
# add in ranking for interactions in the conversion path

filterCombineDataFill_1 <- dplyr::mutate(group_by(filterCombineDataFill, transIDFill), pathrankfirst = row_number(visitNo), pathranklast = row_number(desc(visitNo)))
filterCombineDataFill_2 <- transform(filterCombineDataFill_1, s2b = difftime(transDateFill, date, units = "days"))
filterCombineDataFill_2$s2b <- as.integer(filterCombineDataFill_2$s2b)
# add in the bucket to have week ranges. Pick the relevant week ranges where the distribution is mostly fairly spread out
filterCombineDataFill_3 <- left_join(filterCombineDataFill_2, bucket4, c("s2b" = "lag"))
filterCombineDataFill_3 <- filterCombineDataFill_3[,-c(27:30)]
colnames(filterCombineDataFill_3)[27] <- "s2bBk4"
filterCombineDataFill_3 <- filterCombineDataFill_3 %>%
  mutate(s2bBk4M = case_when(!is.na(transID) ~ "Booking",
                             s2b == 0 ~ "Same Day of Booking",
                             TRUE ~ as.character(s2bBk4))) %>%
  mutate(transID = as.character(transID))


# Bring in the new channel grouping
filterCombineDataFill_4 <- left_join(filterCombineDataFill_3, channelMap, c("channel" = "Channel"))

# This is the final visit history data that we will operate further on
# filterCombineDataFinal <- filterCombineDataFill_4
filterCombineDataFinal <- filterCombineDataFill_3

# summarise info at the trans level to get
# firstVisitDate: 1st date when users start researching. Time lag from this to trans date
# pathLength = no. of interactions leading to booking
# pathLengthWithSearch = how many interactions involve a flight search instead of browsing other contents
# pathLengthFlightSearch = total no. of flight search happening before the booking happens
transInfo <- sqldf("select 
                   vid, transIDFill, device,
                   min(date) as firstVisitDate, max(date) as transDate, max(date)-min(date) as timeLag, count(sid) as pathLength,
                   min(visitNo) as firstVisitNo, max(visitNo) as transVisitNo,
                   sum(checkFs) as pathLengthWithSearch, sum(noFs) as pathLengthFlightSearch
                   from filterCombineDataFinal
                   group by vid, transIDFill, device
                   ")
transInfo$firstVisitDate <- as.Date(transInfo$firstVisitDate)
transInfo$transDate <- as.Date(transInfo$transDate)

# add in min cap day
transInfo1 <- cbind(transInfo, minCapDate = as.Date(c("2016-09-15")))
transInfo2 <- transform(transInfo1, transVsCapDate = difftime(transDate, minCapDate, units = "days"))

# Bring in other trans information. IMPORTANT: This is so that we can segment the conversion path to see the difference between devices, markets etc
# ensure joining columns have same data type
transData$vid <- as.numeric(transData$vid)
transInfo2$vid <- as.numeric(transInfo2$vid)
transInfo2$transIDFill <- as.character(transInfo2$transIDFill)
transData$transID <- as.character(transData$transID)
transFull <- left_join(transData, transInfo2, by = c("transID" = "transIDFill", "vid" = "vid"))

# this is specific for AirNZ, but in your case, please check if there is any NA rows and understand why in this case it is:
# Due to some transactions don't have visit history (i.e: buy farehold product that we haven't filtered in the trans query)
transFullnoNA <- transFull[!is.na(transFull$device),]

# Get "Immediate" vs "Same Day" booking - Time Lag 1
# Immediate: no previous interaction, straight away visit and book
# Same day: your 1st session and the booking happen at the same time
transFullnoNA <- transform(transFullnoNA, timeLag1 = ifelse(timeLag == 0 & pathLength == 1, "Immediate",ifelse(timeLag == 0 & pathLength > 1, "Same Day",timeLag)))

# add in search to flight (s2f) (full cycle): Dream to Flight = search to book + Book to fly 
transFullnoNA$bld <- as.integer(transFullnoNA$bld)
transFullnoNA$sld <- as.integer(transFullnoNA$sld)
transFullnoNA <- transform(transFullnoNA, s2f = timeLag + bld)

# If you already have revenue in the trans info. then ignore this part
# add in revenue info. 
transAll <- left_join(transFullnoNA, transRevenue, by = c("transID" = "transID"))
colnames(transAll)[c(1,2,3)] <- c("vid", "sid", "date")
transAll <- transAll[,-c(25,26,27)]

# Create a new device category categorisation
transAll <- transform(transAll, deviceCat = ifelse(device == 'desktop', "Desktop", "Mobile"))

# For booking lead days (book to flight), add in Bucket to have week ranges
transAll_BLDBucket <- left_join(transAll, bucket4, c("bld"="lag"))
transAll_BLDBucket <- transAll_BLDBucket[,-c(30)]
colnames(transAll_BLDBucket)[c(30:33)] <- c("b2fBk1","b2fBk2","b2fBk3", "b2fBk4")

# For search to book, add in Bucket to have week ranges
transAll_S2BBucket <- left_join(transAll_BLDBucket, bucket4, c("timeLag"="lag"))
transAll_S2BBucket <- transAll_S2BBucket[,-c(34)]
colnames(transAll_S2BBucket)[c(34:37)] <- c("s2bBk1","s2bBk2","s2bBk3", "s2bBk4")

# remove booking with negative bld (some tracking problems)
transAllBucket <- subset(transAll_S2BBucket, bld >= 0)
# break down the 0 days further into "same day" or "immediate"
transAllBucket <- transform(transAllBucket, s2bBk1M = ifelse(timeLag == 0, as.character(timeLag1), as.character(s2bBk1)))
transAllBucket <- transform(transAllBucket, s2bBk2M = ifelse(timeLag == 0, as.character(timeLag1), as.character(s2bBk2)))
transAllBucket <- transform(transAllBucket, s2bBk3M = ifelse(timeLag == 0, as.character(timeLag1), as.character(s2bBk3)))
transAllBucket <- transform(transAllBucket, s2bBk4M = ifelse(timeLag == 0, as.character(timeLag1), as.character(s2bBk4)))

# Summarise trans statistics. How many trans by device and market to give an overview of the dataset that we are looking at
totalTrans <- nrow(transAllBucket)
transDev <- dplyr::summarise(group_by(transAllBucket, deviceCat), cnt = n ())
transMar <- dplyr::summarise(group_by(transAllBucket, sMarket), cnt = n ())
  
# Insight: Get distribution of B2F by different percentile point (rather than just average number which are normally skewed)
# Summarise for market and device
b2fPct_Mar_Dev <- dplyr::summarise(group_by(transAllBucket, sMarket, deviceCat), cnt = n(), b2fpct90 = quantile(bld, 0.9),
                        b2fpct80 = quantile(bld, 0.8), b2fpct70 = quantile(bld, 0.7),
                        b2fpct60 = quantile(bld, 0.6), b2fpct50 = quantile(bld, 0.5),
                        b2fpct40 = quantile(bld, 0.4), b2fpct30 = quantile(bld, 0.3),
                        b2fpct20 = quantile(bld, 0.2), b2fpct10 = quantile(bld, 0.1),
                        avgFare = sum(flightRevenue)/sum(flightQuantity)
)

b2fPct_Mar_Dev <- data.frame(b2fPct_Mar_Dev)

# Summarise for market
b2fPct_Mar <- dplyr::summarise(group_by(transAllBucket, sMarket), cnt = n(), b2fpct90 = quantile(bld, 0.9),
                           b2fpct80 = quantile(bld, 0.8), b2fpct70 = quantile(bld, 0.7),
                           b2fpct60 = quantile(bld, 0.6), b2fpct50 = quantile(bld, 0.5),
                           b2fpct40 = quantile(bld, 0.4), b2fpct30 = quantile(bld, 0.3),
                           b2fpct20 = quantile(bld, 0.2), b2fpct10 = quantile(bld, 0.1),
                           avgFare = sum(flightRevenue)/sum(flightQuantity)
)

# add in a column with device = "all"
b2fPct_Mar_1 <- data.frame(cbind(b2fPct_Mar[,1], deviceCat = "all", b2fPct_Mar[,c(2:12)]))

# Summarise for device
b2fPct_Dev <- dplyr::summarise(group_by(transAllBucket, deviceCat), cnt = n(), b2fpct90 = quantile(bld, 0.9),
                               b2fpct80 = quantile(bld, 0.8), b2fpct70 = quantile(bld, 0.7),
                               b2fpct60 = quantile(bld, 0.6), b2fpct50 = quantile(bld, 0.5),
                               b2fpct40 = quantile(bld, 0.4), b2fpct30 = quantile(bld, 0.3),
                               b2fpct20 = quantile(bld, 0.2), b2fpct10 = quantile(bld, 0.1),
                               avgFare = sum(flightRevenue)/sum(flightQuantity)
)

# add in a column with market = "all"
b2fPct_Dev_1 <- data.frame(cbind(sMarket = "all" , b2fPct_Dev))

# summarise overall across all market and device
b2fPct <- data.frame(dplyr::summarise(transAllBucket, cnt = n(), b2fpct90 = quantile(bld, 0.9),
                               b2fpct80 = quantile(bld, 0.8), b2fpct70 = quantile(bld, 0.7),
                               b2fpct60 = quantile(bld, 0.6), b2fpct50 = quantile(bld, 0.5),
                               b2fpct40 = quantile(bld, 0.4), b2fpct30 = quantile(bld, 0.3),
                               b2fpct20 = quantile(bld, 0.2), b2fpct10 = quantile(bld, 0.1),
                               avgFare = sum(flightRevenue)/sum(flightQuantity)
))

# add in a column with market = "all" and device = "all"
b2fPct_1 <- data.frame(cbind(sMarket = "all", deviceCat = "all", b2fPct))

# binding all together to have combined overview of b2f distribution
b2fPctCombine <- rbind(b2fPct_Mar_Dev, b2fPct_Mar_1, b2fPct_Dev_1, b2fPct_1)

# B2F Summarise by buckets 
b2fBk1 <- dplyr::summarise(group_by(transAllBucket, sMarket, deviceCat, b2fBk1), transCnt = n(), flightRevenue = sum(flightRevenue), flightQuantity = sum(flightQuantity))
b2fBk2 <- dplyr::summarise(group_by(transAllBucket, sMarket, deviceCat, b2fBk2), transCnt = n(), flightRevenue = sum(flightRevenue), flightQuantity = sum(flightQuantity))
b2fBk3 <- dplyr::summarise(group_by(transAllBucket, sMarket, deviceCat, b2fBk3), transCnt = n(), flightRevenue = sum(flightRevenue), flightQuantity = sum(flightQuantity))

# Insight: Get distribution of S2B by different percentile point (rather than just average number which are normally skewed)
# For S2B, we don't use the full data, but just use transactions in 2 months data: Dec-Jan and 3 months of visit history (Look back window)
transAllBucket_Dec <- subset(transAllBucket, date >= as.Date("2016-12-15"))

# Summarise for market and device
s2bPct_Mar_Dev <- dplyr::summarise(group_by(transAllBucket_Dec, sMarket, deviceCat), cnt = n(), 
                                   s2bDayspct90 = quantile(timeLag, 0.9),
                                   s2bDayspct80 = quantile(timeLag, 0.8), s2bDayspct70 = quantile(timeLag, 0.7),
                                   s2bDayspct60 = quantile(timeLag, 0.6), s2bDayspct50 = quantile(timeLag, 0.5),
                                   s2bDayspct40 = quantile(timeLag, 0.4), s2bDayspct30 = quantile(timeLag, 0.3),
                                   s2bDayspct20 = quantile(timeLag, 0.2), s2bDayspct10 = quantile(timeLag, 0.1),
                                   s2bINTpct90 = quantile(pathLength, 0.9),
                                   s2bINTpct80 = quantile(pathLength, 0.8), s2bINTpct70 = quantile(pathLength, 0.7),
                                   s2bINTpct60 = quantile(pathLength, 0.6), s2bINTpct50 = quantile(pathLength, 0.5),
                                   s2bINTpct40 = quantile(pathLength, 0.4), s2bINTpct30 = quantile(pathLength, 0.3),
                                   s2bINTpct20 = quantile(pathLength, 0.2), s2bINTpct10 = quantile(pathLength, 0.1),
                                   avgFare = sum(flightRevenue)/sum(flightQuantity)
)

s2bPct_Mar_Dev <- data.frame(s2bPct_Mar_Dev)

# Summarise by market
s2bPct_Mar <- dplyr::summarise(group_by(transAllBucket_Dec, sMarket), cnt = n(), 
                               s2bDayspct90 = quantile(timeLag, 0.9),
                               s2bDayspct80 = quantile(timeLag, 0.8), s2bDayspct70 = quantile(timeLag, 0.7),
                               s2bDayspct60 = quantile(timeLag, 0.6), s2bDayspct50 = quantile(timeLag, 0.5),
                               s2bDayspct40 = quantile(timeLag, 0.4), s2bDayspct30 = quantile(timeLag, 0.3),
                               s2bDayspct20 = quantile(timeLag, 0.2), s2bDayspct10 = quantile(timeLag, 0.1),
                               s2bINTpct90 = quantile(pathLength, 0.9),
                               s2bINTpct80 = quantile(pathLength, 0.8), s2bINTpct70 = quantile(pathLength, 0.7),
                               s2bINTpct60 = quantile(pathLength, 0.6), s2bINTpct50 = quantile(pathLength, 0.5),
                               s2bINTpct40 = quantile(pathLength, 0.4), s2bINTpct30 = quantile(pathLength, 0.3),
                               s2bINTpct20 = quantile(pathLength, 0.2), s2bINTpct10 = quantile(pathLength, 0.1),
                               avgFare = sum(flightRevenue)/sum(flightQuantity)
)

# add in a column with device = "all"
s2bPct_Mar_1 <- data.frame(cbind(s2bPct_Mar[,1], deviceCat = "all", s2bPct_Mar[,c(2:21)]))

# Summarise by device 
s2bPct_Dev <- dplyr::summarise(group_by(transAllBucket_Dec, deviceCat), cnt = n(), 
                               s2bDayspct90 = quantile(timeLag, 0.9),
                               s2bDayspct80 = quantile(timeLag, 0.8), s2bDayspct70 = quantile(timeLag, 0.7),
                               s2bDayspct60 = quantile(timeLag, 0.6), s2bDayspct50 = quantile(timeLag, 0.5),
                               s2bDayspct40 = quantile(timeLag, 0.4), s2bDayspct30 = quantile(timeLag, 0.3),
                               s2bDayspct20 = quantile(timeLag, 0.2), s2bDayspct10 = quantile(timeLag, 0.1),
                               s2bINTpct90 = quantile(pathLength, 0.9),
                               s2bINTpct80 = quantile(pathLength, 0.8), s2bINTpct70 = quantile(pathLength, 0.7),
                               s2bINTpct60 = quantile(pathLength, 0.6), s2bINTpct50 = quantile(pathLength, 0.5),
                               s2bINTpct40 = quantile(pathLength, 0.4), s2bINTpct30 = quantile(pathLength, 0.3),
                               s2bINTpct20 = quantile(pathLength, 0.2), s2bINTpct10 = quantile(pathLength, 0.1),
                               avgFare = sum(flightRevenue)/sum(flightQuantity)
)

# add in a column with market = "all"
s2bPct_Dev_1 <- data.frame(cbind(sMarket = "all" , s2bPct_Dev))

# summarise overall level
s2bPct <- dplyr::summarise(transAllBucket_Dec, cnt = n(), 
                               s2bDayspct90 = quantile(timeLag, 0.9),
                               s2bDayspct80 = quantile(timeLag, 0.8), s2bDayspct70 = quantile(timeLag, 0.7),
                               s2bDayspct60 = quantile(timeLag, 0.6), s2bDayspct50 = quantile(timeLag, 0.5),
                               s2bDayspct40 = quantile(timeLag, 0.4), s2bDayspct30 = quantile(timeLag, 0.3),
                               s2bDayspct20 = quantile(timeLag, 0.2), s2bDayspct10 = quantile(timeLag, 0.1),
                               s2bINTpct90 = quantile(pathLength, 0.9),
                               s2bINTpct80 = quantile(pathLength, 0.8), s2bINTpct70 = quantile(pathLength, 0.7),
                               s2bINTpct60 = quantile(pathLength, 0.6), s2bINTpct50 = quantile(pathLength, 0.5),
                               s2bINTpct40 = quantile(pathLength, 0.4), s2bINTpct30 = quantile(pathLength, 0.3),
                               s2bINTpct20 = quantile(pathLength, 0.2), s2bINTpct10 = quantile(pathLength, 0.1),
                               avgFare = sum(flightRevenue)/sum(flightQuantity)
)

# add in a column with market = "all" and device = "all"
s2bPct_1 <- data.frame(cbind(sMarket = "all" , deviceCat = "all", s2bPct))

# bind everything together to have the final summary
s2bPctCombine <- data.frame(rbind(s2bPct_Mar_Dev, s2bPct_Mar_1, s2bPct_Dev_1, s2bPct_1))

# S2B - Summarise by Bucket
s2bBk1 <- dplyr::summarise(group_by(transAllBucket_Dec, sMarket, deviceCat, s2bBk1M), transCnt = n(), flightRevenue = sum(flightRevenue), flightQuantity = sum(flightQuantity))
s2bBk2 <- dplyr::summarise(group_by(transAllBucket_Dec, sMarket, deviceCat, s2bBk2M), transCnt = n(), flightRevenue = sum(flightRevenue), flightQuantity = sum(flightQuantity))
s2bBk3 <- dplyr::summarise(group_by(transAllBucket_Dec, sMarket, deviceCat, s2bBk3M), transCnt = n(), flightRevenue = sum(flightRevenue), flightQuantity = sum(flightQuantity))

# Insight: Relationship between B2F vs S2B 
s2b_b2f <- dplyr::summarise(group_by(transAllBucket_Dec, s2bBk2M, b2fBk2), cnt = n())

# Insight: Get the share of sessions leading to booking with searches by market and device
# Summarise & Compare by Market
pathLengthWithSearch_Mar <- dplyr::summarise(group_by(transAllBucket_Dec, sMarket), 
                                             pctPathLengthWithSearch = sum(pathLengthWithSearch)/sum(pathLength),
                                             avgSearchPerPath = sum(pathLengthFlightSearch)/sum(pathLengthWithSearch))
# Summarise & Compare by Device
pathLengthWithSearch_Dev <- dplyr::summarise(group_by(transAllBucket_Dec, deviceCat), 
                                             pctPathLengthWithSearch = sum(pathLengthWithSearch)/sum(pathLength),
                                             avgSearchPerPath = sum(pathLengthFlightSearch)/sum(pathLengthWithSearch))

# Insight: How search intensity changes when it is getting closer to make a booking 
# Search intensity = No. of flight searches in a sessions
# Conversion Path of booking in past 2 months
CombineDataFinal_Dec <- subset(filterCombineDataFinal, transDateFill >= as.Date("2016-12-15"))  

# Filter only for search session
searchSession <- subset(CombineDataFinal_Dec, checkFs > 0)

# Summarise by S2B - What are the different flight search intensity percentile
searchIntensity <- dplyr::summarise(group_by(searchSession, s2bBk4M), cnt = n(), SIpct90 = quantile(noFs, 0.9), SIpct50 = quantile(noFs, 0.5),  avgSI = sum(noFs)/n())

# CUSTOM ATTRIBUTION -- This is CLICK only data as we don't have impression data
# The same data extraction method is used for custom attribution vs search-book-fly

# Insight: Channel assist vs last click 

# For last interaction
# Extract for row with trans
transDec <- subset(CombineDataFinal_Dec, !is.na(transID))
# summarise on what are the channel of this trans
lastClick <- sqldf("select sourceMedium, channel, count(transID) as lastClick from transDec group by 1,2 order by 3 desc")

# For assisting interaction
# Filter for rows without trans (hence assisting)
assistDec <- subset(CombineDataFinal_Dec, is.na(transID))

assistConv <- sqldf("select sourceMedium, channel, count(transIDFill) as assistedConv, count(distinct transIDFill) as assistedConv_1 
                    from assistDec group by 1,2 order by 3 desc")

# Join assisting vs last click to get the final table
lastClickVsAssist <- left_join(lastClick, assistConv, c("sourceMedium" = "sourceMedium", "channel" = "channel"))

# Insight: The Channel position in the conversion path (based on percentile not average)
channelPosCat <- dplyr::summarise(group_by(CombineDataFinal_Dec, channel), cnt = n(), avgPos = sum(pathranklast)/n(),
                               pospct80 = quantile(pathranklast, 0.8), pospct50 = quantile(pathranklast, 0.5))

















