# Association Rules for Market Basket Analysis (R)
# http://rpubs.com/Adhislacy/281337
# https://github.com/krupanss/Market-Basket-Analysis-R/blob/master/MarketBasketAnalysis.Rmd
# https://educationalresearchtechniques.com/2016/08/01/market-basket-analysis-in-r/
# http://www.rpubs.com/Mughundhan/268460  -> use this first
# https://stackoverflow.com/questions/17313450/how-to-convert-data-frame-to-transactions-for-arules
# https://stackoverflow.com/questions/45578516/r-aggregate-and-collapse-several-cells-into-one
# https://stackoverflow.com/questions/15933958/collapse-concatenate-aggregate-a-column-to-a-single-comma-separated-string-w
# http://rstatistics.net/association-mining-with-r/
# https://rpubs.com/cheverne/associationrules_marketbasketanalysis
# https://rstudio-pubs-static.s3.amazonaws.com/267119_9a033b870b9641198b19134b7e61fe56.html -> ECLAT
# https://benjnmoss.wordpress.com/2017/02/13/market-basket-analysis-in-alteryx/
# https://synerise.com/data-mining-how-to-analyze-customers-market-baskets-to-increase-sales/#

library(arules)  # association rules
library(arulesViz)  # data visualization of association rules
library(RColorBrewer)  # color palettes for plots
library(tidyverse)
library(lubridate)

# read sample data into dataframe
raw_data <- read_csv("https://raw.githubusercontent.com/kh7393/Market-Basket/master/Online%20Retail_new.csv")
raw_data <- raw_data %>%
  mutate(InvoiceDate = format(InvoiceDate, "%H:%M:%S"))
  mutate(InvoiceDate = make_datetime(day, month, year, hour, minute))

# show countries
raw_data1 <- raw_data %>% select(Country, InvoiceNo) %>%
  group_by(Country) %>%
  summarize(InvoiceNo, n())

# Remove the canceled/refunded orders 
# Remove rows with invalid product description 

# select the rows with relevant data for analysis
transaction_detail <- aggregate(raw_data$AirlineDescription ~ raw_data$InvoiceNo,
                                FUN=paste,collapse=',')
# remove column for invoice number
transaction_itemsets<-transaction_detail[,-1]

# convert to transactions object for market basket analysis
write(transaction_itemsets,"itemsets2.csv")
itemsets_txn<-read.transactions("itemsets2.csv",format="basket",rm.duplicates=TRUE,sep=",")

# show the dimensions of the transactions object
print(dim(itemsets_txn))

print(dim(itemsets_txn)[1])  # X no. market baskets for flight trips
print(dim(itemsets_txn)[2])  # X no. of initial product/items

# summary of dataset including most frequent items, itemset/transaction length distribution
summary(itemsets_txn)

# find the top 15 items
itemFrequencyPlot(itemsets_txn, topN=15)

# exploratory plotting - examine frequency for each item with support greater than 0.025
pdf(file="fig_market_basket_initial_item_support.pdf", 
    width = 8.5, height = 11)
itemFrequencyPlot(itemsets_txn, support = 0.025, cex.names=0.8, xlim = c(0,0.3),
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))
dev.off()    


pdf(file="fig_market_basket_final_item_support.pdf", width = 8.5, height = 11)
itemFrequencyPlot(itemsets_txn, support = 0.025, cex.names=1.0, xlim = c(0,0.5),
                  type = "relative", horiz = TRUE, col = "blue", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))
dev.off()   

# obtain large set of association rules for items by category and all shoppers
# this is done by setting very low criteria for support and confidence
first.rules <- apriori(itemsets_txn, 
                       parameter = list(support = 0.001, confidence = 0.05))
print(summary(first.rules))  # yields 69,921 rules... too many
 
# for splitting LHS & RHS
Firstitemsets_txnrules_df <- as(first.rules, "data.frame")
Firstitemsets_txnrules_df <- Firstitemsets_txnrules_df %>%
  separate(rules, c("LHS", "RHS"), sep = "=>")


# select association rules using thresholds for support and confidence 
# yields 344 rules
second.rules <- apriori(itemsets_txn, 
                        parameter = list(support = 0.025, confidence = 0.05))
print(summary(second.rules))  
Seconditemsets_txnrules_df <- Firstitemsets_txnrules_df %>%
  filter(support >= 0.025 & confidence >= 0.05)

# data visualization of association rules in scatter plot
# pdf(file="fig_market_basket_rules.pdf", width = 8.5, height = 8.5)
plot(second.rules, 
     control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift")   
# dev.off()    

# grouped matrix of rules 
# pdf(file="fig_market_basket_rules_matrix.pdf", width = 8.5, height = 8.5)
plot(second.rules, method="grouped",   
     control=list(col = rev(brewer.pal(9, "Greens")[4:9])))
# dev.off()    

# this needs fixing
pdf(file="fig_market_basket_farmer_rules.pdf", width = 11, height = 8.5)
plot(top.second.rules, method="graph", 
     control=list(type="items"), 
     shading = "lift")
dev.off()  

# plot(second.rules,method="graph",interactive=TRUE,shading=NA)

itemsets_txnrules_df <- as(second.rules, "data.frame")
itemsets_txnrules_df <- itemsets_txnrules_df %>%
  separate(rules, c("LHS", "RHS"), sep = "=>") %>%
  mutate(InverseConfidence = (support * lift) / confidence)

# final table of recommended rules to use filtered by max confidence
# option to sort by lift
# if LHS is bought then RHS is purchased
rules_final <- itemsets_txnrules_df %>%
  filter(confidence > InverseConfidence) # %>%
  # arrange(desc(lift))

# non-case sensitive filter
filteredrules_df <- rules_final %>%
  filter(grepl("hotel", RHS, ignore.case = TRUE))