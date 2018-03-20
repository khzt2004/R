library(data.table)
library(ggplot2)
library(arules)
library(sqldf)
library(tidyverse)
library(googledrive)
library(arulesViz)
library(RColorBrewer)

# https://drive.google.com/open?id=1CKlLQWXX88ih2BKcNCtlnrmuVeckF4En for data source. Or connect to bigquery
# mba<-read.csv("https://raw.githubusercontent.com/kh7393/Market-Basket/master/EX_marketbasketanalysis_data.csv", header=T)
# MBA_data <- read_csv("https://raw.githubusercontent.com/kh7393/Market-Basket/master/Airline_MBA.csv")
MBA_data <- read_csv("Airline_MBA.csv")

# filter airlineMBA dataset for required criteria first before running analysis
MBA_1 <-data.table(MBA_data)

# Eg
# MBA_filtered <- MBA_1[ MBA_1$Country == "United States" ,
#                       c("InvoiceNo","StockCode") ]

MBA_filtered <- MBA_1[,StockCode, InvoiceNo]

#create a column with 1 as value for each line
MBA_1$c <- 1

#sum by customer ID or transaction ID --> it will give you the number of orders
mba1_totals<-MBA_1[,sum(c), by = CustomerID]

#rename the variables
mba_totals<-setNames(mba1_totals, c("ID", "Qty"))

# find the users who have the maximum number of orders
mba_totals[mba_totals$Qty==max(mba_totals$Qty)]

# Users with more than 10 orders 
mba_totals[mba_totals$Qty>10]

# The number of users with only one product
nrow(mba_totals[mba_totals$Qty==1])

# Quartile of your number of products per users
summary(mba_totals$Qty)


#to be able to use the function "split", you need to transform the type of your dataset
mba_corrected1 <- as.data.frame(MBA_filtered)
mba_grouped <- split(mba_corrected1$StockCode, mba_corrected1$InvoiceNo)
txn <- as(mba_grouped,"transactions")

#A summary is provided below:
summary(txn)


# The top 15 most frequently selling items
itemFrequencyPlot(txn, topN=15)

# exploratory plotting - examine frequency for each item with support greater than 0.025
itemFrequencyPlot(txn, support = 0.025, cex.names=0.8, xlim = c(0,0.7),
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))

# obtain large set of association rules for items by category and all shoppers
# this is done by setting very low criteria for support and confidence
first.rules <- apriori(txn, 
                       parameter = list(support = 0.001, confidence = 0.05))
print(summary(first.rules))  # if this yields > 20000 rules... too many

# for splitting LHS & RHS
Firstitemsets_txnrules_df <- as(first.rules, "data.frame")
Firstitemsets_txnrules_df <- Firstitemsets_txnrules_df %>%
  separate(rules, c("LHS", "RHS"), sep = "=>")

# select association rules using thresholds for support and confidence 
# also try support of 0.05 and confidence of 0.4
second.rules <- apriori(txn, 
                        parameter = list(support = 0.025, confidence = 0.05))
print(summary(second.rules))  
Seconditemsets_txnrules_df <- Firstitemsets_txnrules_df %>%
  filter(support >= 0.025 & confidence >= 0.05)

# data visualization of association rules in scatter plot
plot(second.rules, 
     control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift") 

# grouped matrix of rules 
plot(second.rules, method="grouped",   
     control=list(col = rev(brewer.pal(9, "Greens")[4:9])))

# mine association rules using a priori algorithm
Rules <- apriori(txn, parameter=list(supp=0.05,conf=0.4,target="rules",minlen=1))
inspect(Rules)

# plot support, confidence and lift
Rules.Quality <- quality(Rules)
p <- ggplot(data = Rules.Quality, aes(x = support, y = confidence, color = lift,
                                      size = 5))
p + geom_point()

itemsets_txnrules_df <- as(second.rules, "data.frame")
itemsets_txnrules_df <- itemsets_txnrules_df %>%
  separate(rules, c("LHS", "RHS"), sep = "=>") %>%
  mutate(InverseConfidence = (support * lift) / confidence)

# final table of recommended rules to use filtered by max confidence
# option to sort by lift
# if LHS is bought then RHS is purchased
rules_final <- itemsets_txnrules_df %>%
  filter(confidence > InverseConfidence)



# find products with lift > 1
subrule1 <- Rules[Rules.Quality$lift > 1]
subrule1_df <- as(subrule1, "data.frame")

# find products with a higher support
subrule2 <- Rules[Rules.Quality$support > 0.2]
subrule2_df <- as(subrule2, "data.frame")

# sort products by confidence
subrule3<-sort(Rules, by="confidence", decreasing=TRUE)
subrule3_df <- as(subrule3, "data.frame")





