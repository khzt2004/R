library(googleAnalyticsR)
library(googleAuthR)
library(dplyr)

ga_auth()

alsoBoughtTable <- function(id, start, end){
  ga <- google_analytics(id, #=This is a (dynamic) ViewID parameter
                         date_range = c(start, end), 
                         metrics = c("itemQuantity"), 
                         dimensions = c("date", "transactionId", "productSku"),
                         anti_sample = TRUE,
                         max = -1)
  
  ga <- ga[1:3]
  ga <- subset(ga, !(duplicated(ga[2:3])))
  
  cross <- matrix(nrow=0, ncol=4)
  colnames(cross) <- c("date","productSku","alsoBought","transactionId")
  
  dates <- unique(ga$date)
  
  for(d in 1:(length(dates))){
    
    products <- unique(ga[which(ga$date == dates[d]),'productSku'])
    
    cr <- matrix(nrow=0, ncol=3)
    colnames(cr) <- c("productSku","alsoBought","transactionId")
    
    
    for (i in 1:(length(products))){
      
      1
      2
      3
      4
      5
      6
      7
      
      receipts <- ga[which(ga$productSku == products[i] & ga$date == dates[d]),'transactionId']
      
      bp <- subset(ga, transactionId %in% receipts)
      bp <- data.frame(productSku = products[i],
                       alsoBought = bp$productSku,
                       transactionId = bp$transactionId)
      cr <- rbind(cr, bp)
      
      if(i == 1 | i%%10 == 0 | i == length(products)){
        cat("\014")
        print(
          paste0(
            d, " of ", length(dates)," dates running: ",round(i*100/(length(products)),1), "% computed"
          )
        ) 
      }
    }
    
    cross <- rbind(cross, cbind(data.frame(date = dates[d]),cr))
  }
  cross$productSku <- as.character(cross$productSku)
  cross$alsoBought <- as.character(cross$alsoBought)
  return (cross)
}


2
3
4
5
6
7
8
9
10
11
12
13
14

calculateReceiptShare <- function(productDf){
  share <- group_by(productDf, productSku, alsoBought) %>%
    summarise(uniquePurchases = n_distinct(transactionId)) %>%
    as.data.frame()
  
  receipts <- group_by(productDf, productSku) %>%
    summarise(allReceipts = n_distinct(transactionId))
  
  share <- merge(share,receipts, by = "productSku", all.x = TRUE)
  
  share$shareOfAllReceipts <- round(share$uniquePurchases / share$allReceipts, 2)
  
  return (share)
}


alsoBought <- function(id, start, end){
  ga <- alsoBoughtTable(id, start, end)
  
  ga <- calculateReceiptShare(ga)
  ga <- ga[order(-ga$uniquePurchases),]
  return (ga)
}


ga_id <- 46948678    

df <- alsoBoughtTable(ga_id, Sys.Date()-5, Sys.Date()-1)

df_alsobought <- alsoBought(ga_id, Sys.Date()-5, Sys.Date()-1)
      
      
      
      
      
      