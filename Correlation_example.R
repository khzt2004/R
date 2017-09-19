df1 <- airquality
df1 <- as.matrix(airquality[c("Ozone",
                              "Solar.R", 
                              "Wind", 
                              "Month")])
df2 <- as.matrix(airquality[c("Temp","Day")])
cor(df1, df2, method = "pearson", use = "complete.obs")  
