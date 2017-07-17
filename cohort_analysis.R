library(curl)
library(plyr)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(colorRamps)


dr <- read.csv(curl("https://github.com/khzt2004/R/raw/master/sample_date.csv"), stringsAsFactors = F )

dr$date_add <- as.Date(dr$date_add)
dr$date_add_month <- as.Date(cut(dr$date_add, breaks = "month"))
dr <- subset(dr, (date_add_month > as.Date("2015-05-01") &   (date_add_month < as.Date("2016-06-01") )))
number_of_months <-length(unique(as.character(dr$date_add_month)))

Cohorts = data.frame() ## empty cohorts data frame. 
for (m in unique(as.character(dr$date_add_month))){
  u = subset(dr, dr$date_add_month == m) # data for month m
  ulist = unique(u$user_id) # orders by unique users in month m
  dr1 = subset(dr, dr$user_id %in% ulist) ## -- only month m users
  dr = subset(dr, !(dr$user_id %in% ulist)) ## -- remove from dr
  ## -- Number of orders by these users for every month
  dr1s = ddply(dr1, .(date_add_month), summarize, total = length(user_id))
  ## -- Combine the calculations into the Cohorts data frame. 
  colnames(dr1s) = c("Month", m)
  a = c(m, dr1s[,m])
  a = data.frame(t(a))
  Cohorts = rbind.fill(Cohorts, a)
}

## Some more processing with the final data frame
col_names = paste("Month", array(1:number_of_months), sep = " ")
colnames(Cohorts) <- c("Month", col_names)
Cohorts["Month"] <- as.Date(Cohorts$Month)
Cohorts["max"] <- as.numeric(as.character(Cohorts[,2]))

# plot dataframe
df_plot <- melt(Cohorts, id.vars = c('Month', 'max'), value.name = 'Orders', variable.name = 'Month_after')
df_plot <- na.omit(df_plot)
df_plot["Value"] <- as.numeric(df_plot$Orders)
df_plot["Pc_value"] <- ceiling(df_plot$Value*100/df_plot$max)
df_plot["Percentage"] <- paste(ceiling(df_plot$Value*100/df_plot$max), "%", sep="")
df_plot["Percentage_2"] <- ifelse(df_plot$Percentage == "100%", df_plot$Value , df_plot$Percentage)

## Cohorts tile chart ##
hm.palette <- colorRampPalette((brewer.pal(9, 'YlOrRd')), space='Lab', bias=10)
p <- ggplot() 
p <- p + geom_tile(data = na.omit(df_plot), aes(x = Month, y = Month_after, fill=Value), width=31, height=1) 
p <- p + geom_text(data =  na.omit(df_plot), aes(x = Month, y = Month_after, label = Percentage_2), color = "white")#, fontface = "bold")
p <- p + scale_fill_gradientn(colours = hm.palette(100)) + theme_bw() + coord_flip() 
p <- p + theme(panel.background = element_blank(), axis.line.x = element_blank(), panel.border = element_blank())
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none") + ylab("") 
p <- p + xlab("Orders by new users") 
print(p)

