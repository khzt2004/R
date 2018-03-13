library(tidyverse)
library(googleAnalyticsR)
library(lubridate)
library(googlesheets)
library(reshape2)
library(ggrepel)


ga_auth(new_user = TRUE)
## get your accounts
account_list <- ga_account_list()
#id_hk <- account_list[64,'viewId']
#id_indo <- account_list[65,'viewId']
#id_my <- account_list[66,'viewId']
#id_ph <- account_list[67,'viewId']
#id_sg <- account_list[68,'viewId']
#id_tw <- account_list[69,'viewId']

rollup <- account_list$viewId[account_list$viewName=='Roll-up All (filtered)']

my_segments <- ga_segment_list()
segs <- my_segments$items

segment_for_allusers <- "gaid::-1"
seg_allUsers <- segment_ga4("All Users", segment_id = segment_for_allusers)

# enter start date and end date here. Format: yyyy-mm-dd
startDate <- "2018-01-13"
endDate <- "2018-01-30"


##Transactions for userGender and Age
#Transactions
scattera <- google_analytics(rollup, 
                               date_range = c(startDate, endDate), 
                               metrics = c("sessions", "transactions", "transactionRevenue"),
                               dimensions = c("userGender", "userAgeBracket"),
                               anti_sample = TRUE,
                               max = -1)
scattera$aov <- (scattera$transactionRevenue / scattera$transactions)
scattera$aov <- round(scattera$aov, digits=2)
scattera$cr <- ((scattera$transactions / scattera$sessions)*100)
scattera$cr <- round(scattera$cr, digits=2)
quantile(scattera$cr, 0.8)

scattera_CR <- scattera %>%
  filter(cr >= quantile(scattera$cr, 0.8))

quantile(scattera$aov, 0.8)

scattera_AOV <- scattera %>%
  filter(aov >= quantile(scattera$aov, 0.8)) %>%
  select(1,2,6,7)

#plot(scattera$cr, scattera$aov, 
#     main="CR/AOV based on User Age & Gender")

scatterfa<-subset(scattera, cr > 0 & aov > 0)
g<-ggplot(data = scatterfa, aes(x = cr, y = aov)) + 
  geom_point(size=3, colour = "#D55E00") +
  geom_text(aes(label = paste(userGender,userAgeBracket)), color = "black", 
            position = "jitter", hjust=0.6, vjust=1.1, size = 2.7) +
  labs(title = "CR/AOV based on User Age and Gender", x = "Conversion Rate", y = "Average Order Value")
##Add a horizontal & vertical line
g + geom_hline(aes(yintercept=quantile(scattera$aov, 0.8))) + geom_vline(aes(xintercept=quantile(scattera$cr, 0.8)))


#Transactions for user Interest

scatter <- google_analytics(rollup, 
                               date_range = c(startDate, endDate), 
                               metrics = c("sessions", "transactions", "transactionRevenue"),
                               dimensions = c("interestAffinityCategory"),
                               anti_sample = TRUE,
                               max = -1)

scatter$aov <- (scatter$transactionRevenue / scatter$transactions)
scatter$aov <- round(scatter$aov, digits=2)
scatter$cr <- ((scatter$transactions / scatter$sessions)*100)
scatter$cr <- round(scatter$cr, digits=2)
colnames(scatter)[1] <- "interest"

##Naming
scatter$interest[scatter$interest == "Movie Lovers"] <- "MovLov"
scatter$interest[scatter$interest == "Technophiles"] <- "Techno"
scatter$interest[scatter$interest == "Music Lovers/Pop Music Fans"] <- "MusLov(Pop)"
scatter$interest[scatter$interest == "TV Lovers"] <- "TVLov"
scatter$interest[scatter$interest == "Music Lovers/Rock Music Fans"] <- "MusLov(Rock)"
scatter$interest[scatter$interest == "Shoppers/Shopaholics"] <- "Shoppers"
scatter$interest[scatter$interest == "Gamers/Casual & Social Gamers"] <- "Gamers(Casual)"
scatter$interest[scatter$interest == "News Junkies & Avid Readers/Entertainment & Celebrity News Junkies"] <- "NewsJunkies(Ent)"
scatter$interest[scatter$interest == "Mobile Enthusiasts"] <- "MobEnthu"
scatter$interest[scatter$interest == "News Junkies & Avid Readers"] <- "NewsJunkies"
scatter$interest[scatter$interest == "Music Lovers/Rap & Hip Hop Fans"] <- "MusLov(Hip)"
scatter$interest[scatter$interest == "Music Lovers/Folk & Traditional Music Enthusiasts"] <- "MusLov(Folk)"
scatter$interest[scatter$interest == "Home Decor Enthusiasts"] <- "HomeDec"
scatter$interest[scatter$interest == "Music Lovers/Electronica & Dance Music Fans"] <- "MusLov(Elect)"
scatter$interest[scatter$interest == "Travel Buffs/Beachbound Travelers"] <- "TravBuf(Beach)"
scatter$interest[scatter$interest == "Music Lovers/Country Music Fans"] <- "MusLov(Country)"
scatter$interest[scatter$interest == "News Junkies & Avid Readers/Business & Economic News Junkies"] <- "NewsJunkies(Biz)"
scatter$interest[scatter$interest == "TV Lovers/Game, Reality & Talk Show Fans"] <- "TVLov(Reality)"
scatter$interest[scatter$interest == "Auto Enthusiasts/Performance & Luxury Vehicle Enthusiasts"] <- "AutoEntu(Luxury)"
scatter$interest[scatter$interest == "News Junkies & Avid Readers/Local News Junkies"] <- "NewsJunkies(Local)"
scatter$interest[scatter$interest == "News Junkies & Avid Readers/Men's Media Fans"] <- "NewsJunkies(Men's)"
scatter$interest[scatter$interest == "  News Junkies & Avid Readers/Women's Media Fans"] <- "NewsJunkies(Women's)"
scatter$interest[scatter$interest == "News Junkies & Avid Readers/World News Junkies"] <- "NewsJunkies(World)"
scatter$interest[scatter$interest == "Music Lovers/Indie & Alternative Rock Fans"] <- "MusLov(Indie)"
scatter$interest[scatter$interest == "Movie Lovers/Action & Adventure Movie Fans"] <- "MovLov(Action)"
scatter$interest[scatter$interest == "Movie Lovers/Sci-Fi & Fantasy Movie Fans"] <- "MovLov(SciFi)"
scatter$interest[scatter$interest == "Sports Fans/Water Sports Enthusiasts"] <- "SportsFan"
scatter$interest[scatter$interest == "Movie Lovers/South Asian Film Fans"] <- "MovLov(SouthAsian)"
scatterf1<-subset(scatter, cr > quantile(cr,.8))
scatterf2<-subset(scatter, aov > quantile(aov,.8, na.rm = TRUE))
scatterf<-rbind(scatterf1,scatterf2)
scatterfs<-unique(scatterf)
scatters<-subset(scatterfs, cr > 0)


scatter_filter <- scatter %>%
  filter(cr > quantile(cr,.8) | aov > quantile(aov,.8, na.rm = TRUE)) %>%
  unique() %>%
  filter(cr > 0)


scatter_filter1 <- scatter %>%
  filter(cr > quantile(cr,.8) | aov > quantile(aov,.8, na.rm = TRUE))

scatter_filter2 <- unique(scatter_filter1)
scatter_filter3 <-subset(scatter_filter2, cr > 0)


g<-ggplot(data = scatters, aes(x = cr, y = aov)) + 
  geom_point(size=3, colour = "#D55E00") +
  geom_text(aes(label = interest), color = "black", 
            position = "jitter", hjust=0.6, vjust=1.1, size = 2.5) +
  labs(title = "CR/AOV based on User Interest Category", x = "Conversion Rate", y = "Average Order Value")
##Add a horizontal & vertical line
g + geom_hline(aes(yintercept=mean(na.omit(scatter$aov)))) + geom_vline(aes(xintercept=mean(na.omit(scatter$cr))))
