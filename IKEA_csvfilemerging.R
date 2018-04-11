library(readbulk)
library(lubridate)
library(tidyverse)
library(stringr)
library(rpivotTable)
library(viridis)
library(TTR)
library(googlesheets)

raw_data <- read_bulk(directory = "IKEA_NEW",
                      extension = ".csv", stringsAsFactor=FALSE)


raw_data2 <- read_bulk(directory = "IKEA_ProductView",
                      extension = ".csv", stringsAsFactor=FALSE)

raw_data_entries <- read_bulk(directory = "C:/Users/User/Documents/IKEA_ProductView/Entries",
                       extension = ".csv", stringsAsFactor=FALSE)

raw_data_stockcheck <- read_bulk(directory = "C:/Users/User/Documents/IKEA_ProductView/StockCheckChannel",
                              extension = ".csv", stringsAsFactor=FALSE)

raw_data_my <- read_bulk(directory = "C:/Users/User/Documents/IKEA_New/Malaysia",
                                 extension = ".csv", stringsAsFactor=FALSE)

raw_data_sg <- read_bulk(directory = "C:/Users/User/Documents/IKEA_New/Singapore",
                         extension = ".csv", stringsAsFactor=FALSE)

raw_data <- raw_data %>%
  left_join(raw_data2, by = "Hour") %>%
  select (-c(File.y))

raw_data_entries <- raw_data_entries %>%
  left_join(raw_data_stockcheck, by = "Hour") %>%
  select (-c(File.y))

raw_data <- raw_data %>%
  rename(DesktopVisits = Visits.from.Desktop.devices,
         MobileVisits = Visits.from.Mobile.Tablet.devices,
         NewVisits = New.Visits,
         ReturnVisits = Return.Visits,
         StockCheckDesktop = Stock.Check.Rate.from.Desktop.devices,
         StockCheckMobile = Stock.Check.Rate.from.Mobile.Tablet.devices,
         AvgMinsSpentDesktop = Average.Time.Spent.on.Site..minutes..from.Desktop.devices,
         AvgMinsSpentMobile = Average.Time.Spent.on.Site..minutes..from.Mobile.Tablet.devices,
         AddToShoppingListDesktop = Add.to.Shopping.List.Rate.from.Desktop.devices,
         AddToShoppingListMobile = Add.to.Shopping.List.Rate.from.Mobile.Tablet.devices,
         PdtViewRate = Product.View.Rate,
         PdtViewRate_Desktop = Product.View.Rate.from.Desktop.devices,
         PdtViewRate_Mobile = Product.View.Rate.from.Mobile.Tablet.devices) %>%
  mutate(DesktopVisits = as.numeric(gsub(",", "", DesktopVisits)),
         MobileVisits = as.numeric(gsub(",", "", MobileVisits)),
         Visits = as.numeric(gsub(",", "", Visits)),
         NewVisits = as.numeric(gsub(",", "", NewVisits)),
         ReturnVisits = as.numeric(gsub(",", "", ReturnVisits)),
         StockCheckDesktop = as.numeric(gsub("%", "", StockCheckDesktop)),
         StockCheckMobile = as.numeric(gsub("%", "", StockCheckMobile)),
         AddToShoppingListDesktop = as.numeric(gsub("%", "", AddToShoppingListDesktop)),
         AddToShoppingListMobile = as.numeric(gsub("%", "", AddToShoppingListMobile)),
         PdtViewRate = as.numeric(gsub("%", "", PdtViewRate)),
         PdtViewRate_Desktop = as.numeric(gsub("%", "", PdtViewRate_Desktop)),
         PdtViewRate_Mobile = as.numeric(gsub("%", "", PdtViewRate_Mobile))) %>%
  separate(Hour, c("Date", "hourofDay"), "T") %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  mutate(TotalVisits = DesktopVisits + MobileVisits) %>%
  mutate(hourofDay = as.numeric(substring(hourofDay, 1, 2))) %>%
  mutate(Day = wday(Date, label = TRUE)) %>%
  mutate(weekday_weekend = ifelse(Day %in% c("Sat", "Sun"), "Weekend", "Weekday"))

raw_data <- raw_data %>%
  group_by(hourofDay) %>%
  summarise(DesktopVisits = sum(DesktopVisits),
            MobileVisits = sum(MobileVisits)) %>%
  gather(DeviceType, NoOfVisits, 2:3)

# entries dataframe
raw_data_entries <- raw_data_entries %>%
  mutate_all(funs(gsub(",", "", .))) %>%
  mutate_all(funs(gsub("%", "", .))) %>%
  mutate_at(vars(c(2:16)), as.numeric) %>%
  mutate_at(vars(c(18:35)), as.numeric) %>%
  rename(OrganicSearch = Organic.Search,
         PaidSearch = Paid.Search,
         UnknownTypedBookmark = Unknown.Typed.Bookmark,
         SocialNetworksOwned = Social.Networks...Owned,
         ReferringDomains = Referring.Domains,
         SocialNetworksNatural = Social.Networks...Natural,
         OtherCampaigns = Other.Campaigns,
         CorpApps = Corporate.Apps,
         DigitalPublications = Digital.Publications,
         GoogleImageSearch = Google.Image.Search,
         EarnedVideo = Video...Earned,
         EmailOwned = Email...Owned) %>%
  separate(Hour, c("Date", "hourofDay"), "T") %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  mutate(hourofDay = as.numeric(substring(hourofDay, 1, 2))) %>%
  mutate(Day = wday(Date, label = TRUE)) %>%
  mutate(weekday_weekend = ifelse(Day %in% c("Sat", "Sun"), "Weekend", "Weekday"))


# facetted heatmap by month, weekday, hour of day
raw_data %>%
  ggplot(aes(x=hourofDay, y=Day, fill=TotalVisits)) +  # change y-axis if summary by weekend vs weekday
  geom_tile(color="white", size=0.1) +
  facet_grid(Month~Year) + 
  # scale_fill_continuous(low = "#7baaf7", high = "#3367d6") + 
  scale_fill_viridis(name="TotalVisits") +
  #scale_x_discrete(breaks = c(0:23), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(y = "Day", title="Visits by Hour and Day of Week") +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_text(size=9)) + 
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=6)) +
  annotate("rect", xmin=0.5, xmax=7.5, ymin=-Inf, ymax=Inf, alpha=.2, fill="blue")

# plot time series
ggplot() + 
  geom_line(data = raw_data, aes(x=hourofDay, y=NoOfVisits, color = DeviceType)) +
  labs(title="Monthly Time Series", 
       y="Visits") 

raw_data_entriesPlot <- raw_data_entries %>%
  select(c(1:17, 37:40)) %>%
  gather(firstTouchChannel, Visits, c(3:17)) %>%
  mutate(firstTouchChannel = as.factor(firstTouchChannel),
         weekday_weekend = as.factor(weekday_weekend)) %>%
  select(-c(Date, Day, Year, weekday_weekend)) %>%
  group_by(firstTouchChannel, hourofDay, Month) %>%
  summarise(Visits = sum(Visits))

# plot time series
ggplot(data = raw_data_entriesPlot, aes(x= hourofDay, y= Visits, group = firstTouchChannel, color = firstTouchChannel)) + 
  geom_line() +
  facet_wrap(~Month, nrow = 2) +
  labs(title="Monthly Time Series", 
       y="Visits") 

write_csv(raw_data_my, "IKEA_MY.csv")
write_csv(raw_data_sg, "IKEA_SG.csv")
write_csv(raw_data_entries, "IKEA_TH_Entries.csv")
write_csv(raw_data, "IKEA_TH_Merged.csv")
ikea_ss <- gs_upload("IKEA_TH_Merged.csv")
ikea_ss %>% gs_read()
file.remove("IKEA_TH_Merged.csv")