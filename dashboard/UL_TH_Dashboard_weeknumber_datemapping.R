library(ISOweek)

date_in_week <- function(year, week, weekday){
  w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
  ISOweek2date(w)
}

start_date <- date_in_week(year = 2018, week = 1:53, weekday = 1) - 1
week <- 0:52
start_date2 <- date_in_week(year = 2019, week = 1:53, weekday = 1) - 1
week2 <- 0:52

c_presentyear <- data_frame(week,start_date)
c_presentyear <- c_presentyear %>% mutate(year = '2018')
c_nextyear <- data_frame(week2,start_date2)
c_nextyear <- c_nextyear %>% mutate(year = '2019') %>% select(week = 'week2', start_date = 'start_date2', year)

c_total <- rbind(c_presentyear, c_nextyear)
c_total <- c_total %>% select(year, week, start_date)

# Variables for the BigQuery upload portion
destinationProject <- 'unified-welder-172709'
destinationDataset <- 'TH_Marketing_Performance_Dashboard_Targets'
reportName <- 'week_date_year_mapping'

# Check if the table exists, if table exists, then delete the table
tryCatch(delete_table(destinationProject, destinationDataset, reportName),
         error = function(e){
           print(paste0(reportName, " not available for deletion"))
         })


# Upload the table into big query
tryCatch(insert_upload_job(destinationProject, destinationDataset, reportName, c_total),
         error = function(e){
           print(paste0(reportName, " failed to upload"))
         })

