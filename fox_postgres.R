library(tidyverse)
library(RPostgreSQL)


drv <- dbDriver("PostgreSQL")

# connect to database
con <- dbConnect(drv, 
                 host = 'foxplusbeta.cvacqrqwmflb.ap-southeast-1.redshift.amazonaws.com',
                 dbname="foxplusbeta", 
                 port=32545,
                 user = 'sparkline', 
                 password = '394$$m_Rsm=X')

# view schema
schema <- dbGetQuery(con, "select * from information_schema.tables;")


# view columns in a table

colnames <- dbGetQuery(con, "SELECT
                       *  FROM information_schema.COLUMNS;")

testquery <- dbGetQuery(con, "select * from fox_website_beta.ahihi;")
#permission denied on schema??

testquery1 <- dbGetQuery(con, "select event, event_text from fox_android_beta.tracks 
group by 1,2;")
testquery1 <- testquery1 %>% select(1,9)

dbWriteTable(con, c("public", "test_table"), value = testquery1, append=TRUE, row.names=F)

# disconnect from database
dbDisconnect(con)

# unload driver
dbUnloadDriver(drv)


