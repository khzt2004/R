library(RPostgreSQL)

# sample data from http://www.postgresqltutorial.com/postgresql-sample-database/

drv <- dbDriver("PostgreSQL")

# connect to database
con <- dbConnect(drv, dbname="dvdrental", user = 'postgres', password = 'c3uch9T@')

# view schema
schema <- dbGetQuery(con, "select * from information_schema.tables where table_schema='public';")

# view columns in a table

colnames <- dbGetQuery(con, "SELECT
*  FROM information_schema.COLUMNS WHERE TABLE_NAME = 'store';")

# make a query
# rs <- dbSendQuery(con, "select * from country")
# fetch(rs,n=-1)

rs <- dbGetQuery(con, "select country, count(id) from staff_list
                 group by country")

rs <- dbGetQuery(con, "select * from store limit 20")

# disconnect from database
dbDisconnect(con)

# unload driver
dbUnloadDriver(drv)