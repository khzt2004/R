library(XLConnect)
library(data.table)

# get leads export from Siebel call center, country = India, created date >= past 2 weeks
# replace file name with name of appropriate source file 
df <- readWorksheetFromFile("India_Adwords.xlsx", sheet = 1)
dfAdwords <- data.frame(df)


index <- grepl("google", dfAdwords$Source, ignore.case = TRUE)
finaldf <- df[index, ]

fileDate <- format(Sys.time(), "%m%d%Y")
fileName <- paste("India_Adwords2_", fileDate, ".xlsx", sep="")

#creating an Excel workbook. Both .xls and .xlsx file formats can be used.
wb <- loadWorkbook(fileName, create = TRUE)

#creating sheets within an Excel workbook
createSheet(wb, name = "filteredVARs")

#writing into sheets within an Excel workbook : 
#writing finaldf data frame into filteredVARs

writeWorksheet(wb, finaldf, sheet = "filteredVARs", startRow = 1, startCol = 1)

#saving a workbook to an Excel file :
#saves a workbook to the corresponding Excel file and writes the file to disk.
saveWorkbook(wb)
