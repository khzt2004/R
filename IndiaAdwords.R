library(XLConnect)
library(data.table)

# replace file name with name of appropriate source file 
df <- readWorksheetFromFile("India_Adwords.xlsx", sheet = 1)
dfAdwords <- data.frame(df)

# update list of VARs here
varList = c(
 "Best Engineering Aids & Consultancies Pvt. Ltd.",
 "Best Engineering Aids & Consultancies Pvt. Ltd. - Belgaum",
 "Best Engineering Aids & Consultancies Pvt. Ltd.- Bangalore",
 "Best Engineering Aids & Consultancies Pvt. Ltd. - Kolhapur",
 "Best Engineering Aids & Consultancies Pvt. Ltd. - Mumbai",
 "Best Engineering Aids & Consultancies Pvt. Ltd. - Pune-West",
 "Best Engineering Aids & Consultancies Pvt. Ltd.- Chennai",
 "Conceptia Software Technologies Pvt. Ltd. - India South",
 "Conceptia Software Technologies Pvt. Ltd. - India West",
 "Conceptia Software Technologies Pvt. Ltd. - India North-Terminated",
 "Pelf Infotech Private Limited",
 "Sim Technologies Pvt. Ltd. - Coimbaore",
 "Sim Technologies Pvt. Ltd. - South",
 "Sim Technologies Pvt. Ltd. - West",
 "Egs Computers India Pvt. LTS. - Trichy",
 "Egs Computers India Pvt. Ltd.",
 "Egs Computers India Pvt. Ltd. - Coimbatore",
 "Ideas Design Solutions - India North",
 "Ideas Design Solutions - India West - terminated",
 "Iris Hightech Private Ltd.",
 "Tech Savvy Engineers Private Ltd.",
 "Mark Engineering",
 "Khodiyar Infotech",
 "Engineering Technique",
 "Addonix Technologies Pvt. Ltd.",
 "Logical Solutions Ltd.")
  
 
index <- dfAdwords$Organization %in% varList & grepl("google", dfAdwords$Source, ignore.case = TRUE)
finaldf <- df[index, ]

fileDate <- format(Sys.time(), "%m%d%Y")
fileName <- paste("India_Adwords_", fileDate, ".xlsx", sep="")

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
