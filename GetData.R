
# Read csv file
CsvURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(CsvURL,destfile = "~/Downloads/Coursera/housing_data_idaho.csv")
dateDownloadedCsv <- date() 
myCsvData <- read.csv(file="~/Downloads/Coursera/housing_data_idaho.csv", header = TRUE)

# length(myCsvData$VAL[!is.na(myCsvData$VAL) & myCsvData$VAL==24])

