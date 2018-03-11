
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id=1:332){
  
  path <- "/Users/dchillal/Downloads/Coursera/specdata"
  mydata <- data.frame()
  
  for (i in id) {
    if (i < 10) {
      linedata <- read.csv(paste(path,"/00", as.character(i),".csv", sep=""), as.is = T, 
                           header = T)
      mydata <- rbind(mydata,linedata)
    }
    else if (i > 9 && i < 100) {
      linedata <- read.csv(paste(path,"/0", as.character(i),".csv", sep=""), as.is = T, 
                           header = T)
      mydata <- rbind(mydata,linedata)
    }
    else {
      linedata <- read.csv(paste(path,"/", as.character(i),".csv", sep=""), as.is = T, 
                           header = T)
      mydata <- rbind(mydata,linedata)
    }
  }
  return(mean(mydata[,pollutant],na.rm = T))
}