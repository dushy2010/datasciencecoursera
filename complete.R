# Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.

complete <- function( directory, id = 1:332 ) {
    
    path <- "/Users/dchillal/Downloads/Coursera/specdata"
    mydata <- data.frame()
    
    for (i in id) {
        if (i < 10) {
            linedata <- read.csv(paste(path,"/00", as.character(i),".csv", sep=""), as.is = T, 
                                 header = T)
        }
        else if (i > 9 && i < 100) {
            linedata <- read.csv(paste(path,"/0", as.character(i),".csv", sep=""), as.is = T, 
                                 header = T)
        }
        else {
            linedata <- read.csv(paste(path,"/", as.character(i),".csv", sep=""), as.is = T, 
                                 header = T)
        }
        nobs <- sum(complete.cases(linedata))
        eachfile <- data.frame(i, nobs)
        mydata <- rbind(mydata,eachfile)
        
    }
    return(mydata)
}