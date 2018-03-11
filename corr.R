# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. 

corr <- function(directory, threshold = 0) {
    
    path <- "/Users/dchillal/Downloads/Coursera/specdata"
    corr_vect <- NULL
    
    for (i in 1:332) {
        if (i < 10) {
            linedata <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                            as.is = T, 
                            header = T)
        }
        else if (i > 9 && i < 100) {
            linedata <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                            as.is = T, 
                            header = T)
        }
        else {
            linedata <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                            as.is = T, 
                            header = T)
        }
        
        data <- linedata[complete.cases(linedata),]
        if (nrow(data) > threshold) {
            corr_vect <- c(corr_vect, cor(data[,"sulfate"], data[, "nitrate"]))
        }
    }
    
    return(corr_vect)
}