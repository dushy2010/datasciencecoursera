# Write a function called best that take two arguments: the 2-character 
# abbreviated name of a state and an outcome name. The function reads the 
# outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality
# for the specified outcome in that state. The hospital name is the name 
# provided in the Hospital.Name variable. The outcomes can be one of 
# “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have 
# data on a particular outcome should be excluded from the set of hospitals 
# when deciding the rankings.

# Handling ties. If there is a tie for the best hospital for a given outcome, 
# then the hospital names should be sorted in alphabetical order and the first 
# hospital in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” 
# are tied for best, then hospital “b” should be returned).

best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    fd   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
    colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if(!state %in% fd[, "state"]) {
        stop('Invalid state')
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop('Invalid outcome')
    } else {
    ## Return hospital name in that state with lowest 30-day death
        si <- which(fd[, "state"] == state)
        ts <- fd[si, ]    
        oi <- as.numeric(ts[, eval(outcome)])
        min_val <- min(oi, na.rm = TRUE)
        result  <- ts[, "hospital"][which(oi == min_val)]
        output  <- result[order(result)]
    }
    return(output)
}
    
