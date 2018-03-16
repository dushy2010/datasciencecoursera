
# Write a function called rankall that takes two arguments: an outcome name 
# (outcome) and a hospital ranking (num). The function reads the 
# outcome-of-care-measures.csv file and returns a 2-column data frame containing 
# the hospital in each state that has the ranking specified in num. For example 
# the function call rankall("heart attack", "best") would return a data frame 
# containing the names of the hospitals that are the best in their respective 
# states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named
# hospital, which contains the hospital name, and the second column is named 
# state, which contains the 2-character abbreviation for the state name. 
# Hospitals that do not have data on a particular outcome should be excluded 
# from the set of hospitals when deciding the rankings.


rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    fd   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
    colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that outcome is valid
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        
        stop('Invalid outcome')
        
    } else if (is.numeric(num)) {
        
        ## For each state, find the hospital of the given rank
        by_state <- with(fd, split(fd, state))
        ordered  <- list()
        
        for (i in seq_along(by_state)){
            by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                 by_state[[i]][, "hospital"]), ]
            ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
    
    } else if (!is.numeric(num)) {
        
        if (num == "best") {
            by_state <- with(fd, split(fd, state))
            ordered  <- list()
            
            for (i in seq_along(by_state)){
                by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                     by_state[[i]][, "hospital"]), ]
                ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, stringsAsFactors = FALSE)
            rownames(output) <- output[, 2]
        
        } else if (num == "worst") {
            
            by_state <- with(fd, split(fd, state))
            ordered  <- list()
            for (i in seq_along(by_state)){
                by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                     by_state[[i]][, "hospital"], 
                                                     decreasing = TRUE), ]
                ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, stringsAsFactors = FALSE)
            rownames(output) <- output[, 2]
        } else {
            stop('invalid num')
        }
    }
    
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return(output)
}