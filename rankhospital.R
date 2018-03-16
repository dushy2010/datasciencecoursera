
# Write a function called rankhospital that takes three arguments: 
#   the 2-character abbreviated name of a state (state), 
#   an outcome (outcome), and
#   the ranking of a hospital in that state for that outcome (num)

# The function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the ranking 
# specified by the num argument


rankhospital <- function(state, outcome, num = "best") {
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
    if (!state %in% fd[, "state"]) {
        
        stop('Invalid state')
    
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    
        stop('Invalid outcome')
    
    } else if (is.numeric(num)) {
    
        ## Return hospital name in that state with the given rank
        si <- which(fd[, "state"] == state)
        ts <- fd[si, ]                     
        ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
        ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
        output <- ts[, "hospital"][num]  
    
    } else if (!is.numeric(num)) {
        
        if (num == "best") {
        
            output <- best(state, outcome)
        
        } else if (num == "worst") {
            
            si <- which(fd[, "state"] == state)
            ts <- fd[si, ]    
            ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
            ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
            output <- ts[, "hospital"][1]
        
        } else {
        
                stop('invalid rank')
        
        }
    }
    ## 30-day death rate
    return(output)
}