rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    
    ## Check that state and outcome are valid
    if(!is.element(outcome, c("heart attack","heart failure","pneumonia"))) {
        stop("invalid outcome")
    }
    
    if(!is.element(state, unique(data$State))) {
        stop("invalid state")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    ## First check relevant column
    if(outcome == "heart attack")
        col <- 11
    
    if(outcome == "heart failure")
        col <- 17
    
    if(outcome == "pneumonia")
        col <- 23
    
    ## Transform outcome to numeric and remove NAs
    data[,col] <- suppressWarnings(as.numeric(data[,col]))
    data <- data[data$State==state & !is.na(data[,col]),]
    
    rankedList <- data[order(data[,col],data$Hospital.Name),c(2,col)]
    
    ## if the number is an integer
    if(!is.na(suppressWarnings(as.integer(num)))) {
        if(num > nrow(rankedList))
            NA
        else
            rankedList$Hospital.Name[num]
    } else {
        if(num != "best" & num != "worst")
            NA
        else {
            if(num == "best")
                head(rankedList$Hospital.Name,1)
            else
                tail(rankedList$Hospital.Name,1)
        }
    }
}