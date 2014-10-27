rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    
    ## Check that state and outcome are valid
    if(!is.element(outcome, c("heart attack","heart failure","pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    ## First check relevant column
    if(outcome == "heart attack")
        col <- 11
    
    if(outcome == "heart failure")
        col <- 17
    
    if(outcome == "pneumonia")
        col <- 23
    
    ## Transform outcome to numeric and remove NAs
    data[,col] <- suppressWarnings(as.numeric(data[,col]))
    data <- data[!is.na(data[,col]),]
    
    rankedList <- by(data,data$State,function(x) 
                                    x[order(x[,col],x$Hospital.Name),2])
    
    stateRank <- sapply(rankedList,getRank,num)
    data.frame(hospital=stateRank,state=names(stateRank),
               row.names=names(stateRank))
}

getRank <- function(rankedList, num) {
    ## if the number is an integer
    if(!is.na(suppressWarnings(as.integer(num)))) {
        if(num > length(rankedList))
            NA
        else
            rankedList[num]
    } else {
        if(num != "best" & num != "worst")
            NA
        else {
            if(num == "best")
                head(rankedList,1)
            else
                tail(rankedList,1)
        }
    }
}
