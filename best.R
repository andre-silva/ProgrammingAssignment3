best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    
    ## Check that state and outcome are valid
    if(!is.element(outcome, c("heart attack","heart failure","pneumonia"))) {
        stop("invalid outcome")
    }
    
    if(!is.element(state, unique(data$State))) {
        stop("invalid state")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    #First check relevant column
    if(outcome == "heart attack")
        col <- 11
    
    if(outcome == "heart failure")
        col <- 17
    
    if(outcome == "pneumonia")
        col <- 23
    
    ## Transform outcome to numeric and remove NAs
    data[,col] <- suppressWarnings(as.numeric(data[,col]))
    data <- data[data$State==state & !is.na(data[,col]),]
    
    ## Get minimum value for relevant outcome
    hospitalNames <- data$Hospital.Name[data[,col]==min(data[,col])]
    
    sort(hospitalNames)[1]
}
