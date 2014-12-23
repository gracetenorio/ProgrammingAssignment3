## --------------------------------------------------------------------------------------------------------------------
##
## Function:  best
## Input:     Quality of care data from over 4,000 Medicare-certified hospitals in the United States.
##            Source of data is the Hospital Compare website run by the U.S. Department of Health and Human Services.
## Output:    Best hospital for a given condition (either "heart attack", "heart failure" or "pneumonia") in the
##            specified state.
##
## --------------------------------------------------------------------------------------------------------------------

best <- function(state, outcome) {
    
    ## read outcome data and check for validity of input
    ds <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    if (!state   %in% ds$State)   stop("invalid state")
    if (!outcome %in% c("heart attack","heart failure","pneumonia")) stop("invalid outcome")
    
    
    ## subset and format data
    col_idx <- if(outcome == 'heart attack')  { 11 }  else if (outcome == 'heart failure') { 17 }   else { 23 }   
    sub <- ds[ds$State==state,c(2,col_idx)]
    suppressWarnings(sub[,2] <- as.numeric(sub[,2]))
    
    
    ## return hospital with lowest mortality rate
    ## note: only hospitals with data for outcome are considered for ranking
    ranks <- sub[order(sub[,2],sub[,1],na.last=NA),]
    ranks[1,1]
}

