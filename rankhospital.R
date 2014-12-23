## -----------------------------------------------------------------------------------------------------------------------
##
## Function:  rankhospital
## Input:     Quality of care data from over 4,000 Medicare-certified hospitals in the United States.
##            Source of data is the Hospital Compare website run by the U.S. Department of Health and Human Services.
## Output:    Hospital with the specified ranking for a given state and condition (either "heart attack", "heart failure" 
##            or "pneumonia").
## 
## -----------------------------------------------------------------------------------------------------------------------

rankhospital <- function(state, outcome, num="best") {
    
    ## read outcome data and check for validity of input
    ds <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    if (!state   %in% ds$State)   stop("invalid state")
    if (!outcome %in% c("heart attack","heart failure","pneumonia")) stop("invalid outcome")
    
    
    ## subset and format data 
    col_idx <- if(outcome == 'heart attack')  { 11 }   else if (outcome == 'heart failure') { 17 }  else { 23 }   
    sub <- ds[ds$State==state,c(2,col_idx)]
    suppressWarnings(sub[,2] <- as.numeric(sub[,2]))
    
    
    ## rank hospitals based on mortality rates (smaller=better)
    ## note: only hospitals with data for outcome are considered for ranking
    ranks <- sub[order(sub[,2],sub[,1],na.last=NA),]
    ranks["hosp_rank"] <- c(1:nrow(ranks))
    
    
    ## return hospital name with rank "num"
    if (num=="best") { ranks[ranks$hosp_rank==1,1] }
    else if (num=="worst") { ranks[ranks$hosp_rank==nrow(ranks),1] }
    else if (num > nrow(ranks) | num <= 0) { NA }
    else { ranks[ranks$hosp_rank==num,1] }
    
}

