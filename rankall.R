## --------------------------------------------------------------------------------------------------------------------
##
## Function:  rankall
## Input:     Quality of care data from over 4,000 Medicare-certified hospitals in the United States.
##            Source of data is the Hospital Compare website run by the U.S. Department of Health and Human Services.
## Output:    Hospitals with the specified ranking for a given condition (either "heart attack", "heart failure" or 
##            "pneumonia") across all states.
##
## --------------------------------------------------------------------------------------------------------------------

rankall <- function(outcome, num="best") {
    
    ## read outcome data and check for validity of input
    ds <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    if (!outcome %in% c("heart attack","heart failure","pneumonia")) stop("invalid outcome")
    
    
    ## subset and format data
    col_idx <- if(outcome == 'heart attack')  { 11 } else if (outcome == 'heart failure') { 17 } else { 23 }   
    sub <- ds[,c(2,7,col_idx)]
    suppressWarnings(sub[,3] <- as.numeric(sub[,3]))
    
    
    ## create empty dataframe that will hold output data
    outdf <- data.frame(Hospital.Name=character(0),State=character(0),stringsAsFactors=FALSE)
    
    
    ## for each state, rank hospitals by mortality rate
    for (i in unique(sub$State)) {
        
        sub_state <- sub[sub$State==i,]
        ranks <- sub_state[order(sub_state[,3],sub_state[,1],na.last=NA),]
        ranks["hosp_rank"] <- c(1:nrow(ranks))
        
        
        ## append to output dataframe based on value of new_num
        new_num <- if (num=="best") { 1 } else if (num=="worst") { nrow(ranks) } else { num }
        ranks_avail <- ranks$hosp_rank
        
        
        if(new_num %in% ranks_avail) { outdf <- rbind(outdf,ranks[ranks$hosp_rank==new_num,c(1,2)]) } 
        else { outdf <- rbind(outdf,data.frame(Hospital.Name="<NA>",State=i)) }
        
    }
    
    ## format output dataframe
    outdf_final <- outdf[order(outdf[2]),]
    names(outdf_final)[1] <- "hospital"; names(outdf_final)[2] <- "state"
    row.names(outdf_final) <- outdf_final$state
    outdf_final
}

