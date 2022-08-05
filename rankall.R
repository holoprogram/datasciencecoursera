rankall <- function(outcome, num = "best") { 
        ## Read outcome data ## Check that state and outcome are valid 
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the 
        ## (abbreviated) state name
        
        data <- read.csv("datasets/outcome-of-care-measures.csv" ,colClasses = "character", na.strings = "Not Available")
        data[, 11] <- as.numeric(data[, 11]) # Heart Attack
        data[, 17] <- as.numeric(data[, 17]) # Heart Failure
        data[, 23] <- as.numeric(data[, 23]) # Pneumonia
        
        
        outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
        Error = 0
        ## Check that state and outcome are valid
        if (!outcome%in%names(outcomes)){
                Error = 1
                stop("invalid outcome")
        } 
        
        
        if(!Error){
                ## final answer
                # final <- data.frame()
                ## get the main cols
                dr <- subset(data, select = c(Hospital.Name, State, outcomes[outcome]))
                
                names(dr) <- c("hospital_name", "state", "rate")
                
                
                ## rank hospital in each state
                splitData <- split(dr, dr$state)
                rankfunc <- function(data,d=FALSE){
                        data <- data[order(data$rate, data$hospital_name, decreasing = d), ]
                        result <- data$hospital_name[num]
                        result
                }
                
                
                if(num=="best"){
                        num <- 1
                        result_list <- lapply(splitData, rankfunc)
                        # final <- cbind.data.frame(unlist(result_list), names(result_list))
                        final <- data.frame(hospital = unlist(result_list)
                                            ,state = names(result_list)
                                            )
                        # names(final) <- c("hospital", "State")
                }
                else if(num=="worst"){
                        num <- 1
                        result_list <- lapply(splitData, rankfunc, d=TRUE)
                        #final <- cbind.data.frame(unlist(result_list), names(result_list))
                        #names(final) <- c("hospital", "State")
                        final <- data.frame(hospital = unlist(result_list)
                                            ,state = names(result_list)
                                            )
                        
                        
                }
                else if(as.numeric(num)){
                        result_list <- lapply(splitData, rankfunc)
                        #final <- cbind.data.frame(unlist(result_list), names(result_list))
                        #names(final) <- c("hospital", "State")
                        final <- data.frame(hospital = unlist(result_list)
                                            ,state = names(result_list)
                                            )
                }
                else{stop("invalid outcome")}
                final
        }
}
