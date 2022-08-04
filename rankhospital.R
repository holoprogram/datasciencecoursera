rankhospital <- function(state, outcome, num = "best") { 
        ## Read outcome data
        data <- read.csv("datasets/outcome-of-care-measures.csv" ,colClasses = "character", na.strings = "Not Available")
        data[, 11] <- as.numeric(data[, 11]) # Heart Attack
        data[, 17] <- as.numeric(data[, 17]) # Heart Failure
        data[, 23] <- as.numeric(data[, 23]) # Pneumonia
        
        stt <- unique(data$State)
        outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
        Error = 0
        ## Check that state and outcome are valid
        if (!outcome%in%names(outcomes)){
                Error = 1
                stop("invalid outcome")
        } 
        
        else if (!state%in%stt){
                Error = 2
                stop("invalid State")
        }

        
        if(!Error){
                dr <- subset(data, select = c(Hospital.Name, State, outcomes[outcome]))
                names(dr) <- c("hospital_name", "state", "rate")
                
                
                ## Return hospital name in that state with the given rank
                c <- as.data.frame(split(dr, dr$state)[state])
                names(c) <- c("hospital_name", "state", "rate")
                c <- c[order(c$rate, c$hospital_name), ]
                d <- tail(na.omit(c), n=1)
                
                
                if(num=="best"){
                        result <- as.character(c[1, 1])
                }
                else if(num=="worst"){
                        result <- as.character(d[1,1])
                }
                else if(is.numeric(num)){
                        result <- as.character(c[num, 1])
                }
                else{
                        stop("invalid number")
                }
                result

                
        } 
        
        

        
}