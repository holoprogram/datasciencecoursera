best <- function(state, outcome){ 
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
            
        if (!state%in%stt){
                Error = 2
                stop("invalid State")
        }
        
        if(!Error){
                # factor changed, not numeric
                #dr <- as.data.frame(cbind(data$Hospital.Name, data$State
                #                          , data[,outcomes[outcome]]))
                
                # subset hold the type ,data.frame ,and not change the numeric
                dr <- subset(data, select = c(Hospital.Name, State, outcomes[outcome]))
                
                names(dr) <- c("hospital_name", "state", "rate")
        } 
                
        ## Return hospital name in that state with lowest 30-day death
        c <- as.data.frame(split(dr, dr$state)[state])
        names(c) <- c("hospital_name", "state", "rate")
        # asnumber <- as.numeric(c$rate)
        # c[,3] <- as.numeric(c[, 3]) 
        c <- c[order(c$rate, c$hospital_name), ]
        return(c)
        
        # I went around in a big circle to deal with tie breaker
        # testtie <- unique(c$rate)
        # 
        # ## rate and tie
        # result <- as.character(c[1, 1])
        # # 
        # ## you can also use unique and compare the row length to check 
        # ## if there is atie
        # 
        # if(dim.data.frame(c)[1] == length(testtie)){
        #         return(result)
        # }
        # else{
        #         mint <- min(c$rate, na.rm=TRUE)
        #         d <- sapply(c$rate, function(x){identical(x ,mint)})
        #         ## how to deal with ties look the pointers in Coursera
        #         # get_tie <- which(c$rate == min(c$rate, na.rm = TRUE)
        #         #                 , arr.ind=TRUE)
        #         ## get these rows
        #         ## there is a easy way,just order twice, like order(df$outcome.values, df$hnames)
        #         tie_rows <- c[d, ]
        #         tie_rows <- tie_rows[order(tie_rows$hospital_name), ]
        #         gname <- as.character(tie_rows[1, 1])
        #         return(gname)
        # 
        # }
        # # result
        # # substr(what, 1, 1)
        # ## if there are tiessys
}
