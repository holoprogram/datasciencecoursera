## My answer to the function 3

corr <- function(directory, threshold = 0){
        ## 'threshold' is a numeric vector of 1 indicating the
        ## number of completely observed observations(on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        ## (e.g., if threshold is 150, then 
        ## the condition on each file is having at least 151 complete cases)
        files_full <- list.files(directory, full.names = TRUE, pattern = "csv")
        dat <- data.frame()
        r <- vector()
        for(i in 1:332){
                dtread <- read.csv(files_full[i])
                nobs <- sum(complete.cases(dtread))
                if(nobs > threshold){
                        my_cor <- cor(dtread[,'sulfate'], dtread[, 'nitrate'], "complete")
                        # result
                        r <- c(r, my_cor)
                }
        }
        r
}

