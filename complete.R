## my first answer to function 2
complete <- function(directory, id=1:332){
        ## 'directory' is a character vector of length 1
        ## indicating the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the 
        ## number of complete cases
        
        files_full <- list.files(directory, full.names = TRUE, pattern = "csv")
        dat <- data.frame()
        for(i in id){
                dtread <- read.csv(files_full[i])
                
                
                # This is where it goes wrong, complete cases means rows that 
                # sulfate and nitrate both have values, but there I just 
                # calculate on,which will get more
                
                # c <- dtread[, 'sulfate']
                # gn <- !is.na(dtread[, 'sulfate']) 
                
                # right anwser is
                nobs <- sum(complete.cases(dtread))
                dat <- rbind(dat,data.frame(i,nobs))
                
        }
        
        ## set colnames
        colnames(dat) <- c('id', 'nobs')
        dat

}
# thanks to complete.cases() 
