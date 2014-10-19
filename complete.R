load.poll.data <- function(directory, id = 1:332) 
{
    lapply(id, 
           function(id)
               read.csv(
                    paste0(directory, "/", formatC(id, width=3, flag="0"), ".csv")
               )
    )    
}
    
complete.frame <- function(frame) 
{
    sum(
        apply(frame, 1,
              function(row) 
                  if (!is.na(row["nitrate"]) && !is.na(row["sulfate"])) 1 else 0
        )
    )
}

complete <- function(directory, id = 1:332)
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    observations <- load.poll.data(directory, id) 
    
    data.frame(id=id, nobs=sapply(observations, complete.frame))
}