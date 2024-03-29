corr <- function(directory, threshold = 0) 
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    obs <- load.poll.data(directory)
    
    complete.count <- sapply(obs, complete.frame)
    
    res <- numeric()
    for (i in 1:length(obs)) 
    {
        if (complete.count[i] >= threshold) 
        {
            res <- c(res, cor(obs[[i]]$sulfate, obs[[i]]$nitrate, use="na.or.complete"))
        }
    }
    res
}
