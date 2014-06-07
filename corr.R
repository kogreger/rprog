corr <- function(directory, threshold = 0) {
    correlations <- vector(mode = "integer", length = 0)
    filenames <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    for(i in 1:332) {
        data <- read.csv(filenames[i])
        ok <- complete.cases(data)
        if(sum(ok) > threshold) {
            correlations <- c(correlations, 
                              cor(data$sulfate, 
                                  data$nitrate, 
                                  use = "complete.obs"))
        }
    }
    return(correlations)
}