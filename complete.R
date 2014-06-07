complete <- function(directory, id = 1:332) {
    completeCases <- vector(mode = "integer", length = 0)
    filenames <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    for(i in id) {
        data <- read.csv(filenames[i])
        ok <- complete.cases(data)
        completeCases <- c(completeCases, sum(ok))
    }
    return(data.frame(id = id, nobs = completeCases))
}