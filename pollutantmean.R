pollutantmean <- function(directory, pollutant, id = 1:332) {
    means <- NA
    filenames <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    for(i in id) {
        data <- read.csv(filenames[i])
        if(pollutant == "sulfate") {
            means <- c(means, data$sulfate)
        }
        else {
            means <- c(means, data$nitrate)
        }
    }
    mean(means, na.rm = TRUE)
}