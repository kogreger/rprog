setwd("~/Documents/Coursera/Data Science Specialization/R Programming/rprog/")

## P1
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

pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
pollutantmean("specdata", "nitrate", 23)
## [1] 1.281


## P2
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

complete("specdata", 1)
##   id nobs
## 1  1  117
complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
complete("specdata", 3)
##   id nobs
## 1  3  243


## P3
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

cr <- corr("specdata", 150)
head(cr)
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
cr <- corr("specdata", 400)
head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
length(cr)
## [1] 0
cr <- corr("specdata")
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
length(cr)
## [1] 323


## Submit
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")
submit()
