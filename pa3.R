setwd("~/Documents/Coursera/Data Science Specialization/R Programming/rprog/")

## P1
outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", 
                    colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


## P2
source("best.R")
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()


## P3
source("rankhospital.R")
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()


## P4
source("rankall.R")
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()