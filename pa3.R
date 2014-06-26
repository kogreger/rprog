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
rankall <- function(outcome, num = "best") {
    # read outcome data
    data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", 
                     colClasses = "character")
    # initialize outcome data
    outcomes <- as.data.frame(cbind(3:5, 
                                    c("heart attack", "heart failure", "pneumonia")))
    names(outcomes) <- c("id", "name")
    # reduce to relevant columns
    data <- data[, c(2, 7, 11, 17, 23)]
    # coerce outcome rates to numeric
    data[, 3] <- as.numeric(data[, 3]) # heart attack
    data[, 4] <- as.numeric(data[, 4]) # heart failure
    data[, 5] <- as.numeric(data[, 5]) # pneumonia
    
    # check that state and outcome are valid
    ifelse(sum(state %in% data$State), 1, stop("invalid state"))
    ifelse(sum(outcome %in% outcomes$name), 1, stop("invalid outcome"))
    
    # reduce to relevant columns
    outcome.id <- as.numeric(as.character(outcomes[outcomes$name == outcome, ]$id))
    data <- data[, c(1, 2, outcome.id)]
    # remove NAs in mortality rate
    data <- data[complete.cases(data[, 3]), ]
    
    # return a data frame with the hospital names and the abbr. state name
    ifelse(num > nrow(data), return(NA), 1)
    rank <- ifelse(num == "best", 1, 
                   ifelse(num == "worst", nrow(data), num))
    
    frequency <- data.frame(State = names(tapply(data$State, 
                                             data$State, 
                                             length)), 
                        Freq = tapply(data$State, 
                                      data$State, 
                                      length))
    rownames(frequency) <- NULL
    name <- character(0)
    
    for(state in frequency$State) {
        state.table <- data[data$State == state, ]
        state.table <- state.table[order(state.table[, 3], 
                                         state.table$Hospital.Name), ]
        name <- c(name, state.table[rank, ]$Hospital.Name)
    }
    return(data.frame(hospital = name, state = frequency$State))
}