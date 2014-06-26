rankhospital <- function(state, outcome, num = "best") {
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
    data <- subset(data, 
                   State == state, 
                   select = c(1, 2, outcome.id))
    # remove NAs in mortality rate
    data <- data[complete.cases(data[, 3]), ]
    # order by mortality rate and hospital name
    data <- data[order(data[,3], data[,1]), ]
    
    # return hospital name in that state with the given rank 30-day death rate
    ifelse(num == "best", data[1, 1], 
           ifelse(num == "worst", data[nrow(data), 1], 
                  ifelse(num > nrow(data), NA, data[num, 1])))
}