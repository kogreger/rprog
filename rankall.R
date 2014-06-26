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
    
    # check that outcome is valid
    ifelse(sum(outcome %in% outcomes$name), 1, stop("invalid outcome"))
    
    # reduce to relevant columns
    outcome.id <- as.numeric(as.character(outcomes[outcomes$name == outcome, ]$id))
    data <- data[, c(1, 2, outcome.id)]
    # remove NAs in mortality rate
    data <- data[complete.cases(data[, 3]), ]
    
    # return a data frame with the hospital names and the abbr. state name
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
        rank <- ifelse(num == "best", 1, 
                       ifelse(num == "worst", nrow(state.table), num))
        print(paste(state, rank, nrow(state.table), sep = ": "))
        name <- c(name, ifelse(rank > nrow(state.table), 
                               NA, 
                               state.table[rank, ]$Hospital.Name))
    }
    return(data.frame(hospital = name, state = frequency$State))
}