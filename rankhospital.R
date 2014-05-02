rankhospital <- function(state,outcome,num) {
  
  tolower(num)
  if (num == "best"){
    num <- 1
    num <- as.numeric(num)
  }
  
  outcome_frame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  unique_states <- unique(outcome_frame[, 7])
  
  if (is.element(state, unique_states) == "TRUE" ) {
    subset <- outcome_frame[(outcome_frame$State == state),]
    } else {
    stop("invalid state")
  }
  
  
  if (outcome == "heart attack") {
    sort.state <- subset[order(as.numeric(subset[, 11]), subset[, 2]), ]
    sort.state <- sort.state[!sort.state[,11] %in% "Not Available", ] 
    max <- nrow(sort.state)
    if (num == "worst") { num <- max }
    if (num > max) { output <- NA; return(output) }
    output <- sort.state[num, 2]
    output
  }
  else if(outcome == "heart failure") {
    sort.state <- subset[order(as.numeric(subset[, 17]), subset[, 2]), ]
    sort.state <- sort.state[!sort.state[,17] %in% "Not Available", ] 
    max <- nrow(sort.state)
    if (num == "worst") { num <- max }
    if (num > max) { output <- NA; return(output) }
    output <- sort.state[num, 2]
    output
  }
  else if (outcome == "pneumonia") {
    sort.state <- subset[order(as.numeric(subset[, 23]), subset[, 2]), ]
    sort.state <- sort.state[!sort.state[,23] %in% "Not Available", ] 
    max <- nrow(sort.state)
    if (num == "worst") { num <- max }
    if (num > max) { output <- NA; return(output) }
    output <- sort.state[num, 2]
    output
    
  } else { 
    stop("invalid outcome") 
  }
}

