best <- function(state,outcome) {
  
  outcome_frame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  unique_states <- unique(outcome_frame[, 7])
  
  if (is.element(state, unique_states) == "TRUE" ) {
    subset <- outcome_frame[(outcome_frame$State == state),]
  } else {
    stop("invalid state")
  }
    
  if (outcome == "heart attack") {
    sort.state <- subset[order(as.numeric(subset[, 11]), subset[, 2]), ]
    output <- sort.state[1, 2]
    output
  }
  else if(outcome == "heart failure") {
    sort.state <- subset[order(as.numeric(subset[, 17]), subset[, 2]), ]
    output <- sort.state[1, 2]
    output
  }
  else if (outcome == "pneumonia") {
    sort.state <- subset[order(as.numeric(subset[, 23]), subset[, 2]), ]
    output <- sort.state[1, 2]
    output
    
  } else { 
    Stop("invalid outcome") 
  }
}
