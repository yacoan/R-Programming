rankall <- function(outcome, num = "best") {
  
  tolower(num)
  if (num == "best"){
    num <- 1
    num <- as.numeric(num)
  }
  
  if (outcome %in% c("heart attack","heart failure","pneumonia")) {
    
    outcome_frame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    unique_states <- unique(outcome_frame[, 7])
    
    df <- data.frame(matrix(ncol = 2, nrow = length(unique_states)))
    names(df) <- c("hospital","state")
    n <- 1
    
    for(i in unique_states){
      
      df[n,2] <- i
      subset <- outcome_frame[(outcome_frame$State == i),]
      if (outcome == "heart attack") {
        sort.state <- subset[order(as.numeric(subset[, 11]), subset[, 2]), ]
        sort.state <- sort.state[!sort.state[,11] %in% "Not Available", ] 
      }
      else if(outcome == "heart failure") {
        sort.state <- subset[order(as.numeric(subset[, 17]), subset[, 2]), ]
        sort.state <- sort.state[!sort.state[,17] %in% "Not Available", ] 
      }
      else if (outcome == "pneumonia") {
        sort.state <- subset[order(as.numeric(subset[, 23]), subset[, 2]), ]
        sort.state <- sort.state[!sort.state[,23] %in% "Not Available", ] 
      } else { 
        stop("invalid outcome") 
      }
        
  
      max <- nrow(sort.state)
      if (num == "worst") { 
        temp_num <- max 
      } else if (num > max) { 
        output <- "NA"
        #return(output) 
      } else { 
        temp_num <- num 
      }
      
      output <- sort.state[temp_num, 2]
      df[n,1] <- output
      n <- n + 1
      #print(i)
      
    }   
    df <- df[order(df$state),]
    return(df)
  
  } else {
    stop("invalid outcome")
  }
}
