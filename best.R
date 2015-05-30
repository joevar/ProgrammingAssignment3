best <- function(state, outcome) {

  ## Read outcome data
  outcome_data = read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings="Not Available")
  
  ## Check that state are valid  
  if(!(state %in% outcome_data$State)) { 
    stop("Invalid state")
  }
  
  ## Check that outcome are valid  
  outcome_num <- switch(outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23, 0)
  if (outcome_num == 0) stop("Invalid Outcome")
  
  ## filter for state
  state_data <-outcome_data[outcome_data$State==state,]
  
  ## derive the row with minimum value
  min_row <- which.min(as.numeric(state_data[,outcome_num]))
  
  ## Return hospital name in that state with lowest 30-day death rate
  state_data[min_row,"Hospital.Name"]
}