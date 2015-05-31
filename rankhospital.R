rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  if (!(state %in% outcome_data[,7])) stop("Invalid state")
  outcome_num <- switch(outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23, 0)
  if (outcome_num == 0) stop("Invalid outcome")
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  ## filter data for state
  state_data  <- outcome_data[outcome_data[['State']]==state,]
  
  ##filter data to eliminate "not available" values
  state_clean <- state_data[state_data[,outcome_num]!="Not Available",]
  
  ## derive num value 
  num <- switch(as.character(num), "best"=1, "worst"=nrow(state_clean), num)
  
  ## check for num > data rows
  if(num>nrow(state_clean)) NA else
  ## sort data 
  as.character(state_clean[order(state_clean[,outcome_num],state_clean[[2]])[num],2])
}