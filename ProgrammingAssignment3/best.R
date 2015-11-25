best <- function(state,outcome) {
  
  ## Read outcome data
  outcomeFrame <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  statelist <- unique(outcomeFrame$State)
  if (!state %in% statelist) {stop("invalid state")}
  outcomelist <- c("heart attack","heart failure","pneumonia")
  if (!outcome %in% outcomelist) {stop("invalid outcome")}

  ## Return hospital name in that state with lowest 30-day death rate
  thisStateFrame <- outcomeFrame[outcomeFrame$State == state,]
  colNum <- c()
  if (outcome == outcomelist[1]) {colNum <- 11} # heart attack
  if (outcome == outcomelist[2]) {colNum <- 17} # heart failure
  if (outcome == outcomelist[3]) {colNum <- 23} # pneumonia
  
  # keep only the relevant columns
  thisStateFrame[,colNum] <- suppressWarnings(as.numeric(thisStateFrame[,colNum]))
  thisStateFrame <- thisStateFrame[,c(2,colNum)]
  
  # delete all NA's
  thisStateFrame <- thisStateFrame[(!thisStateFrame[,2] == "Not Available"),]
  
  row <- which.min(thisStateFrame[,2])
  
  hospitalName <- thisStateFrame[row,1]
  
  return(hospitalName)
}