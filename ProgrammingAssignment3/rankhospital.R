rankhospital <- function(state,outcome,num) {

  ## Read outcome data
  outcomeFrame <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  statelist <- unique(outcomeFrame$State)
  if (!state %in% statelist) {stop("invalid state")}
  outcomelist <- c("heart attack","heart failure","pneumonia")
  if (!outcome %in% outcomelist) {stop("invalid outcome")}
  
  ## Return hospital name in that state with lowest 30-day death rate
  # Get the hospitals only in that state
  thisStateFrame <- outcomeFrame[outcomeFrame$State == state,]

  colNum <- c()
  if (outcome == outcomelist[1]) {colNum <- 11} # heart attack
  if (outcome == outcomelist[2]) {colNum <- 17} # heart failure
  if (outcome == outcomelist[3]) {colNum <- 23} # pneumonia
  
  # keep only the relevant columns
  thisStateFrame[,colNum] <- suppressWarnings(as.numeric(thisStateFrame[,colNum]))
  thisStateFrame <- thisStateFrame[,c(2,colNum)]
  names(thisStateFrame) <- c("one","two")
  
  # delete all NA's
  thisStateFrame <- thisStateFrame[(!thisStateFrame[,2] == "Not Available"),]
  thisStateFrame <- thisStateFrame[!is.na(thisStateFrame[,2]),]
  
  thisStateFrame <- thisStateFrame[order(thisStateFrame[,2],thisStateFrame[,1],decreasing = FALSE),]
  #print(head(thisStateFrame))
  
  if (num == "best") {num = 1}
  if (num == "worst") {num = nrow(thisStateFrame)}

  if (nrow(thisStateFrame) < num) {
    return(NA)
  } else {
      return(thisStateFrame[num,1])
  }
}