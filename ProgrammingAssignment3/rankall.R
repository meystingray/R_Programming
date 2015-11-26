rankall <- function(outcome,num) {
  
  ## Read outcome data
  outcomeFrame <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  statelist <- unique(outcomeFrame$State)

  numStates = length(statelist)
  
  outcomelist <- c("heart attack","heart failure","pneumonia")
  if (!outcome %in% outcomelist) {stop("invalid outcome")}
  
  ## Return hospital name in that state with lowest 30-day death rate
  # Get the hospitals only in that state

  colNum <- c()
  if (outcome == outcomelist[1]) {colNum <- 11} # heart attack
  if (outcome == outcomelist[2]) {colNum <- 17} # heart failure
  if (outcome == outcomelist[3]) {colNum <- 23} # pneumonia

  
  # keep only the relevant columns
  outcomeFrame <- outcomeFrame[,c(2,7,colNum)]
  outcomeFrame[,3] <- suppressWarnings(as.numeric(outcomeFrame[,3]))

  
  # delete all NA's
  outcomeFrame <- outcomeFrame[(!outcomeFrame[,3] == "Not Available"),]
  outcomeFrame <- outcomeFrame[!is.na(outcomeFrame[,3]),]

  returnFrame <- c()
  

  for (i in 1:numStates) {

    thisState = statelist[[i]]

    thisStateFrame <- outcomeFrame[outcomeFrame[,2] == thisState,]

    thisStateFrame <- thisStateFrame[order(thisStateFrame[,3],thisStateFrame[,1],decreasing = FALSE),]
    print(thisStateFrame)
    cat ("Press [enter] to continue")
    line <- readline()
    
    if (num == "best") {num = 1}
    if (num == "worst") {num = nrow(thisStateFrame)}
  
    if (nrow(thisStateFrame) < num) {
      returnFrame <- rbind(returnFrame,c(NA,thisState))
    } else {returnFrame <- rbind(returnFrame,c(thisStateFrame[num,1],thisState))
    }
    
  } # end for loop
  
  colnames(returnFrame) <- c("hospital","state")
  rownames(returnFrame) <- returnFrame[,2]
  returnFrame <- returnFrame[order(returnFrame[,2]),]
  returnFrame <- as.data.frame(returnFrame)
  return(returnFrame)
  
  
} # end function