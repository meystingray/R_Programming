corr <- function(directory, threshold = 0) {

  thisWD <- getwd()
  
  gd <- paste(thisWD,"/",directory,sep="")
  setwd(gd)
  dirSet <- dir()
  corVector <- c()
  
  for (i in 1:length(dirSet)) {
    
    thisID <- dirSet[i]
    fileName <- paste(thisWD,"/",directory,"/",dirSet[i],sep="")
    thisData <- read.csv(fileName)
    cc <- complete.cases(thisData)
    numcc <- length(cc[cc==TRUE])

    if (numcc > threshold) {
      thisData <- thisData[cc,]
      thisCor <- cor(thisData$nitrate, thisData$sulfate)
      corVector <- rbind(corVector,thisCor)
    }
  }

  
  setwd(thisWD)  
  return(corVector)

    ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}