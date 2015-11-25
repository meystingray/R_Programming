pollutantmean <- function(directory, pollutant, id =  1:332) {
  
  thisDir = getwd()
  monitorDataSum <- c()
  
  numID <- length(id)

  for (i in 1:numID) {
 
    if (id[i] < 10) {thisID <- paste("00",id[i],sep="")}
    else if (id[i] < 100) {thisID <- paste("0",id[i],sep="")}
    else {thisID <- id[i]}

    fileName <- paste(directory,"/",thisID,".csv",sep="")
    thisData = read.csv(fileName)

    if (pollutant == "sulfate") {
      monitorDataSum <- c(monitorDataSum,thisData$sulfate[!is.na(thisData$sulfate)])
      }
    else if (pollutant == "nitrate") {
      monitorDataSum <- c(monitorDataSum,thisData$nitrate[!is.na(thisData$nitrate)])
      }

  }
  
  return(mean(monitorDataSum))
  
  setwd(thisDir)
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
}