complete <- function(directory, id = 1:332) {
 
  
  thisDir = getwd()
  
  numID <- length(id)
  numObs = data.frame(id=c(),nobs=c())
  
  for (i in 1:numID) {
    
    if (id[i] < 10) {thisID <- paste("00",id[i],sep="")}
    else if (id[i] < 100) {thisID <- paste("0",id[i],sep="")}
    else {thisID <- id[i]}
    
    fileName <- paste(thisDir,"/",directory,"/",thisID,".csv",sep="")
    thisData = read.csv(fileName)
    
    thisNum <- length(thisData[complete.cases(thisData),1])
    #if (thisNum > 0) {
      numObs = rbind(numObs,c(id[i],thisNum))
    #}
  
  }    
  
  names(numObs) <- c("id","nobs")
  setwd(thisDir)
  return(numObs)
  
     ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
    
    
}