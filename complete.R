complete <- function(directory, id = 1:332) {
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
  
  fileNames <- sprintf("%03d.csv", id)
  filePaths <- paste(getwd(),"/",directory,"/",fileNames,sep="")
  
  dataset <- data.frame()
  results <- c()
  
  for (i in 1:length(filePaths)){
    dataset <- read.csv(filePaths[i])
    completeDS <-subset(dataset,!is.na(dataset["sulfate"]) & !is.na(dataset["nitrate"]))
    results<- c(results,id[i],nrow(completeDS))	
  }
  
  final <- matrix(results,nrow=length(filePaths),ncol=2,byrow=TRUE)
  
  rownames(final)<-c(1:length(id))
  colnames(final)<-c("id","nobs")
  return(as.data.frame(final)) 
  
}
