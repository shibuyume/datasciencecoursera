corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  fileNames<-list.files(paste(getwd(),"/",directory,sep=""),full.names=TRUE,pattern="*.csv")
  dataset<-data.frame()
  
  v<-c()
  answer<-c()
  
  for (i in 1:length(fileNames)) {
    
    dataset <- rbind(dataset,read.csv(fileNames[i]))
    cleanDS<-na.omit(dataset)
    
    dataByID <- subset(cleanDS, cleanDS[,4]==i)
    numrows <- nrow(dataByID)
    
    if (numrows > threshold) {
      correlationByID <- cor(dataByID$sulfate, dataByID$nitrate)
      
      answer <- c(answer, correlationByID)
    }
  }
  return (answer)
}
