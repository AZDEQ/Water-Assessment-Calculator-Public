csvFrom_s3 <- function(s3_bucket,csvFile){
  #textString = s3_bucket+csvFile
  #textString = str(s3_bucket+csvFile)
  bucketString = paste(s3_bucket,csvFile,sep="")
  dataObject <-get_object(bucketString)
  csvcharobj <- rawToChar(dataObject)
  datacon <- textConnection(csvcharobj)
  dataName <- read.csv(datacon)
  dataObject = NULL
  csvcharobj = NULL
  datacon = NULL
  return(dataName)
}