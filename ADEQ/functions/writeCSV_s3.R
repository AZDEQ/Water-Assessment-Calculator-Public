# write csv file to s3

csvTo_s3 <- function(dataFrame,s3_bucket,fileName,folder){
  zz <- rawConnection(raw(0),"r+")
  write.csv(dataFrame,zz)
  if (folder=='none') {
    fileString = fileName
  } else {
    fileString = paste(folder,"/",fileName,sep="")
  }
  aws.s3::put_object(file = rawConnectionValue(zz), bucket= s3_bucket, object = fileString)
  close(zz)
}





