# Configuration values
startDate <- as.Date("2012-07-01")
endDate <- as.Date("2021-06-30") # negotiating 5/1/21 end date


# credentials to access s3

# AWS s3 buckets
s3Bucket <- "s3://<bucket name>/"
s3_epa_bucket <-"s3://<bucket name>/"
# s3 bucket for input files - versioning enabled
s3_csv_bucket <-"s3://<bucket name>/"

# Dialog to ask User if they want to use (requires - library(svDialogs):
# Oracle
# s3 for CSV files
# s3 for EPA files
# write results to mySQL

user.s3CSV <- dlgInput("Use s3 bucket as source for config for CSV and .Rdata files - y/n:", default = "y", Sys.info()["y/n"])$res
user.s3EPA <- dlgInput("Use EPA data from s3 schedule nightly - y/n:", default = "y", Sys.info()["y/n"])$res
user.useLocalEPA <- dlgInput("Use saved local copy of EPA data for testing - y/n:",default = "n", Sys.info()["y/n"])$res
user.useOracle <- dlgInput("Use Oracle to pull some config data y/n:", default = "y", Sys.info()["y/n"])$res
user.writeDB <- dlgInput("Write results to mySQL DB y/n:", default = "n", Sys.info()["y/n"])$res



connSuccess <- "no"

if(user.useOracle == "y"){
  # Make Oracle Connection
  #Oracle credentials
  oracleUserID <- "<oracle user>"
  oraclePass <- "<password>"
  oracleDB <- "<database>"
  
  conn <- makeConnection(oracleDB,oracleUserID,oraclePass)
  # odbcGetInfo(conn)
  
  if(conn){
    # message("say hello to success!")
    # On successful connection to Oracle, set 'useOracle' flag to 'yes'
    connSuccess <- "yes"
  } else {
    connSuccess <- "no"
  }

}




