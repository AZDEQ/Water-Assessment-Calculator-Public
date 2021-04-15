# Read configuration files

# working with .Rdata files
# r code save to locatl dir
# save(data1, file = "data.RData")
# To load the data again
# load("data.RData")
# save_object() saves an S3 object to a specified local file without reading it into memory
# 
# save an in-memory R object into S3
# s3save(mtcars, bucket = "my_bucket", object = "mtcars.Rdata")
# 
# # `load()` R objects from the file
# s3load("mtcars.Rdata", bucket = "my_bucket")

# If an Oracle connection has been made (useOracle = 'yes') then read from Oracle
# and write to CSVs.  Otherwise, use the CSVs that are in place


# Check to see if there are any new/updated files in 'newCSVs' folder 
# If so, copy to s3 and delete from local folder
files <- list.files(path = "~/projects/WaterCalculator/ADEQ/newCSVs", pattern = ".csv", all.files=T, full.names=F)  

# for each file in the newCSVs folder, copy to s3 bucket
for (fileName in files) {
  # print(fileName)
  # Now that the print works, copy to s3 bucket
  # read into Dataframe
  filePathString <- paste("newCSVs/", fileName,sep="")
  tempDF <- read_csv(filePathString)
  csvTo_s3(tempDF,s3_csv_bucket,fileName,"inputs")
  #delete from newCSVs folder
  if (file.exists(filePathString)) {
    #Delete file if it exists
    file.remove(filePathString)
  }
}

filesRdata <- list.files(path = "~/projects/WaterCalculator/ADEQ/newCSVs", pattern = ".Rdata", all.files=T, full.names=F)  

# for each file in the newCSVs folder, copy to s3 bucket
for (fileName in filesRdata) {
  # print(fileName)
  # Now that the print works, copy to s3 bucket
  filePathString <- paste("newCSVs/", fileName,sep="")
  # print(filePathString)
  # s3bucketPath <- paste(s3_csv_bucket,"inputs/",sep="")
  filePutString <- paste("inputs/",fileName,sep="")
  # print(filePutString)
  put_object(filePathString,filePutString,s3_csv_bucket)
  # delete from newCSVs folder
  if (file.exists(filePathString)) {
    #Delete file if it exists
    file.remove(filePathString)
  }
}


# # Check connection object is open.

if(connSuccess == "y"){
  # Query the database and put the results into the data frame
  #YSTA_STATION <- sqlQuery(conn,"select * from STA_STATION", rows_at_time = 1,believeNRows = FALSE)
  YSTA_STATION <- sqlQuery(conn,"select STATION_NAME, STATION_RID, STATION_CD, STATION_ALT_NAME, STATION_TYPE_RID from STA_STATION", rows_at_time = 1,believeNRows = FALSE)
  #YSW_SITES <- sqlQuery(conn,"select * from VW_SW_SITES", rows_at_time = 1,believeNRows = FALSE)
  YSW_SITES <- sqlQuery(conn,"select STATION_NAME, STATION_CD, STATION_ALT_NAME, WATERBODY_TYPE_CD, HUC_CD, RF3_REACH_NO, LATITUDE_DECDEG, LONGITUDE_DECDEG, STATION_ACCESS, STATION_DESC from VW_SW_SITES", rows_at_time = 1,believeNRows = FALSE)
  #YUSGS_SITES <- sqlQuery(conn,"select * from VW_USGS_WATERBODY_HUC_REACH", rows_at_time = 1,believeNRows = FALSE)
  YUSGS_SITES <- sqlQuery(conn,"select WBID, STATION_ALT_NAME, STATION_ALIAS_ID, STATION_NAME from VW_USGS_WATERBODY_HUC_REACH", rows_at_time = 1,believeNRows = FALSE)
  #YWBHUCREACH <- sqlQuery(conn,"select * from VW_WATERBODY_HUC_REACH", rows_at_time = 1,believeNRows = FALSE) # going to use this later for lake acres and stream miles also uses
  YWBHUCREACH <- sqlQuery(conn,"select WBID, WATERBODY_NAME, HUC, WATERSHED, REACH_DISTANCE, NUTRIENT, AWC, AWW, AWE, AWEDW, FC, PBC, FBC, DWS, AGI, AGL, LAKE_ACRES, ORIGIN, TERMINUS from VW_WATERBODY_HUC_REACH", rows_at_time = 1,believeNRows = FALSE) # going to use this later for lake acres and stream miles also uses
  
  
  # Save to s3 from in memory
  s3save(YSTA_STATION, bucket = s3_csv_bucket, object = "inputs/YSTA_STATION.Rdata")
  s3save(YSW_SITES, bucket = s3_csv_bucket, object = "inputs/YSW_SITES.Rdata")
  s3save(YUSGS_SITES, bucket = s3_csv_bucket, object = "inputs/YUSGS_SITES.Rdata")
  s3save(YWBHUCREACH, bucket = s3_csv_bucket, object = "inputs/YWBHUCREACH.Rdata")
  
  
  #Close connection object.
  close(conn)
}



# Read in input files from s3
# Pull data from AWS s3 bucket
# gsh - change to pull sample data one year at a time as the 5 year was timing out
z_epa_azstream_y1 <-csvFrom_s3(s3_epa_bucket,"samples_chem_year1.csv")
z_epa_azstream_y2 <-csvFrom_s3(s3_epa_bucket,"samples_chem_year2.csv")
z_epa_azstream_y3 <-csvFrom_s3(s3_epa_bucket,"samples_chem_year3.csv")
z_epa_azstream_y4 <-csvFrom_s3(s3_epa_bucket,"samples_chem_year4.csv")
z_epa_azstream_y5 <-csvFrom_s3(s3_epa_bucket,"samples_chem_year5.csv")

# Now, combine all 5 years in dataframe - bind rows
z_epa_azstream <- rbind(z_epa_azstream_y1,z_epa_azstream_y2,z_epa_azstream_y3,z_epa_azstream_y4, z_epa_azstream_y5)

#z_epa_azstream <- csvFrom_s3(s3_epa_bucket,"samples_chem.csv")
z_epa_usgs <- csvFrom_s3(s3_epa_bucket,"samples_chem_usgs_Result.csv")
z_epa_azsites <- csvFrom_s3(s3_epa_bucket,"azsites_Station.csv")
z_epa_usgssites <- csvFrom_s3(s3_epa_bucket,"usgssites_Station.csv")

# Write to local s3 folder
write.csv(z_epa_azstream,"s3/z_epa_azstream.csv")
write.csv(z_epa_usgs,"s3/z_epa_usgs.csv")
write.csv(z_epa_azsites,"s3/z_epa_azsites.csv")
write.csv(z_epa_usgssites,"s3/z_epa_usgssites.csv")
# commented out ATTAINS move from s3 to local because files are generated by tool
# ATTAINS
# s3temp <- csvFrom_s3(s3_csv_bucket,"ATTAINS/associated-actions.csv")
# write.csv(s3temp,"ATTAINS/associated-actions.csv")
# s3temp <- csvFrom_s3(s3_csv_bucket,"ATTAINS/General.csv")
# write.csv(s3temp,"ATTAINS/General.csv")
# s3temp <- csvFrom_s3(s3_csv_bucket,"ATTAINS/location.csv")
# write.csv(s3temp,"ATTAINS/location.csv")
# s3temp <- csvFrom_s3(s3_csv_bucket,"ATTAINS/Parameters.csv")
# write.csv(s3temp,"ATTAINS/Parameters.csv")
# s3temp <- csvFrom_s3(s3_csv_bucket,"ATTAINS/Sources.csv")
# write.csv(s3temp,"ATTAINS/Sources.csv")
# s3temp <- csvFrom_s3(s3_csv_bucket,"ATTAINS/Uses.csv")
# write.csv(s3temp,"ATTAINS/Uses.csv")
# s3temp <- csvFrom_s3(s3_csv_bucket,"ATTAINS/water_type.csv")
# write.csv(s3temp,"ATTAINS/water_type.csv")


# inputs/Rdata files - saves s3 object to local file
save_object("inputs/ZLASTDB.Rdata", file = "inputs/ZLASTDB.Rdata", bucket = s3_csv_bucket)
save_object("inputs/FISH.Rdata", file = "inputs/FISH.Rdata", bucket = s3_csv_bucket)
save_object("inputs/YSOURCEID.Rdata", file = "inputs/YSOURCEID.Rdata", bucket = s3_csv_bucket)
save_object("inputs/YSTA_STATION.Rdata", file = "inputs/YSTA_STATION.Rdata", bucket = s3_csv_bucket)
save_object("inputs/YSW_SITES.Rdata", file = "inputs/YSW_SITES.Rdata", bucket = s3_csv_bucket)
save_object("inputs/YUSGS_SITES.Rdata", file = "inputs/YUSGS_SITES.Rdata", bucket = s3_csv_bucket)
save_object("inputs/YWBHUCREACH.Rdata", file = "inputs/YWBHUCREACH.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZASSESSMENTHIST.Rdata", file = "inputs/ZASSESSMENTHIST.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZATTAINSPARAMETERMAP.Rdata", file = "inputs/ZATTAINSPARAMETERMAP.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZBINOMIAL.Rdata", file = "inputs/ZBINOMIAL.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZCORE.Rdata", file = "inputs/ZCORE.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZDATAGAPRANK.Rdata", file = "inputs/ZDATAGAPRANK.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZDEQSTANDARDS.Rdata", file = "inputs/ZDEQSTANDARDS.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZNORMALIZEDSITES.Rdata", file = "inputs/ZNORMALIZEDSITES.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZNUTRIENTSTDS.Rdata", file = "inputs/ZNUTRIENTSTDS.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZPHSTDS.Rdata", file = "inputs/ZPHSTDS.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZSTDUNIT.Rdata", file = "inputs/ZSTDUNIT.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZTROPHIC.Rdata", file = "inputs/ZTROPHIC.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZUNIVERSE.Rdata", file = "inputs/ZUNIVERSE.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZUSECROSSWALK2.Rdata", file = "inputs/ZUSECROSSWALK2.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZUSGSDLSITES.Rdata", file = "inputs/ZUSGSDLSITES.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZNWISTOWQPMAP.Rdata", file = "inputs/ZNWISTOWQPMAP.Rdata", bucket = s3_csv_bucket)
save_object("inputs/ZCARRYFORWARDMAP.Rdata", file = "inputs/ZCARRYFORWARDMAP.Rdata", bucket = s3_csv_bucket)


# Load 2018 and 2020 - Data files
save_object("inputs/Z2014TO2016_AZDATA.Rdata", file = "inputs/Z2014TO2016_AZDATA.Rdata", bucket = s3_csv_bucket)
save_object("inputs/Z2012TO2014_AZDATA.Rdata", file = "inputs/Z2012TO2014_AZDATA.Rdata", bucket = s3_csv_bucket)

# Load 2018 and 2020 - Site files
save_object("inputs/Z2014TO2016_AZSITES.Rdata", file = "inputs/Z2014TO2016_AZSITES.Rdata", bucket = s3_csv_bucket)
save_object("inputs/Z2012TO2014_AZSITES.Rdata", file = "inputs/Z2012TO2014_AZSITES.Rdata", bucket = s3_csv_bucket)

# Load most current EPA approved standards.  Currently the 2016 triennial review.
save_object("inputs/ZDEQSTANDARDSAPPROVED.Rdata", file = "inputs/ZDEQSTANDARDSAPPROVED.Rdata", bucket = s3_csv_bucket)
load(file = "inputs/ZDEQSTANDARDSAPPROVED.Rdata")


# inputs/csv files
#s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/human.csv")
#write.csv(s3temp,"inputs/human.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ATTAINS2018ACTIONS.csv")
write.csv(s3temp,"inputs/ATTAINS2018ACTIONS.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ATTAINS2018USES.csv") 
write.csv(s3temp,"inputs/ATTAINS2018USES.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ATTAINS2018PARAMETERS.csv") 
write.csv(s3temp,"inputs/ATTAINS2018PARAMETERS.csv") 
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZSOURCES.csv") 
write.csv(s3temp,"inputs/ZSOURCES.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/YSITESMANUAL.csv")
write.csv(s3temp,"inputs/YSITESMANUAL.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZAUNOTINATTAINS.csv") 
write.csv(s3temp,"inputs/ZAUNOTINATTAINS.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZBUGSANDPEBBLES.csv") 
write.csv(s3temp,"inputs/ZBUGSANDPEBBLES.csv") 
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZCRITDATA.csv") 
write.csv(s3temp,"inputs/ZCRITDATA.csv") 
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZCRITICALCONDITION.csv") 
write.csv(s3temp,"inputs/ZCRITICALCONDITION.csv") 
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZDATAGAPINPUT.csv") 
write.csv(s3temp,"inputs/ZDATAGAPINPUT.csv") 
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZFISH.csv") 
write.csv(s3temp,"inputs/ZFISH.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZFISHADVISORIES.csv") 
write.csv(s3temp,"inputs/ZFISHADVISORIES.csv") 
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZIMPAIRMENTYEAR.csv")
write.csv(s3temp,"inputs/ZIMPAIRMENTYEAR.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZIMPROVEMENTS.csv") 
write.csv(s3temp,"inputs/ZIMPROVEMENTS.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZLABCOST.csv") 
write.csv(s3temp,"inputs/ZLABCOST.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZPARAMPRIORITY.csv")
write.csv(s3temp,"inputs/ZPARAMPRIORITY.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZPARAMYEAR.csv")
write.csv(s3temp,"inputs/ZPARAMYEAR.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZQAPARAM.csv") 
write.csv(s3temp,"inputs/ZQAPARAM.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZSOURCES.csv") 
write.csv(s3temp,"inputs/ZSOURCES.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZSTDTYPE.csv")
write.csv(s3temp,"inputs/ZSTDTYPE.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZUSENOTASSESSED.csv") 
write.csv(s3temp,"inputs/ZUSENOTASSESSED.csv") 
# gsh added 2/8/21 load wotusCSV from s3
# gsh updated on 3/15/21 per Jason
#s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/zwotusimpaired_old.csv") 
#write.csv(s3temp,"inputs/zwotusimpaired_old.csv") 
#s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/zwotusimpaired_new.csv") 
#write.csv(s3temp,"inputs/zwotusimpaired_new.csv") 
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ZWOTUS.csv") 
write.csv(s3temp,"inputs/ZWOTUS.csv") 
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/ATTAINSSPLIT.csv") 
write.csv(s3temp,"inputs/ATTAINSSPLIT.csv") 


# jdrive as folder changed to 'inputs'
s3temp  <- csvFrom_s3(s3_csv_bucket,"inputs/FishAdvisories2020-09-10.csv")
write.csv(s3temp,"inputs/FishAdvisories2020-09-10.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/FishAdvisories2020-09-15.csv")
write.csv(s3temp,"inputs/FishAdvisories2020-09-15.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/FishAdvisories2020-09-18.csv")
write.csv(s3temp,"inputs/FishAdvisories2020-09-18.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/FishAdvisories2020-09-21.csv")
write.csv(s3temp,"inputs/FishAdvisories2020-09-21.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/FishAdvisories2020-09-22.csv")
write.csv(s3temp,"inputs/FishAdvisories2020-09-22.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/FishAdvisories2020-09-23.csv")
write.csv(s3temp,"inputs/FishAdvisories2020-09-23.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/Fishdata.csv")
write.csv(s3temp,"inputs/Fishdata.csv")
s3temp <- csvFrom_s3(s3_csv_bucket,"inputs/voldecisions.csv")
write.csv(s3temp,"inputs/voldecisions.csv")

# commented out ATTAINS move from s3 to local because files are generated by tool
# metrics
# s3temp <- csvFrom_s3(s3_csv_bucket,"metrics/decisions.csv")
# write.csv(s3temp,"metrics/decisions.csv")
# s3temp <- csvFrom_s3(s3_csv_bucket,"metrics/percentassessed.csv")
# write.csv(s3temp,"metrics/percentassessed.csv")

# Turn off scientific notation
options(scipen = 999)


# SPLITARAMETERS <- read_csv("inputs/SPLITPARAMETERS.csv")


## New O
ATTAINSACTIONS <- read_csv("inputs/ATTAINS2018ACTIONS.csv",
                           col_types = cols(ASSESSMENT_UNIT_ID = col_character(),
                                            ACTION_PARAM_NAME = col_character(),
                                            CharacteristicName = col_character(),
                                            ACTION_ID = col_character(),
                                            ACTION_TYPE = col_character(),
                                            WBID = col_character()))

# remove added X1 column
# ATTAINSACTIONS <- select(ATTAINSACTIONS,-X1)
if("X1" %in% colnames(ATTAINSACTIONS)){ATTAINSACTIONS <- select(ATTAINSACTIONS,-X1)}

ATTAINSUSES <- read_csv("inputs/ATTAINS2018USES.csv", 
                        col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                         USE_AGENCY_CODE = col_character(), 
                                         USE_ASMT_BASIS = col_character(), 
                                         USE_ASMT_DATE = col_date(format = "%m/%d/%Y"), 
                                         USE_ASSESSOR_NAME = col_character(), 
                                         USE_ATTAINMENT_CODE = col_character(), 
                                         USE_COMMENT = col_double(), 
                                         USE_MONITORING_END = col_character(), 
                                         USE_MONITORING_START = col_character(), 
                                         USE_NAME = col_character(), 
                                         USE_ORG_QUALIFIER_FLAG = col_character(), 
                                         USE_STATE_IR_CAT = col_character(), 
                                         USE_THREATENED = col_character(), 
                                         USE_TREND = col_character()))

# remove added X1 column
# ATTAINSUSES <- select(ATTAINSUSES,-X1)
if("X1" %in% colnames(ATTAINSUSES)){ATTAINSUSES <- select(ATTAINSUSES,-X1)}

ATTAINSPARAMETERS <- read_csv("inputs/ATTAINS2018PARAMETERS.csv", 
                              col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                               PARAM_AGENCY_CODE = col_character(), 
                                               PARAM_ALT_LISTING_ID = col_character(), 
                                               PARAM_ATTAINMENT_CODE = col_character(), 
                                               PARAM_COMMENT = col_character(), 
                                               PARAM_CONSENT_DECREE_CYCLE = col_character(), 
                                               PARAM_DELISTING_AGENCY = col_character(), 
                                               PARAM_DELISTING_COMMENT = col_character(), 
                                               PARAM_DELISTING_REASON = col_character(), 
                                               PARAM_EXPECTED_TO_ATTAIN = col_double(), 
                                               PARAM_NAME = col_character(), 
                                               PARAM_ORG_QUALIFIER_FLAG = col_character(), 
                                               PARAM_POLLUTANT_INDICATOR = col_character(), 
                                               PARAM_PRIORITY_RANKING = col_character(), 
                                               PARAM_STATE_IR_CAT = col_character(), 
                                               PARAM_STATUS_NAME = col_character(), 
                                               PARAM_TARGET_TMDL_DATE = col_double(), 
                                               PARAM_TREND = col_character(), 
                                               PARAM_USE_NAME = col_character(), 
                                               PARAM_YEAR_LISTED = col_double()))
# remove added X1 column
# ATTAINSPARAMETERS <- select(ATTAINSPARAMETERS,-X1)
if("X1" %in% colnames(ATTAINSPARAMETERS)){ATTAINSPARAMETERS <- select(ATTAINSPARAMETERS,-X1)}

# Below file loaded as h.assessall
load(file = "inputs/ZLASTDB.Rdata")

human <- read_csv("human.csv", col_types = cols(Assessed = col_character(), 
                                                CharacteristicName = col_character(), 
                                                NewUse = col_character(), 
                                                No = col_double(), 
                                                ResultSampleFractionText = col_character(), 
                                                Use = col_character(), 
                                                WATERBODY_NAME = col_character(), 
                                                WBID = col_character(), 
                                                Yes = col_double(), 
                                                actualsampneed = col_double(), 
                                                binomial = col_character(), 
                                                newassess = col_double(), 
                                                paramcarryforward = col_character(), 
                                                provassess = col_double(), 
                                                provcomment = col_character(), 
                                                provdate = col_date(format = "%Y-%m-%d"), 
                                                provdatetext = col_character(), 
                                                sampcount = col_double()))


ZSOURCES <- read_csv("inputs/ZSOURCES.csv", 
                     col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                      CharacteristicName = col_character(), 
                                      SOURCE_COMMENT = col_character(), 
                                      SOURCE_CONFIRMED = col_character(), 
                                      SOURCE_NAME = col_character(), 
                                      SOURCE_PARAM_NAME = col_character(), 
                                      WBID = col_character()))

if("X1" %in% colnames(ZSOURCES)){ZSOURCES <- select(ZSOURCES,-X1)}

j.decisionsfile <- read_csv("metrics/decisions.csv",
                            col_types = cols(
                              Date = col_date(format = "%Y-%m-%d"),
                              CountDecision = col_double(),
                              Type = col_character()))

# Remove X1 
#j.decisionsfile <- if(!is.null(select(j.decisionsfile,-X1)
if("X1" %in% colnames(j.decisionsfile)){j.decisionsfile <- select(j.decisionsfile,-X1)}

j.percentassessedfile <- read_csv("metrics/percentassessed.csv",
                                  col_types = cols(
                                    Date = col_date(format = "%Y-%m-%d"),
                                    CountAssessed = col_double(),
                                    PercentAssessed = col_double(),
                                    Type = col_character()))
# Remove X1 
# j.percentassessedfile <- select(j.percentassessedfile,-X1)
if("X1" %in% colnames(j.percentassessedfile)){j.percentassessedfile <- select(j.percentassessedfile,-X1)}

j.volprevious <- read_csv("inputs/voldecisions.csv",
                           col_types = cols(
                             date = col_date(format = "%Y-%m-%d"),
                             `Insufficient Information` = col_double(),
                             `Meeting Criteria` = col_double(),
                             `Not Meeting Criteria` = col_double(),
                             paramdecision = col_double(),
                             `Not Assessed` = col_double()))

# Remove X1 
# j.voldecisions <- select(j.voldecisions,-X1)
if("X1" %in% colnames(j.volprevious)){j.volprevious <- select(j.volprevious,-X1)}

YSITESMANUAL <- read_csv("inputs/YSITESMANUAL.csv",
                         col_types = cols(WBID = col_character(), 
                                          MonitoringLocationIdentifier = col_character(), 
                                          WATERBODY_TYPE_CD = col_character(), 
                                          STATION_ALT_NAME = col_character(), 
                                          LATITUDE_DECDEG = col_double(), 
                                          LONGITUDE_DECDEG = col_double()))

# Remove X1 
# YSITESMANUAL <- select(YSITESMANUAL,-X1)
if("X1" %in% colnames(YSITESMANUAL)){YSITESMANUAL <- select(YSITESMANUAL,-X1)}

load(file = "inputs/YSOURCEID.Rdata")
load(file = "inputs/YSTA_STATION.Rdata")
load(file = "inputs/YSW_SITES.Rdata")
load(file = "inputs/YUSGS_SITES.Rdata")
load(file = "inputs/YWBHUCREACH.Rdata")
# YWBHUCREACH <- mutate(YWBHUCREACH,
#                       WBID=as.character(WBID),
#                       WATERBODY_NAME=as.character(WATERBODY_NAME),
#                       HUC=as.character(HUC),
#                       WATERSHED=as.character(WATERSHED))
load(file = "inputs/ZASSESSMENTHIST.Rdata")
load(file = "inputs/ZATTAINSPARAMETERMAP.Rdata")

# Depricated
# ZATTAINSUSE <- read_csv("inputs/ZATTAINSUSE.csv")
# ZATTAINSUSE <- csvFrom_s3(s3_csv_bucket,"inputs/ZATTAINSUSE.csv")
# read from RDS
# ZATTAINSUSE <- dbReadTable(conDb, "ZATTAINSUSE")

ZAUNOTINATTAINS <- read_csv("inputs/ZAUNOTINATTAINS.csv", 
                            col_types = cols(WBID = col_character()))

# Remove X1 
# ZAUNOTINATTAINS <- select(ZAUNOTINATTAINS,-X1)
if("X1" %in% colnames(ZAUNOTINATTAINS)){ZAUNOTINATTAINS <- select(ZAUNOTINATTAINS,-X1)}

# use this if can't connect to EPA
# ZAZSITES <- read_csv("inputs/ZAZSITES.csv")
# ZAZSITES <- csvFrom_s3(s3_csv_bucket,"inputs/ZAZSITES.csv")
# ZAZSITES <- dbReadTable(conDb, "ZAZSITES")

load(file = "inputs/ZBINOMIAL.Rdata")

ZBUGSANDPEBBLES <- read_csv("inputs/ZBUGSANDPEBBLES.csv", 
                            col_types = cols(Decision = col_character(), 
                                             Number_meeting_criteria = col_double(), 
                                             Number_samples = col_double(), 
                                             Parameter = col_character(), 
                                             Use = col_character(), 
                                             WBID = col_character(), 
                                             comments = col_character()))

# Remove X1 
# ZBUGSANDPEBBLES <- select(ZBUGSANDPEBBLES,-X1)
if("X1" %in% colnames(ZBUGSANDPEBBLES)){ZBUGSANDPEBBLES <- select(ZBUGSANDPEBBLES,-X1)}

load(file = "inputs/ZCORE.Rdata")

ZCRITDATA <- read_csv("inputs/ZCRITDATA.csv", 
                      col_types = cols(
                        WBID = col_character(),
                        aggdate = col_date(format = "%m/%d/%Y"),
                        CharacteristicName = col_character(),
                        ResultSampleFractionText = col_character(),
                        NewUse = col_character(),
                        aggtimespace = col_double(),
                        critmet = col_character()))

# Remove X1
# ZCRITDATA <- select(ZCRITDATA,-X1)
if("X1" %in% colnames(ZCRITDATA)){ZCRITDATA <- select(ZCRITDATA,-X1)}

ZCRITICALCONDITION <- read_csv("inputs/ZCRITICALCONDITION.csv", 
                               col_types = cols(
                                 WBID = col_character(),
                                 WATERBODY_NAME = col_character(),
                                 criticalcondition = col_character(), 
                                 criticallocation = col_character())) 

# Remove X1
# ZCRITICALCONDITION <- select(ZCRITICALCONDITION,-X1)
if("X1" %in% colnames(ZCRITICALCONDITION)){ZCRITICALCONDITION <- select(ZCRITICALCONDITION,-X1)}

ZDATAGAPINPUT <- read_csv("inputs/ZDATAGAPINPUT.csv", 
                          col_types = cols(Champion = col_character(), 
                                           Decision = col_character(), 
                                           WBID = col_character()))

# Remove X1
# ZDATAGAPINPUT <- select(ZDATAGAPINPUT,-X1)
if("X1" %in% colnames(ZDATAGAPINPUT)){ZDATAGAPINPUT <- select(ZDATAGAPINPUT,-X1)}

load(file = "inputs/ZDATAGAPRANK.Rdata")
load(file = "inputs/ZDEQSTANDARDS.Rdata")

# May be deprecated
# ZDEQUSES <- read_csv("inputs/ZDEQUSES.csv")
# ZDEQUSES <- csvFrom_s3(s3_csv_bucket,"inputs/ZDEQUSES.csv")
# ZDEQUSES <- dbReadTable(conDb, "ZDEQUSES")

# May be deprecated
# ZECOLISTDS <- read_csv("inputs/ZECOLISTDS.csv")
# ZECOLISTDS <- csvFrom_s3(s3_csv_bucket,"inputs/ZECOLISTDS.csv")
# ZECOLISTDS <- dbReadTable(conDb, "ZECOLISTDS")

ZFISH <- read_csv("inputs/ZFISH.csv", 
                  col_types = cols(WBID = col_character(), 
                                   NewUse = col_character(), 
                                   Use = col_character(), 
                                   CharacteristicName = col_character(), 
                                   ResultSampleFractionText = col_character(), 
                                   existimpair = col_character(), 
                                   paramcarryforward = col_character(), 
                                   newassess = col_number()))

# Remove X1
# ZFISH <- select(ZFISH,-X1)
if("X1" %in% colnames(ZFISH)){ZFISH <- select(ZFISH,-X1)}

ZFISHADVISORIES <- read_csv("inputs/ZFISHADVISORIES.csv", 
                            col_types = cols(Advice = col_character(), 
                                             Color = col_character(), 
                                             `Date Issued` = col_date(format = "%m/%d/%Y"), 
                                             Parameter = col_character(), 
                                             Species = col_character(), 
                                             WBID = col_character(), 
                                             `Waterbody Name` = col_character()))

# Remove X1
# ZFISHADVISORIES <- select(ZFISHADVISORIES,-X1)
if("X1" %in% colnames(ZFISHADVISORIES)){ZFISHADVISORIES <- select(ZFISHADVISORIES,-X1)}


ZIMPAIRMENTYEAR <- read_csv("inputs/ZIMPAIRMENTYEAR.csv",
                            col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                             WBID = col_character(),
                                             PARAM_NAME = col_character(),
                                             CharacteristicName = col_character(),
                                             PARAM_YEAR_LISTED = col_character()))

# Remove X1
# ZIMPAIRMENTYEAR <- select(ZIMPAIRMENTYEAR,-X1)
if("X1" %in% colnames(ZIMPAIRMENTYEAR)){ZIMPAIRMENTYEAR <- select(ZIMPAIRMENTYEAR,-X1)}

ZIMPROVEMENTS <- read_csv("inputs/ZIMPROVEMENTS.csv", 
                          col_types = cols(
                            WBID = col_character(),
                            WATERBODY_DESC = col_character(),
                            improvementdate = col_date(format = "%m/%d/%Y"),
                            improvement = col_character()))

# Remove X1
# ZIMPROVEMENTS <- select(ZIMPROVEMENTS,-X1)
if("X1" %in% colnames(ZIMPROVEMENTS)){ZIMPROVEMENTS <- select(ZIMPROVEMENTS,-X1)}

ZLABCOST <- read_csv("inputs/ZLABCOST.csv", 
                     col_types = cols(CharacteristicName = col_character(), 
                                      Cost = col_double(), 
                                      ResultSampleFractionText = col_character()))

# Remove X1
# ZLABCOST <- select(ZLABCOST,-X1)
if("X1" %in% colnames(ZLABCOST)){ZLABCOST <- select(ZLABCOST,-X1)}

load(file = "inputs/ZNORMALIZEDSITES.Rdata")
load(file = "inputs/ZNUTRIENTSTDS.Rdata")

ZPARAMPRIORITY <- read_csv("inputs/ZPARAMPRIORITY.csv",
                           col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                            WBID = col_character(),
                                            PARAM_NAME = col_character(),
                                            CharacteristicName = col_character(),
                                            PARAM_PRIORITY_RANKING = col_character()))

# Remove X1
# ZPARAMPRIORITY <- select(ZPARAMPRIORITY,-X1)
if("X1" %in% colnames(ZPARAMPRIORITY)){ZPARAMPRIORITY <- select(ZPARAMPRIORITY,-X1)}

ZPARAMYEAR <- read_csv("inputs/ZPARAMYEAR.csv",
                       col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                        PARAM_NAME = col_character(),
                                        PARAM_USE_NAME = col_character(),
                                        PARAM_STATUS_NAME = col_character(), 
                                        PARAM_ATTAINMENT_CODE = col_character(), 
                                        NEWYEAR = col_double()))

# Remove X1
# ZPARAMYEAR <- select(ZPARAMYEAR,-X1)
if("X1" %in% colnames(ZPARAMYEAR)){ZPARAMYEAR <- select(ZPARAMYEAR,-X1)}

# May be depricated
# ZPH <- read_csv("inputs/ZPH.csv")
# ZPH <- csvFrom_s3(s3_csv_bucket,"inputs/ZPH.csv")
# ZPH <- dbReadTable(conDb, "ZPH")

load(file = "inputs/ZPHSTDS.Rdata")

ZQAPARAM <- read_csv("inputs/ZQAPARAM.csv", 
                     col_types = cols(
                       WBID = col_character(),
                       NAME = col_character(),
                       CharacteristicName = col_character(),
                       Date = col_date(format = "%m/%d/%Y"),
                       Action = col_character(),
                       Stage = col_character(),
                       Comment = col_character(),
                       Current = col_character()))

# Remove X1
# ZQAPARAM <- select(ZQAPARAM,-X1)
if("X1" %in% colnames(ZQAPARAM)){ZQAPARAM <- select(ZQAPARAM,-X1)}

ZSOURCES <- read_csv("inputs/ZSOURCES.csv", 
                     col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                      CharacteristicName = col_character(), 
                                      SOURCE_COMMENT = col_character(), 
                                      SOURCE_CONFIRMED = col_character(), 
                                      SOURCE_NAME = col_character(), 
                                      SOURCE_PARAM_NAME = col_character(), 
                                      WBID = col_character()))

# Remove X1 
# ZSOURCES <- select(ZSOURCES,-X1)
if("X1" %in% colnames(ZSOURCES)){ZSOURCES <- select(ZSOURCES,-X1)}

ZSTDTYPE <- read_csv("inputs/ZSTDTYPE.csv",
                     col_types = cols(
                       CharacteristicName = col_character(), 
                       ResultSampleFractionText = col_character(),
                       STDTYPE = col_character()))

# Remove X1
# ZSTDTYPE <- select(ZSTDTYPE,-X1)
if("X1" %in% colnames(ZSTDTYPE)){ZSTDTYPE <- select(ZSTDTYPE,-X1)}

load(file = "inputs/ZSTDUNIT.Rdata")
load(file = "inputs/ZTROPHIC.Rdata")
load(file = "inputs/ZUNIVERSE.Rdata")
load(file = "inputs/ZUSECROSSWALK2.Rdata")
load(file = "inputs/ZUSECROSSWALK2.Rdata")

ZUSENOTASSESSED <- read_csv("inputs/ZUSENOTASSESSED.csv", 
                            col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                             EPA_IR_CATEGORY_ID = col_character(), 
                                             LAST_ASSESSED_CYCLE = col_double(), 
                                             LAST_MODIFIED_CYCLE = col_double(), 
                                             WBID = col_character()))

# Remove X1
# ZUSENOTASSESSED <- select(ZUSENOTASSESSED,-X1)
if("X1" %in% colnames(ZUSENOTASSESSED)){ZUSENOTASSESSED <- select(ZUSENOTASSESSED,-X1)}


# gsh 2/8/21 - added wotus read from CSV
# new wotus
# gsh updated per Jason on 3/15/21

# wotusnewCSV  <- read_csv("inputs/zwotusimpaired_new.csv", 
#                      col_types = cols(`Waterbody ID` = col_character(), 
#                                       WOTUSSTATUS = col_character()))
# 
# if("X1" %in% colnames(wotusnewCSV)){wotusnewCSV <- select(wotusnewCSV,-X1)}
# wotusnewCSV <- select(wotusnewCSV, WBID = Waterbody.ID, WOTUSSTATUS)
# 
# # old wotus
# wotusoldCSV  <- read_csv("inputs/zwotusimpaired_old.csv", 
#                          col_types = cols(WBID = col_character(), 
#                                           WOTUSSTATUS = col_character()))
# if("X1" %in% colnames(wotusoldCSV)){wotusoldCSV <- select(wotusoldCSV,-X1)}
# wotusoldCSV <- select(wotusoldCSV, WBID, WOTUSSTATUS)
# 
# ZLASTWOTUS <- read_csv("inputs/ZLASTWOTUS.csv")
# if("X1" %in% colnames(ZLASTWOTUS)){ZLASTWOTUS <- select(ZLASTWOTUS,-X1)}

# Load recent WOTUS determinations - change by Jdj on 3/15
ZWOTUS <- read_csv("inputs/ZWOTUS.csv", 
                         col_types = cols(WBID = col_character(),
                                          WOTUSSTATUS = col_character()))
if("X1" %in% colnames(ZWOTUS)){ZWOTUS <- select(ZWOTUS,-X1)}
# end load wotus from CSV

# Add ATTAINSSPLIT
ATTAINSSPLIT <- read_csv("inputs/ATTAINSSPLIT.csv", 
                         col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                          PARAM_AGENCY_CODE = col_character(), 
                                          PARAM_ALT_LISTING_ID = col_character(), 
                                          PARAM_ATTAINMENT_CODE = col_character(), 
                                          PARAM_COMMENT = col_character(), 
                                          PARAM_CONSENT_DECREE_CYCLE = col_character(), 
                                          PARAM_DELISTING_AGENCY = col_character(), 
                                          PARAM_DELISTING_COMMENT = col_character(), 
                                          PARAM_DELISTING_REASON = col_character(), 
                                          PARAM_EXPECTED_TO_ATTAIN = col_double(), 
                                          PARAM_NAME = col_character(), PARAM_ORG_QUALIFIER_FLAG = col_character(), 
                                          PARAM_POLLUTANT_INDICATOR = col_character(), 
                                          PARAM_PRIORITY_RANKING = col_character(), 
                                          PARAM_STATE_IR_CAT = col_character(), 
                                          PARAM_STATUS_NAME = col_character(), 
                                          PARAM_TARGET_TMDL_DATE = col_double(), 
                                          PARAM_TREND = col_character(), PARAM_USE_NAME = col_character(), 
                                          PARAM_YEAR_LISTED = col_double()))

if("X1" %in% colnames(ATTAINSSPLIT)){ATTAINSSPLIT <- select(ATTAINSSPLIT,-X1)}

# May be depricated
# ZWATERBODYNAME <- read_csv("inputs/ZWATERBODYNAME.csv")
# ZWATERBODYNAME <- csvFrom_s3(s3_csv_bucket,"inputs/ZWATERBODYNAME.csv")
# ZWATERBODYNAME <- dbReadTable(conDb, "ZWATERBODYNAME")

# Open USGS Sites with DL issues
load(file = "inputs/ZUSGSDLSITES.Rdata")

# This loads to dataframe - nwistowqpMAP
load(file = "inputs/ZNWISTOWQPMAP.Rdata")

# Open carry forward map.  This resolves the problem due to ATTAINS 
# not tracking fraction and acute/chronic which leaves these 
# values null for carry forward decisions
load(file = "inputs/ZCARRYFORWARDMAP.Rdata")

# gsh added fish file in place of odbc
# the zfish.csv is for new fish advisories which are impairments.  
# The fish odbc connection looks at the fish data and trys to figure out 
# if there should be a new advisory.
# FISH <- read_csv("inputs/ZFISH_ORACLE.csv", 
#                             col_types = cols(STATION_CD = col_character(), 
#                                              STATION_ALT_NAME = col_character(), 
#                                              #ACTIVITY_END_DATE = col_date(format = "%m/%d/%Y"), 
#                                              FINALID = col_character(), 
#                                              LAB_QA_FLAGS = col_character(), 
#                                              SUBSTANCE_NAME = col_character(), 
#                                              LAB_RESULT = col_double(),
#                                              LAB_RESULT_UNIT = col_character(),
#                                              DETECTION_LIMIT = col_double(),
#                                              DETECTION_LIMIT_UNIT = col_character()))

load(file = "inputs/FISH.Rdata")


# As per Jason Jones - # Load Historic Data for the 2018 and 2020 cycle (7/1/2012 to 6/30/2016)
load(file = "inputs/Z2014TO2016_AZDATA.Rdata") # note that this opens as a.azdata
a.azdata2014to2016 <- a.azdata

load(file = "inputs/Z2014TO2016_AZSITES.Rdata") # note that this opens as a.azsites
a.azsites2014to2016 <- a.azsites

load(file = "inputs/Z2012TO2014_AZDATA.Rdata") # note that this opens as a.azdata
a.azdata2012to2014 <- a.azdata

load(file = "inputs/Z2012TO2014_AZSITES.Rdata") # note that this opens as a.azsites
a.azsites2012to2014 <- a.azsites

#Remove temp dataframes
rm(a.azdata)
rm(a.azsites)
