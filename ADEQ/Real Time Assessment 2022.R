#### ARIZONA's Real Time Assessment Tool - "Ratatoole" ####
### Code by Jason Jones, Arizona Department of Environmental Quality, October 2018 to now() ###
### Code is free to use and distribute.  Proper credit is requested.  
### Contact.  Jason Jones, Arizona Assessment Coordinator, jdj@azdeq.gov, 602-771-2235.

### Note: Input naming scheme is:
# X = Dataframes created by code.
# Y = Data from ODBC
# Z = Data from RData or csv files.  RData generally used for static tables while csv used if regularlly updated.

### Purpose: To pull data from WQX, compare it to standards and determine assessment decisions by parameter, use and waterbody

# 2022/20 ATTAINS clean up
#* 15040004-0840 incorrectly has nutrients listed as AWW.  Should be AWC.
#* 15030204-003A or 003.  WQDB has 003, but assessment using 003A.	BILL WILLIAMS RIVER 	AMMONIA-NITROGEN
#* 15030101-015 colorado river listed for Se 2018 303d list but also on delist report.
#* 15050100-014B has wrong use in ATTAINS.  Correct AWW with AWEDW
#* 15070201-1010 has wrong use in ATTAINS.  Correct AWC with AWW
#* 15050301-560 has wrong use in ATTAINS (note no Cox gulch in app b).  Remove AGL use.




#### A - ESTABLISH LIVE CONNECTION WITH WATER QUALITY PORTAL AND GET RAW DATA ####



# Clock how long it takes code to run...end time is at the end of the code
start_time <- Sys.time()

# Load packages - install package if not already installed
library(tidyverse) 
library(lubridate)
library(googlesheets4)
library(RODBC)


# Turn off scientific notation
options(scipen = 999)

# Set up directory
setwd("C:/Users/65942/Desktop/Real Time Assessment")

# Pick date range
startDate <- as.Date("2016-07-01")
endDate <- as.Date("2021-06-30")

# Static file for testing...remove if want live data
load(file = "inputs/ZAZDATA.Rdata") # note that this opens as a.azdata
load(file = "inputs/ZAZSITES.Rdata") # note that this opens as a.azsites

# USGS Colorado River Results

# Create temp file
temp <- tempfile()

a.usgssites <- "&siteid=USGS-355241114401601&siteid=USGS-09429600&siteid=USGS-344459114313801&siteid=USGS-09424000&siteid=USGS-324343114364901&siteid=USGS-330131114364101&siteid=USGS-332203114421201&siteid=USGS-332604114373601&siteid=USGS-09429100&siteid=USGS-333038114340401&siteid=USGS-335832111425401&siteid=USGS-09421500&siteid=USGS-09423000&siteid=USGS-09423500&siteid=USGS-09423550&siteid=USGS-09423560&siteid=USGS-09427520&siteid=USGS-09429490&siteid=USGS-09429500&siteid=USGS-09522000&siteid=USGS-09521100"

# Download zip
url <- paste0("https://www.waterqualitydata.us/data/Result/search?", a.usgssites, "&startDateLo=07-01-2016&startDateHi=06-30-2021&mimeType=csv&zip=yes")
download.file(url ,temp, mode="wb")

# Column Mapping

# Extract and put in data frame
a.usgs <- read_csv(unz(temp, "result.csv"),
                   col_types = cols(OrganizationIdentifier = col_character(), 
                                    OrganizationFormalName = col_character(), 
                                    ActivityIdentifier = col_character(), 
                                    ActivityTypeCode = col_character(),
                                    ActivityMediaName = col_character(),
                                    ActivityMediaSubdivisionName = col_skip(),
                                    ActivityStartDate = col_date(format = "%Y-%m-%d"), 
                                    `ActivityStartTime/Time` = col_character(),
                                    `ActivityEndTime/TimeZoneCode` = col_skip(),
                                    `ActivityDepthHeightMeasure/MeasureValue` = col_double(),
                                    `ActivityDepthHeightMeasure/MeasureUnitCode` = col_character(),
                                    ActivityDepthAltitudeReferencePointText = col_skip(),
                                    `ActivityTopDepthHeightMeasure/MeasureValue` = col_skip(),
                                    `ActivityTopDepthHeightMeasure/MeasureUnitCode` = col_skip(),
                                    `ActivityBottomDepthHeightMeasure/MeasureValue` = col_skip(),
                                    `ActivityBottomDepthHeightMeasure/MeasureUnitCode` = col_skip(),
                                    ProjectIdentifier = col_skip(),
                                    ActivityConductingOrganizationText = col_character(),
                                    MonitoringLocationIdentifier = col_character(),
                                    ActivityCommentText = col_character(),
                                    HydrologicCondition = col_character(),
                                    HydrologicEvent = col_skip(),
                                    `SampleCollectionMethod/MethodIdentifier` = col_skip(),
                                    `SampleCollectionMethod/MethodIdentifierContext` = col_skip(),
                                    `SampleCollectionMethod/MethodName` = col_skip(),
                                    SampleCollectionEquipmentName = col_skip(),
                                    ResultDetectionConditionText = col_character(),
                                    CharacteristicName = col_character(),
                                    ResultSampleFractionText = col_character(),
                                    ResultMeasureValue = col_double(),
                                    `ResultMeasure/MeasureUnitCode` = col_character(),
                                    MeasureQualifierCode = col_character(),
                                    ResultStatusIdentifier = col_skip(),
                                    StatisticalBaseCode = col_skip(),
                                    ResultValueTypeName = col_skip(),
                                      ResultWeightBasisText = col_skip(),
                                    ResultTemperatureBasisText = col_skip(),
                                    ResultParticleSizeBasisText = col_character(),
                                    PrecisionValue = col_skip(),
                                    ResultCommentText = col_character(),
                                    USGSPCode = col_skip(),
                                    `ResultDepthHeightMeasure/MeasureValue` = col_skip(),
                                    `ResultDepthHeightMeasure/MeasureUnitCode` = col_skip(),
                                    ResultDepthAltitudeReferencePointText = col_skip(),
                                    SubjectTaxonomicName = col_skip(),
                                    SampleTissueAnatomyName = col_skip(),
                                    `ResultAnalyticalMethod/MethodIdentifier` = col_character(),
                                    `ResultAnalyticalMethod/MethodIdentifierContext`= col_character(),
                                    `ResultAnalyticalMethod/MethodName` = col_character(),
                                    MethodDescriptionText = col_character(),
                                    LaboratoryName = col_skip(),
                                    AnalysisStartDate = col_skip(),
                                    ResultLaboratoryCommentText = col_skip(),
                                    DetectionQuantitationLimitTypeName = col_character(),
                                    `DetectionQuantitationLimitMeasure/MeasureValue` = col_double(),
                                    `DetectionQuantitationLimitMeasure/MeasureUnitCode` = col_character(),
                                    PreparationStartDate = col_skip(),
                                    ProviderName = col_skip(),
                                    SampleAquifer = col_skip(),
                                    ResultTimeBasisText = col_skip()))

unlink(temp)

# Streams and Lakes

# Create temp file
temp <- tempfile()

# Download zip
url <- "https://www.waterqualitydata.us/data/Result/search?statecode=US%3A04&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Stream&startDateLo=07-01-2016&startDateHi=06-30-2021&mimeType=csv&zip=yes"
download.file(url ,temp, mode="wb")

# Extract and put in data frame
a.azstream <- read_csv(unz(temp, "result.csv"),
                       col_types = cols(OrganizationIdentifier = col_character(), 
                                        OrganizationFormalName = col_character(), 
                                        ActivityIdentifier = col_character(), 
                                        ActivityTypeCode = col_character(),
                                        ActivityMediaName = col_character(),
                                        ActivityMediaSubdivisionName = col_skip(),
                                        ActivityStartDate = col_date(format = "%Y-%m-%d"), 
                                        `ActivityStartTime/Time` = col_character(),
                                        `ActivityEndTime/TimeZoneCode` = col_skip(),
                                        `ActivityDepthHeightMeasure/MeasureValue` = col_double(),
                                        `ActivityDepthHeightMeasure/MeasureUnitCode` = col_character(),
                                        ActivityDepthAltitudeReferencePointText = col_skip(),
                                        `ActivityTopDepthHeightMeasure/MeasureValue` = col_skip(),
                                        `ActivityTopDepthHeightMeasure/MeasureUnitCode` = col_skip(),
                                        `ActivityBottomDepthHeightMeasure/MeasureValue` = col_skip(),
                                        `ActivityBottomDepthHeightMeasure/MeasureUnitCode` = col_skip(),
                                        ProjectIdentifier = col_skip(),
                                        ActivityConductingOrganizationText = col_character(),
                                        MonitoringLocationIdentifier = col_character(),
                                        ActivityCommentText = col_character(),
                                        HydrologicCondition = col_character(),
                                        HydrologicEvent = col_skip(),
                                        `SampleCollectionMethod/MethodIdentifier` = col_skip(),
                                        `SampleCollectionMethod/MethodIdentifierContext` = col_skip(),
                                        `SampleCollectionMethod/MethodName` = col_skip(),
                                        SampleCollectionEquipmentName = col_skip(),
                                        ResultDetectionConditionText = col_character(),
                                        CharacteristicName = col_character(),
                                        ResultSampleFractionText = col_character(),
                                        ResultMeasureValue = col_double(),
                                        `ResultMeasure/MeasureUnitCode` = col_character(),
                                        MeasureQualifierCode = col_character(),
                                        ResultStatusIdentifier = col_skip(),
                                        StatisticalBaseCode = col_skip(),
                                        ResultValueTypeName = col_skip(),
                                        ResultWeightBasisText = col_skip(),
                                        ResultTemperatureBasisText = col_skip(),
                                        ResultParticleSizeBasisText = col_character(),
                                        PrecisionValue = col_skip(),
                                        ResultCommentText = col_character(),
                                        USGSPCode = col_skip(),
                                        `ResultDepthHeightMeasure/MeasureValue` = col_skip(),
                                        `ResultDepthHeightMeasure/MeasureUnitCode` = col_skip(),
                                        ResultDepthAltitudeReferencePointText = col_skip(),
                                        SubjectTaxonomicName = col_skip(),
                                        SampleTissueAnatomyName = col_skip(),
                                        `ResultAnalyticalMethod/MethodIdentifier` = col_character(),
                                        `ResultAnalyticalMethod/MethodIdentifierContext`= col_character(),
                                        `ResultAnalyticalMethod/MethodName` = col_character(),
                                        MethodDescriptionText = col_character(),
                                        LaboratoryName = col_skip(),
                                        AnalysisStartDate = col_skip(),
                                        ResultLaboratoryCommentText = col_skip(),
                                        DetectionQuantitationLimitTypeName = col_character(),
                                        `DetectionQuantitationLimitMeasure/MeasureValue` = col_double(),
                                        `DetectionQuantitationLimitMeasure/MeasureUnitCode` = col_character(),
                                        PreparationStartDate = col_skip(),
                                        ProviderName = col_skip(),
                                        SampleAquifer = col_skip(),
                                        ResultTimeBasisText = col_skip()))

unlink(temp)

# Bind rows
a.azdata <- rbind(a.azstream, a.usgs)

# Rename columns with / to .
a.azdata <- a.azdata %>% 
  rename(ActivityStartTime.Time = `ActivityStartTime/Time`, 
         ActivityStartTime.TimeZoneCode = `ActivityStartTime/TimeZoneCode`,
         ActivityEndTime.Time = `ActivityEndTime/Time`,
         ActivityDepthHeightMeasure.MeasureValue = `ActivityDepthHeightMeasure/MeasureValue`,
         ActivityDepthHeightMeasure.MeasureUnitCode = `ActivityDepthHeightMeasure/MeasureUnitCode`,
         ResultMeasure.MeasureUnitCode = `ResultMeasure/MeasureUnitCode`,
         ResultAnalyticalMethod.MethodIdentifier = `ResultAnalyticalMethod/MethodIdentifier`,
         ResultAnalyticalMethod.MethodIdentifierContext = `ResultAnalyticalMethod/MethodIdentifierContext`,
         ResultAnalyticalMethod.MethodName = `ResultAnalyticalMethod/MethodName`,
         DetectionQuantitationLimitMeasure.MeasureValue = `DetectionQuantitationLimitMeasure/MeasureValue`,
         DetectionQuantitationLimitMeasure.MeasureUnitCode = `DetectionQuantitationLimitMeasure/MeasureUnitCode`)

# Download AZ sites from portal

# Create temp file
temp <- tempfile()

# Download zip
url <- "https://www.waterqualitydata.us/data/Station/search?statecode=US%3A04&startDateLo=07-01-2016&startDateHi=06-30-2021&mimeType=csv&zip=yes"
download.file(url ,temp, mode="wb")

# Extract and put in data frame
a.azsites <- read_csv(unz(temp, "station.csv"),
                      col_types = cols(MonitoringLocationIdentifier = col_character(), 
                                       OrganizationIdentifier = col_character(),
                                       MonitoringLocationTypeName = col_character(),
                                       LatitudeMeasure = col_double(),
                                       LongitudeMeasure = col_double()))

a.azsites <- a.azsites %>% 
  select(MonitoringLocationIdentifier, OrganizationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure)

unlink(temp)

# Download USGS Sites.

# Create temp file
temp <- tempfile()

# Download zip
url <- paste0("https://www.waterqualitydata.us/data/Station/search?", a.usgssites, "&startDateLo=07-01-2016&startDateHi=06-30-2021&mimeType=csv&zip=yes")
download.file(url ,temp, mode="wb")

# Extract and put in data frame
a.usgssites <- read_csv(unz(temp, "station.csv"),
                        col_types = cols(MonitoringLocationIdentifier = col_character(), 
                                         OrganizationIdentifier = col_character(),
                                         MonitoringLocationTypeName = col_character(),
                                         LatitudeMeasure = col_double(),
                                         LongitudeMeasure = col_double()))

a.usgssites <- a.usgssites %>% 
  select(MonitoringLocationIdentifier, OrganizationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure)

unlink(temp)

a.azsites <- bind_rows(a.azsites, a.usgssites)

# Backups for testing
save(a.azdata, file = "inputs/ZAZDATA.Rdata")
save(a.azsites, file = "inputs/ZAZSITES.Rdata")
# load(file = "inputs/ZAZDATA.Rdata")
# load(file = "inputs/ZAZSITES.Rdata")



#### B -  DATA PREPARATION ####




# Prepare raw data for formatting
b.azdata <- a.azdata
b.azsites <- a.azsites

# Find any temp result with na resultsamplefraction change to Total.  Needed for ammonia.  NA/Total fraction types will not aggregated properly without standization.
b.azdata[grep("Temperature, water", b.azdata$CharacteristicName), "ResultSampleFractionText"] <- "Total"
b.azdata[grep("Escherichia coli", b.azdata$CharacteristicName), "ResultSampleFractionText"] <- "Total"
b.azdata[grep("pH", b.azdata$CharacteristicName), "ResultSampleFractionText"] <- "Total"

# Change just total residual chlorine to total.  Standard and results still total residual.
b.azdata <- b.azdata %>% 
  mutate(ResultSampleFractionText = ifelse(CharacteristicName == "Chlorine" & ResultSampleFractionText == "Total Residual", "Total", ResultSampleFractionText))

# Change "Present Below Quantification Limit" to "Not Detected" in ResultDetectionConditionText.  Same thing for assessment purposes
b.azdata[grep("Present Below Quantification Limit", b.azdata$ResultDetectionConditionText), "ResultDetectionConditionText"] <- "Not Detected"

# Filter for just pH and calculate hydrogen ion concentration (cannot take an average of a log function)
b.ph <- filter(b.azdata, CharacteristicName == "pH")

# Limit ph results to just field.  Exclude lab.
b.ph <- filter(b.ph, ResultAnalyticalMethod.MethodIdentifier == "FIELD" | ResultAnalyticalMethod.MethodIdentifier == "PROBE" | ResultAnalyticalMethod.MethodIdentifier == "FIELD MEASURES")

# Make hion dataframe
XHION <- setNames(data.frame(c("pH", "pH"), c("hionmin", "hionmax")), c("CharacteristicName","newname"))

# Do the join.  Will double records to hionmin and hionmax.
b.ph <- left_join(b.ph, XHION, by = "CharacteristicName")

# Calculate hydrogen ion concentration and get the results ready to put back into b.azdata dataset
b.ph <- b.ph %>%
  select(-CharacteristicName) %>%
  rename(CharacteristicName = newname) %>%
  mutate(hion = 1*10^-ResultMeasureValue) %>%
  select(-ResultMeasureValue) %>%
  rename(ResultMeasureValue = hion)

# Add in pH hydrogen ion data to dataset
b.azdata <- bind_rows(b.azdata, b.ph)

# Select fields for removed records
r.select <- c("WBID", "OrganizationIdentifier", "ActivityStartDate", "ActivityDepthHeightMeasure.MeasureValue", "MonitoringLocationIdentifier", "ResultDetectionConditionText", "ResultSampleFractionText", "CharacteristicName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "ResultCommentText", "DetectionQuantitationLimitTypeName", "DetectionQuantitationLimitMeasure.MeasureValue", "DetectionQuantitationLimitMeasure.MeasureUnitCode", "removereason")

# Removed Records Documentation
r.tribal <- b.azdata %>% 
  filter(OrganizationIdentifier %in% c("AK-CHIN_WQX", "COCOPAH_INDIAN", "CRITEPO_WQX", "FMYN_WQX", "HOPI_WQX", "KBOPWQP", "QUECHAN_WQX", "SRPMIC", "SRPMIC_WQX", "WHITEMOUNTAIN_WQX", "WMAT_WQX", "YAN_WQX")) %>% 
  mutate(WBID = NA) %>% 
  mutate(removereason = "Tribal") %>% 
  select(all_of(r.select))

# Exclude Tribal Data
b.azdata <- b.azdata %>% 
  filter(!OrganizationIdentifier %in% c("AK-CHIN_WQX", "COCOPAH_INDIAN", "CRITEPO_WQX", "FMYN_WQX", "HOPI_WQX", "KBOPWQP", "QUECHAN_WQX", "SRPMIC", "SRPMIC_WQX", "WHITEMOUNTAIN_WQX", "WMAT_WQX", "YAN_WQX"))



#### C - STANDARDIZE DATA ####



# Import Standard Unit table
load(file = "inputs/ZSTDUNIT.RData")

# Join STDUNIT to azdata.  This standardizes units and resolves issues with speciation
c.stddata <- left_join(b.azdata, ZSTDUNIT, by = "ResultMeasure.MeasureUnitCode")

# Removed Records Documentation
r.unit <- c.stddata %>% 
  filter(STDUNIT == "remove") %>% 
  mutate(WBID = NA) %>% 
  mutate(removereason = "Unit") %>% 
  select(r.select)

# Remove any data with newunit = remove.  Some parameters like TKN are in mg/kg.  This removes those units
c.stddata <- filter(c.stddata, STDUNIT != "remove" | is.na(STDUNIT))

# Remove any sediment data not picked up by unit filter
c.stddata <- filter(c.stddata, ActivityMediaName == "Water" | ActivityMediaName == "Other")

# Apply Conversion.  This standardizes results to common units.
c.stddata <- mutate(c.stddata, STDResult = ResultMeasureValue * Conversion)

# Connect to ADEQ Water Quality Database *** Don't share ***
conn <- odbcConnect("com", uid="<username>", pwd="<password>")
 
# Check connection object is open.
odbcGetInfo(conn)
 
# Query the database and put the results into the data frame
YSTA_STATION <- sqlQuery(conn,"select STATION_NAME, STATION_RID, STATION_CD, STATION_ALT_NAME, STATION_TYPE_RID from STA_STATION", rows_at_time = 1,believeNRows = FALSE)
# YSW_SITES <- sqlQuery(conn,"select STATION_NAME, STATION_CD, STATION_ALT_NAME, WATERBODY_TYPE_CD, HUC_CD, RF3_REACH_NO, LATITUDE_DECDEG, LONGITUDE_DECDEG, STATION_ACCESS, STATION_DESC from VW_SW_SITES", rows_at_time = 1,believeNRows = FALSE)
# YUSGS_SITES <- sqlQuery(conn,"select WBID, STATION_ALT_NAME, STATION_ALIAS_ID, STATION_NAME from VW_USGS_WATERBODY_HUC_REACH", rows_at_time = 1,believeNRows = FALSE)
# YWBHUCREACH <- sqlQuery(conn,"select WBID, WATERBODY_NAME, HUC, WATERSHED, REACH_DISTANCE, NUTRIENT, AWC, AWW, AWE, AWEDW, FC, PBC, FBC, DWS, AGI, AGL, LAKE_ACRES, ORIGIN, TERMINUS from VW_WATERBODY_HUC_REACH", rows_at_time = 1,believeNRows = FALSE) # going to use this later for lake acres and stream miles also uses

# #Close connection object.
close(conn)

# Back up incase database connection not accessible
# save(YSTA_STATION, file = "inputs/YSTA_STATION.RData") 
# save(YSW_SITES, file = "inputs/YSWSITES.RData")
# save(YUSGS_SITES, file = "inputs/YUSGS_SITES.RData") 
# save(YWBHUCREACH, file = "inputs/YWBHUCREACH.RData")

# Use if slow or no connection via ODBC
load(file = "inputs/YSTA_STATION.RData")
load(file = "inputs/YSWSITES.RData")
load(file = "inputs/YUSGS_SITES.RData")
load(file = "inputs/YWBHUCREACH.RData")

# Standardize WBID
# YWBHUCREACHJDJ <- YWBHUCREACH %>%
#   mutate(WBIDCOPY = WBID) %>%
#   separate(WBIDCOPY, c("c", "d", "e"), sep = "([\\Z\\L])") %>%
#   mutate(WBID = ifelse(d == "", e, d)) %>%
#   select(-c, -d, -e) %>%
#   separate(WATERBODY_NAME, c("WATERBODY_NAME"), sep = "-") %>%
#   mutate(ORIGIN = as.character(ORIGIN)) %>%
#   mutate(TERMINUS = as.character(TERMINUS))

YWBHUCREACH$WBID <- sub("^[AZL]", "", substring(YWBHUCREACH$WBID, 3))

YWBHUCREACH <- YWBHUCREACH %>% 
  separate(WATERBODY_NAME, c("WATERBODY_NAME"), sep = "-") %>% 
  mutate(ORIGIN = as.character(ORIGIN)) %>% 
  mutate(TERMINUS = as.character(TERMINUS))

# USGS sites with WBID from WQDB
YUSGS_SITES <- YUSGS_SITES %>% 
  distinct(WBID, STATION_ALT_NAME, STATION_ALIAS_ID, STATION_NAME) %>% 
  left_join(YSTA_STATION, by = "STATION_NAME") %>% 
  left_join(YSW_SITES, by = "STATION_NAME") %>% 
  mutate(WATERBODY_TYPE_CD = ifelse(STATION_TYPE_RID == 0, "S",
                                    ifelse(STATION_TYPE_RID == 1, "L", "O"))) %>% 
  mutate(MonitoringLocationIdentifier = paste0("USGS-", STATION_ALIAS_ID)) %>% # paste0 is like paste but no spaces
  select(WBID, MonitoringLocationIdentifier, WATERBODY_TYPE_CD, STATION_ALT_NAME.x, LATITUDE_DECDEG, LONGITUDE_DECDEG) %>% 
  rename(STATION_ALT_NAME = STATION_ALT_NAME.x)

# ADEQ Sites with WBID from WQDB
YSW_SITES <- YSW_SITES %>% 
  left_join(YSTA_STATION, by = "STATION_NAME") %>% 
  mutate(WBID = paste(HUC_CD, RF3_REACH_NO, sep = "-")) %>% 
  mutate(MonitoringLocationIdentifier = paste0("AZDEQ_SW-", STATION_RID))

# Spin off a Y Sites that has the connection between sites, RIDs and WBID.  Use later for fish
c.sites <- YSW_SITES %>% 
  select(STATION_NAME, STATION_CD.x, STATION_ALT_NAME.x, WBID, MonitoringLocationIdentifier) %>% 
  rename(DEQNUM = STATION_NAME) %>% 
  rename(SiteID = STATION_CD.x) 

# Sites with monitoring location and WBID connection  
YSW_SITES <- YSW_SITES %>% 
  select(WBID, MonitoringLocationIdentifier, WATERBODY_TYPE_CD, STATION_ALT_NAME.x, LATITUDE_DECDEG, LONGITUDE_DECDEG, STATION_ACCESS) %>% 
  rename(STATION_ALT_NAME = STATION_ALT_NAME.x)

# Manually Updated Sites.  Could be replaced by Doug McCarty's automated assignment of WBID.  For now use the c.nowbid to identify unmatched WBIDs and use GIS to associate
YSITESMANUAL <- read_csv("inputs/YSITESMANUAL.csv",
                         col_types = cols(WBID = col_character(), 
                                          MonitoringLocationIdentifier = col_character(), 
                                          WATERBODY_TYPE_CD = col_character(), 
                                          STATION_ALT_NAME = col_character(), 
                                          LATITUDE_DECDEG = col_double(), 
                                          LONGITUDE_DECDEG = col_double()))

# Combine Sites
YSITESALL <- bind_rows(YSW_SITES, YUSGS_SITES, YSITESMANUAL)
YSITESALL <- distinct(YSITESALL, WBID, MonitoringLocationIdentifier, .keep_all = TRUE) # .keep_all allows distinct to work and keep the columns.

# Add WBID and siteid to monitoring location identifier.
c.nowbid <- full_join(c.stddata, YSITESALL, by = "MonitoringLocationIdentifier")

# Filter for just sites with no WBID.
c.nowbid <- c.nowbid %>%
  left_join(b.azsites, by = "MonitoringLocationIdentifier") %>%
  filter(is.na(WBID)) %>%
  filter(!OrganizationIdentifier.x == "21ARIZ_WQX") %>%
  filter(ActivityMediaName == "Water") %>%
  select(MonitoringLocationIdentifier, OrganizationIdentifier.x, LatitudeMeasure, LongitudeMeasure, ActivityIdentifier) %>%
  distinct(MonitoringLocationIdentifier, .keep_all = TRUE)


# Make field that shows duplicate data (same site, date, time, parameter, fraction, depth, result) and exclude
c.stddata$concate <- paste(c.stddata$CharacteristicName, c.stddata$ResultSampleFractionText, c.stddata$ActivityStartDate, c.stddata$ActivityStartTime.Time, c.stddata$ActivityDepthHeightMeasure.MeasureValue, c.stddata$ResultMeasureValue, c.stddata$ActivityTypeCode, c.stddata$MonitoringLocationIdentifier)

# Document removed duplicates
r.duplicated <- c.stddata %>% 
  mutate(diff = duplicated(concate)) %>% 
  filter(diff == "TRUE") %>% 
  mutate(removereason = "Duplicate") %>%
  mutate(WBID = NA) %>% 
  select(r.select)

# Find Duplicate data (same site, date, time, parameter, fraction, depth, result) and exclude
c.stddata <- c.stddata %>% 
  mutate(diff = duplicated(concate)) %>% 
  filter(diff != "TRUE")

# Need to ensure antijoin and inner join work properly
YSITESALL <- YSITESALL %>% 
  distinct(MonitoringLocationIdentifier, .keep_all = TRUE)

# Removed Records Documentation
r.nowbid <- anti_join(c.stddata, YSITESALL, by = "MonitoringLocationIdentifier") %>% 
  mutate(WBID = NA) %>% 
  mutate(removereason = "No WBID") %>% 
  select(r.select)

# Associate WQX data with WBID from DEQ database.  Innerjoin because only want data from c.stddata with WBID.
c.stddata <- inner_join(c.stddata, YSITESALL, by = "MonitoringLocationIdentifier")

# Open standard detection unit table
ZSTDDETECTUNIT <- ZSTDUNIT %>% 
  rename(DetectionQuantitationLimitMeasure.MeasureUnitCode = ResultMeasure.MeasureUnitCode) %>% 
  rename(DLConversion = Conversion) %>% 
  rename(STDDETECTUNIT = STDUNIT)

# Make Total Recoverable Fraction = Total
c.stddata[grep("Total Recoverable", c.stddata$ResultSampleFractionText), "ResultSampleFractionText"] <- "Total" 
c.stddata[grep("Recoverable", c.stddata$ResultSampleFractionText), "ResultSampleFractionText"] <- "Total" 

### Temporary Code.  Remove once USGS adds detection limits back to the portal ###
library(dataRetrieval)

# Open USGS Sites with DL issues
load(file = "inputs/ZUSGSDLSITES.RData")

siteNumber <- unique(ZUSGSDLSITES$siteNumber)

# Pull NWIS data
dfLong <- readNWISqw(siteNumber, parameterCd = "All", startDate, endDate)

# Open the map
load(file = "inputs/ZNWISTOWQPMAP.RData")

# Format.  USGS uses leading 0's.  str_pad takes care of this.
nwistowqpMAP <- nwistowqpMAP %>% 
  select(parameter_cd, CharacteristicName, NWISParam, ResultSampleFractionText, DLUnit) %>% 
  mutate(parm_cd = as.character(parameter_cd)) %>% 
  mutate(parm_cd = str_pad(parm_cd, 5, pad = "0"))  

# Join map to dflong
dfLong22 <- dfLong %>% 
  left_join(nwistowqpMAP, by = "parm_cd")

# Short version to prep for join to c.stddata
dfLong222 <- dfLong22 %>% 
  select(site_no, sample_dt, rpt_lev_va, CharacteristicName, ResultSampleFractionText, DLUnit) %>% 
  mutate(MonitoringLocationIdentifier = paste0("USGS-", site_no)) %>% 
  filter(CharacteristicName != "None") %>% 
  filter(!is.na(rpt_lev_va)) %>% 
  distinct(MonitoringLocationIdentifier, sample_dt, CharacteristicName, ResultSampleFractionText, rpt_lev_va, DLUnit)

# Special characters introduced.  Replace them for join to include nitrate.
dfLong222[grep("nitratenitrite", dfLong222$CharacteristicName), "CharacteristicName"] <- "Inorganic nitrogen (nitrate and nitrite)"
dfLong222[grep("tkn", dfLong222$CharacteristicName), "CharacteristicName"] <- "Kjeldahl nitrogen"

# Final Join of NWIS detection limits to WQP
c.stddata <- c.stddata %>% 
  left_join(dfLong222, by = c("MonitoringLocationIdentifier", "CharacteristicName", "ActivityStartDate" = "sample_dt", "ResultSampleFractionText")) %>% 
  # filter(MonitoringLocationIdentifier == "USGS-09380000", CharacteristicName == "Iron") %>% 
  mutate(DetectionQuantitationLimitMeasure.MeasureValue = ifelse(is.na(DetectionQuantitationLimitMeasure.MeasureValue), rpt_lev_va, DetectionQuantitationLimitMeasure.MeasureValue)) %>% 
  mutate(DetectionQuantitationLimitMeasure.MeasureUnitCode = ifelse(is.na(DetectionQuantitationLimitMeasure.MeasureUnitCode), DLUnit, DetectionQuantitationLimitMeasure.MeasureUnitCode)) 
  
### END Tempary fix ###

# Join to c.stddata
c.stddata <- left_join(c.stddata, ZSTDDETECTUNIT, by = "DetectionQuantitationLimitMeasure.MeasureUnitCode")  

# Take 1/2 the detection limit so aggregation takes into account nondetects.  Note:  This join excludes all data without detection limit units which includes FIELD data

# Apply conversion.  This standardizes detection limits to common units.
c.stddata <- mutate(c.stddata, STDDETECTLIMIT = DetectionQuantitationLimitMeasure.MeasureValue * DLConversion)

# Identify field data
c.stddata[grep("FIELD|PROBE", c.stddata$ResultAnalyticalMethod.MethodIdentifier), "DetectionQuantitationLimitMeasure.MeasureUnitCode"] <- "FIELD" 

# Add column for half detection
c.stddata <- mutate(c.stddata, halfdl = 0.5 * STDDETECTLIMIT) 

# Find the NA's in STDResult
c.stddata$STDResult[is.na(c.stddata$STDResult)] <- 999999

# use 999999 to identify nondetects
c.stddata <- mutate(c.stddata, ndorresult = ifelse(STDResult == 999999, "nondetect", "detect")) 

# Keep the result if it exists if not replace with half detection limit
c.stddata <- within(c.stddata, STDResult[ndorresult == "nondetect"] <- (halfdl[ndorresult == "nondetect"])) 

# Remove Records Documentation
r.nodetect <- c.stddata %>% 
  filter(ResultDetectionConditionText == "Not Detected") %>% 
  filter(is.na(DetectionQuantitationLimitMeasure.MeasureValue)) %>% 
  mutate(removereason = "Not Detected and no DL value") %>% 
  select(r.select)

# Filter out any data that is missing detection limits and is not detected
c.stddata <- c.stddata %>% 
  mutate(temp = ifelse(ResultDetectionConditionText == "Not Detected" & is.na(DetectionQuantitationLimitMeasure.MeasureValue), "remove", "keep")) %>% 
  filter(temp != "remove" | is.na(temp))

# Impute dissolved if hardness fraction type is null.  Common issue with USGS data
c.stddata <- c.stddata %>% 
  mutate(ResultSampleFractionText = ifelse(CharacteristicName == "Hardness, Ca, Mg" & is.na(ResultSampleFractionText), "Dissolved", ResultSampleFractionText))

# Join site data with results
c.stddata <- left_join(c.stddata, b.azsites, by = c("MonitoringLocationIdentifier", "OrganizationIdentifier"))

# Change Lake, Reservoir, Impoundment to lake and river/stream to stream
c.stddata[grep("Lake, Reservoir, Impoundment", c.stddata$MonitoringLocationTypeName), "MonitoringLocationTypeName"] <- "Lake" 
c.stddata[grep("River/Stream", c.stddata$MonitoringLocationTypeName), "MonitoringLocationTypeName"] <- "Stream" 

# Replace depth NA with 0 if stream, but not for lake. Streams don't need depth for assessment, lakes do.
c.stddata <- c.stddata %>% 
  mutate(ActivityDepthHeightMeasure.MeasureValue = ifelse(is.na(ActivityDepthHeightMeasure.MeasureValue) & MonitoringLocationTypeName != "Lake", 0, ActivityDepthHeightMeasure.MeasureValue)) 

# Some organizations use just oxygen not dissolved oxygen.  Use common CharacteristicName.
c.stddatao2 <- c.stddata %>% filter(CharacteristicName == "Oxygen")
c.stddata <- c.stddata %>% filter(CharacteristicName != "Oxygen")

# Crosswalk dissolved oxygen data
c.stddatao2[grep("mg/l", c.stddatao2$ResultMeasure.MeasureUnitCode), "CharacteristicName"] <- "Dissolved oxygen (DO)"
c.stddatao2[grep("% saturatn", c.stddatao2$ResultMeasure.MeasureUnitCode), "CharacteristicName"] <- "Dissolved oxygen saturation"

# Some organizations use different nutrient names.  Convert to common schema.
c.stddata[grep("Nitrogen, mixed forms ", c.stddata$CharacteristicName), "CharacteristicName"] <- "Nitrogen"

# Combine oxygen subset back to main dataset
c.stddata <- bind_rows(c.stddata,c.stddatao2)

# Removed Records Documentation
r.oxygenmgl <- c.stddata %>% 
  filter(CharacteristicName == "Dissolved oxygen (DO)") %>%  
  filter(ActivityDepthHeightMeasure.MeasureValue > 1 | is.na(ActivityDepthHeightMeasure.MeasureValue)) %>%
  filter(MonitoringLocationTypeName == "Lake") %>% 
  mutate(removereason = "Oxygen") %>% 
  select(r.select)

r.oxygenper <- c.stddata %>% 
  filter(CharacteristicName == "Dissolved oxygen saturation") %>%  
  filter(ActivityDepthHeightMeasure.MeasureValue > 1 | is.na(ActivityDepthHeightMeasure.MeasureValue)) %>%
  filter(MonitoringLocationTypeName == "Lake") %>% 
  mutate(removereason = "Oxygen") %>% 
  select(r.select)

# Remove all Dissolved Oxygen that is > 1 m in depth for a lake.  1.0 m would be included per R18-11-109.
c.stddata <- filter(c.stddata, ActivityDepthHeightMeasure.MeasureValue <= 1 | MonitoringLocationTypeName != "Lake" | CharacteristicName != "Dissolved oxygen (DO)")
c.stddata <- filter(c.stddata, ActivityDepthHeightMeasure.MeasureValue <= 1 | MonitoringLocationTypeName != "Lake" | CharacteristicName != "Dissolved oxygen saturation")

# Filter out QC data.  First create a list of possible QC options.
c.ActivityTypeCode <- c("Quality Control Sample-Field Replicate",
                        "Quality Control Sample-Inter-lab Split",
                        "Field Msr/Obs-Habitat Assessment",
                        "Quality Control Sample-Field Blank",
                        "Quality Control Sample-Equipment Blank",
                        "Quality Control Sample-Lab Matrix Spike",
                        "Quality Control Sample-Lab Matrix Spike Duplicate",
                        "Quality Control Sample-Lab Spike",
                        "Field Msr/Obs-Portable Data Logger",
                        "Quality Control Field Replicate Portable Data Logger",
                        "Quality Control Field Replicate Msr/Obs",
                        "Sample-Integrated Vertical Profile")

# Removed Records Documentation
r.qc <- c.stddata %>% 
  filter(ActivityTypeCode %in% c.ActivityTypeCode) %>% 
  mutate(removereason = "QC") %>% 
  select(r.select)

# Then filter out by that list
c.stddata <- c.stddata %>% filter(!(ActivityTypeCode %in% c.ActivityTypeCode))

# Resolve any missing organizations
c.stddata <- c.stddata %>% 
  mutate(ActivityConductingOrganizationText = ifelse(is.na(ActivityConductingOrganizationText), OrganizationIdentifier, ActivityConductingOrganizationText))

# Removed Records Documentation
r.notcredible <- c.stddata %>% 
  filter(ActivityConductingOrganizationText == "Friends of the Santa Cruz River, Tubac, AZ", ActivityStartDate < "2019-04-19") %>% 
  mutate(removereason = "Not Credible") %>% 
  select(r.select)

# Step 1 Subset the portion that is credible
c.stddata.p1 <- c.stddata %>% 
  filter(ActivityStartDate >= "2019-04-09", ActivityConductingOrganizationText == "Friends of the Santa Cruz River, Tubac, AZ")

# Step 2 Exclude the non-credible organization (acceptable data based on date will be filtered back in next)
c.stddata.p2 <- c.stddata %>% 
  filter(ActivityConductingOrganizationText != "Friends of the Santa Cruz River, Tubac, AZ")

c.stddata <- bind_rows(c.stddata.p1, c.stddata.p2)

# Removed Records Documentation
r.usgsnotcredible <- c.stddata %>% 
  filter((WBID == "15010004-001B" & CharacteristicName == "Selenium")) %>% # USGS indicated not credible
  mutate(removereason = "Not Credible") %>% 
  select(r.select)

# One off for USGS
c.stddata <- c.stddata %>% 
  filter(!(WBID == "15010004-001B" & CharacteristicName == "Selenium")) # USGS indicated not credible

# Add column to identify 'not for listing' flag.  This will be used to later exclude data with this field that has an exceedance
c.stddata <- c.stddata %>% mutate(nfl = "N")

# Identify all NFL in one easy to read column.
c.stddata[grep("NFL", c.stddata$ResultCommentText), "nfl"] <- "Y"
c.stddata[grep("E4|E8", c.stddata$ResultCommentText), "nfl"] <- "N"

# Removed Records Documentation
r.nfl <- c.stddata %>% 
  filter(nfl == "Y") %>% 
  mutate(removereason = "nfl") %>% 
  select(r.select)

# Filter out data that was flagged 'not for listing'
c.stddata <- c.stddata %>% 
  filter(nfl != "Y")

# Don't use Rejected Records
c.stddata <- c.stddata %>% 
  filter(MeasureQualifierCode != "R" | is.na(MeasureQualifierCode))

# Add water quality improvements.  These are used to filter out data before the improvement was made.  Resets the data.
ZIMPROVEMENTS <- read_csv("inputs/ZIMPROVEMENTS.csv", 
                          col_types = cols(
                            WBID = col_character(),
                            WATERBODY_DESC = col_character(),
                            improvementdate = col_date(format = "%m/%d/%Y"),
                            improvement = col_character()))
 
# Removed Records Documentation
r.improve <- c.stddata %>% 
  left_join(ZIMPROVEMENTS, by = "WBID") %>% 
  mutate(improvexclude = ifelse(ActivityStartDate < improvementdate, "Y", "N")) %>% 
  filter(improvexclude == "Y") %>%
  mutate(removereason = "Improvement") %>% 
  select(r.select)

# Join to dataset then exclude improved data
c.stddata <- c.stddata %>% 
  left_join(ZIMPROVEMENTS, by = "WBID") %>% 
  mutate(improvexclude = ifelse(ActivityStartDate < improvementdate, "Y", "N")) %>% 
  filter(improvexclude != "Y" | is.na(improvexclude))

c.stddata$concate <- paste(c.stddata$WBID, c.stddata$CharacteristicName, c.stddata$ResultSampleFractionText, c.stddata$ActivityStartDate, c.stddata$ActivityStartTime.Time, c.stddata$ActivityDepthHeightMeasure.MeasureValue, c.stddata$ResultMeasureValue, c.stddata$ActivityTypeCode, c.stddata$MonitoringLocationIdentifier)

# Removed Records Documentation
r.duplicated <- c.stddata %>% 
  mutate(diff = duplicated(concate)) %>% 
  filter(diff == "TRUE") %>% 
  mutate(removereason = "Duplicate") %>% 
  select(r.select)

# Find Duplicate data (same site, date, time, parameter, fraction, depth, result) and exclude
c.stddata <- c.stddata %>% 
  mutate(diff = duplicated(concate)) %>% 
  filter(diff != "TRUE")

# Some Dissolved oxygen missing fraction type.  Add dissolved.
c.stddata[grepl("Dissolved oxygen", c.stddata$CharacteristicName), "ResultSampleFractionText"] <- "Dissolved"  #Note: grep does not handle NA substitution but grepl doe

# Change Uranium 238 to Uranium (most uranium is uranium 238)
c.stddata[grep("Uranium-238", c.stddata$CharacteristicName), "CharacteristicName"] <- "Uranium"

# If Suspended Sediment Concentration not already calculated then calculate based on fine/coarse fractions
c.ssc <- c.stddata %>% 
  filter(CharacteristicName == "Suspended Sediment Concentration (SSC)") %>% 
  select(WBID, CharacteristicName, ActivityStartDate, STDResult, ResultParticleSizeBasisText) 

c.ssc$ResultParticleSizeBasisText[is.na(c.ssc$ResultParticleSizeBasisText)] <- "CALCULATED"

# SSC is acute so taking the max if there are multiple samples on same date.  Need to identify unique 'cases' before spread can work.  
c.ssc <- c.ssc %>% 
  group_by(WBID, CharacteristicName, ActivityStartDate, ResultParticleSizeBasisText) %>% 
  summarise(SSC = max(STDResult)) 

c.sscsp <- c.ssc %>% 
  spread(ResultParticleSizeBasisText, SSC)

# Make NA's = 0
c.sscsp[is.na(c.sscsp)] <- 0

# Prioritize calculated.  If no calculated then take sum of coarse and fine
c.sscsp <- c.sscsp %>% 
  ungroup() %>% 
  mutate(STDResult = ifelse(CALCULATED > 0, CALCULATED, Coarse + Fine)) %>% 
  select(WBID, ActivityStartDate, STDResult) %>% 
  mutate(CharacteristicName = "newssc")

# Add back to dataset
c.stddata <- bind_rows(c.stddata, c.sscsp)

c.stddata <- c.stddata %>% select(WBID, ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ResultDetectionConditionText, ResultSampleFractionText, STDResult, STDUNIT, STDDETECTLIMIT, STDDETECTUNIT, everything())

# Narrow to just fields needed.  Always follow format of WBID, USE, Parameter
c.stddata2 <- select(c.stddata,
                     WBID,
                     ActivityStartDate,
                     MonitoringLocationIdentifier,
                     CharacteristicName,
                     ResultDetectionConditionText,
                     ResultSampleFractionText,
                     STDUNIT,
                     STDResult,
                     STDDETECTLIMIT,
                     STDDETECTUNIT,
                     OrganizationIdentifier,
                     ActivityConductingOrganizationText,
                     ResultCommentText,
                     ResultAnalyticalMethod.MethodIdentifier,
                     ResultAnalyticalMethod.MethodName,
                     ActivityCommentText,
                     ndorresult, 
                     HydrologicCondition)





#### D - AGGREGATION BY TIME / ITERATION BY DESIGNATED USE ####



# Import current designated uses.  Note YWBHUCREACH is an ODBC connection.
d.usejoin <- left_join(c.stddata, YWBHUCREACH, by = "WBID")

# Select Just columns needed
d.usejoin <- select(d.usejoin,
                    ActivityTypeCode, 
                    ActivityStartDate, 
                    ActivityStartTime.Time, 
                    ActivityConductingOrganizationText,
                    MonitoringLocationIdentifier,
                    ActivityCommentText,
                    ResultDetectionConditionText,
                    CharacteristicName,
                    ResultSampleFractionText,
                    ResultMeasureValue,
                    ResultMeasure.MeasureUnitCode,
                    ResultCommentText,
                    ResultAnalyticalMethod.MethodIdentifier,
                    ResultAnalyticalMethod.MethodName,
                    DetectionQuantitationLimitMeasure.MeasureValue,
                    DetectionQuantitationLimitMeasure.MeasureUnitCode,
                    STDUNIT,
                    STDResult,
                    WBID,
                    REACH_DISTANCE,
                    NUTRIENT,
                    AWC,
                    AWW,
                    AWE,
                    AWEDW,
                    FC,
                    FBC,
                    PBC,
                    DWS,
                    AGI,
                    AGL,
                    STDDETECTLIMIT,
                    STDDETECTUNIT,
                    ndorresult)

# Gather data (iterate by use)
d.usejoin <- gather(d.usejoin, Use, UseCode, 
                    -ActivityTypeCode, 
                    -ActivityStartDate, 
                    -ActivityStartTime.Time, 
                    -ActivityConductingOrganizationText,
                    -MonitoringLocationIdentifier,
                    -ActivityCommentText,
                    -ResultDetectionConditionText,
                    -CharacteristicName,
                    -ResultSampleFractionText,
                    -ResultMeasureValue,
                    -ResultMeasure.MeasureUnitCode,
                    -ResultCommentText,
                    -ResultAnalyticalMethod.MethodIdentifier,
                    -ResultAnalyticalMethod.MethodName,
                    -DetectionQuantitationLimitMeasure.MeasureValue,
                    -DetectionQuantitationLimitMeasure.MeasureUnitCode,
                    -STDUNIT,
                    -STDResult,
                    -WBID,
                    -REACH_DISTANCE,
                    -NUTRIENT,
                    -STDDETECTLIMIT,
                    -STDDETECTUNIT,
                    -ndorresult)

# Filter to just the designated uses that apply
d.usejoin <- filter(d.usejoin, UseCode == "Y")



## 5 AGGREGATION BY TIME ##



# Split/Copy Aquatic and Wildlife into Acute/Chronic
XUSECROSSWALK <- setNames(data.frame(c("AWW", "AWW", "FC", "FBC", "AGL", "AGI", "PBC", "DWS", "AWEDW", "AWEDW", "AWE", "AWC", "AWC"), 
                                      c("AWWChronic", "AWWAcute", "FC", "FBC", "AGL", "AGI", "PBC", "DWS", "AWEDWChronic", "AWEDWAcute", "AWEAcute", "AWCChronic", "AWCAcute")), c("Use","NewUse"))

d.usejoin <- left_join(d.usejoin, XUSECROSSWALK, by = "Use")

# Create Mapping that says which STEP 1 Temporal Aggregation Rule Applies; AW Acute = max (except DO), pH = min, all others = mean
d.usejoin <- mutate(d.usejoin, Steponemap = ifelse(NewUse == "AWWAcute", "Maximum", "Mean"))
d.usejoin[grep("AWCAcute", d.usejoin$NewUse), "Steponemap"] <- "Maximum"  
d.usejoin[grep("AWEDWCAcute", d.usejoin$NewUse), "Steponemap"] <- "Maximum"  
d.usejoin[grep("AWEAcute", d.usejoin$NewUse), "Steponemap"] <- "Maximum"  

# Add in exceptions for DO and pH
d.usejoin[grep("Dissolved oxygen \\(DO)", d.usejoin$CharacteristicName), "Steponemap"] <- "Minimum" # double backslash escapes the () special character
d.usejoin[grep("Dissolved oxygen saturation", d.usejoin$CharacteristicName), "Steponemap"] <- "Minimum" 
d.usejoin[grep("hionmin", d.usejoin$CharacteristicName), "Steponemap"] <- "Minimum"  
d.usejoin[grep("hionmax", d.usejoin$CharacteristicName), "Steponemap"] <- "Maximum"  

# Captilalize characteristicname to match with standards table
d.usejoin$CharacteristicName <- toupper(d.usejoin$CharacteristicName) 

# Remove Duplicates
d.usejoin <- d.usejoin %>% 
  distinct(WBID, ActivityStartDate, ActivityStartTime.Time, MonitoringLocationIdentifier, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, Use, NewUse, .keep_all = TRUE)

# Look for dissolved results that don't have paired total results.  Use dissolved in place of total if present and add to dataset.
d.usejoin2 <- d.usejoin %>%
  spread(ResultSampleFractionText, STDResult) 

# Combine all fractions based on priority (total then dissolved if no total)
d.usejoin2$newtotal <- ifelse(!is.na(d.usejoin2$Total), d.usejoin2$Total, 
                                   ifelse(!is.na(d.usejoin2$Dissolved), d.usejoin2$Dissolved, NA))

# Filter for just dissolved that don't have total results.  
d.usejoin3 <- filter(d.usejoin2, is.na(Total))

# Rename column names back to total
d.usejoin3 <- d.usejoin3 %>% 
  rename(STDResult = newtotal) %>% 
  rename(ResultSampleFractionText = Total) %>% 
  mutate(ResultSampleFractionText = "Total") %>% 
  select(ActivityTypeCode, ActivityStartDate, ActivityStartTime.Time, ActivityConductingOrganizationText, MonitoringLocationIdentifier, ActivityCommentText,
         ResultDetectionConditionText, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ResultCommentText, 
         ResultAnalyticalMethod.MethodIdentifier, ResultAnalyticalMethod.MethodName, DetectionQuantitationLimitMeasure.MeasureValue, 
         DetectionQuantitationLimitMeasure.MeasureUnitCode, STDUNIT, STDResult, WBID, REACH_DISTANCE, NUTRIENT, STDDETECTLIMIT, 
         STDDETECTUNIT, ndorresult, Use, UseCode, NewUse, Steponemap)

# Add dissolved data with no total pairs back to dataset
d.usejoin <- rbind(d.usejoin, d.usejoin3)

# Need to account for situations where in the same 7 days there are dl issues and not dl issues.  Do this with distinct.
d.dlagg <- d.usejoin %>% 
  distinct(ActivityTypeCode, ActivityStartDate, ActivityStartTime.Time, ActivityConductingOrganizationText, MonitoringLocationIdentifier,
           ActivityCommentText, ResultDetectionConditionText, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, 
           ResultCommentText, ResultAnalyticalMethod.MethodIdentifier, ResultAnalyticalMethod.MethodName, DetectionQuantitationLimitMeasure.MeasureValue, 
           DetectionQuantitationLimitMeasure.MeasureUnitCode, STDUNIT, STDResult, WBID, REACH_DISTANCE, NUTRIENT, STDDETECTLIMIT, 
           STDDETECTUNIT, ndorresult, Use, UseCode, NewUse, Steponemap)

# 2 columns for detection and nondetection so that you know whether to remove a sample
d.dlagg <- d.dlagg %>% 
  mutate(number = 1) %>% 
  spread(ndorresult, number) 

# Add zeros
d.dlagg$detect[is.na(d.dlagg$detect)] <- 0
d.dlagg$nondetect[is.na(d.dlagg$nondetect)] <- 0

# Tally the detects and nondetects
d.dlagg <- d.dlagg %>% 
  group_by(WBID, aggdate = floor_date(ActivityStartDate, "1 week"), CharacteristicName, ResultSampleFractionText, NewUse) %>%
  summarize(detect = sum(detect), nondetect = sum(nondetect), maxdl = max(STDDETECTLIMIT))

# Keep all nondetects.  If nd and detect both present in same week treat as detect.
d.dlagg <- d.dlagg %>% 
  mutate(dldescription = ifelse(detect > 0 & nondetect > 0, "detect",
                                ifelse(detect > 0, "detect", 
                                       ifelse(nondetect > 0, "nondetect", "other")))) %>% 
  filter(dldescription == "nondetect") %>% 
  mutate(countdl = 1)

# Apply Step 1 aggregation based on mapping

# Filter by stepone map so can summarize/aggregate
d.timemean <- filter(d.usejoin, Steponemap == "Mean")
d.timemax <- filter(d.usejoin, Steponemap == "Maximum")
d.timemin <- filter(d.usejoin, Steponemap == "Minimum")

# Summarize step 1 results by 7 days
d.timemeanagg <- d.timemean %>% 
  group_by(WBID, MonitoringLocationIdentifier, aggdate = floor_date(ActivityStartDate, "1 week"), CharacteristicName, ResultSampleFractionText, NewUse) %>%
  summarize(aggtime = mean(STDResult)) 

d.timemaxagg <- d.timemax %>% 
  group_by(WBID, MonitoringLocationIdentifier, aggdate = floor_date(ActivityStartDate, "1 week"), CharacteristicName, ResultSampleFractionText, NewUse) %>%
  summarize(aggtime = max(STDResult))

d.timeminagg <- d.timemin %>% 
  group_by(WBID, MonitoringLocationIdentifier, aggdate = floor_date(ActivityStartDate, "1 week"), CharacteristicName, ResultSampleFractionText, NewUse) %>%
  summarize(aggtime = min(STDResult))

# Combine aggregated data into one dataframe
d.timeaggfinal <- bind_rows(d.timemeanagg, d.timemaxagg, d.timeminagg)

d.timeaggfinal3 <- d.timeaggfinal

# Remove dissolved oxygen.  The others will be picked off if there are no standards.  DO doesn't have a defined fraction type in AZ standards.
d.timeaggfinal3 <- filter(d.timeaggfinal3, !grepl("DISSOLVED OXYGEN", CharacteristicName, fixed = TRUE))
d.timeaggfinal3 <- filter(d.timeaggfinal3, !grepl("SEDIMENT CONC", CharacteristicName, fixed = TRUE))




#### E - AGGREGATE TO ASSESSMENT UNIT / WBID ####



# Set up two choices.  Almost every parameter = maximum. DO is a minimum.  pH is a Max and Min
e.spaceaggregate <- mutate(d.timeaggfinal, Steptwomap = ifelse(CharacteristicName == "DISSOLVED OXYGEN (DO)", "Minimum", "Maximum"))

# Add in exceptions for pH and oxygen saturation
e.spaceaggregate[grep("HIONMAX", e.spaceaggregate$CharacteristicName), "Steptwomap"] <- "Maximum"
e.spaceaggregate[grep("HIONMIN", e.spaceaggregate$CharacteristicName), "Steptwomap"] <- "Minimum"
e.spaceaggregate[grep("DISSOLVED OXYGEN SATURATION", e.spaceaggregate$CharacteristicName), "Steptwomap"] <- "Minimum"

# Apply Step 2 aggregation based on mapping

# Filter by steptwo map so can summarize/aggregate
e.spacemax <- filter(e.spaceaggregate, Steptwomap == "Maximum")
e.spacemin <- filter(e.spaceaggregate, Steptwomap == "Minimum")

# Summarize step 2 results by worst case (min max)
e.spacemaxagg <- e.spacemax %>% group_by(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse) %>%
  summarize(aggtimespace = max(aggtime))
e.spaceminagg <- e.spacemin %>% group_by(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse) %>%
  summarize(aggtimespace = min(aggtime))

# Combine aggregated data into one dataframe
e.spaceaggregatefinal <- bind_rows(e.spacemaxagg, e.spaceminagg)

# Replace hardness values > 400 with 400.  Note there are multiple characteristics for hardness that need resolved
e.spaceaggregatefinal <- within(e.spaceaggregatefinal, aggtimespace[CharacteristicName == 'HARDNESS, CA, MG' & aggtimespace >= 400] <- 400)
e.spaceaggregatefinal <- within(e.spaceaggregatefinal, aggtimespace[CharacteristicName == 'TOTAL HARDNESS' & aggtimespace >= 400] <- 400)
e.spaceaggregatefinal <- within(e.spaceaggregatefinal, aggtimespace[CharacteristicName == 'HARDNESS, NON-CARBONATE' & aggtimespace >= 400] <- 400)

# See if there is any critical condition records to exclude
ZCRITDATA <- read_csv("inputs/ZCRITDATA.csv", 
                      col_types = cols(
                        WBID = col_character(),
                        aggdate = col_date(format = "%m/%d/%Y"),
                        CharacteristicName = col_character(),
                        ResultSampleFractionText = col_character(),
                        NewUse = col_character(),
                        aggtimespace = col_double(),
                        critmet = col_character()))

# Just select the fields from e.spaceaggregatefinal plus the human added critmet
ZCRITDATA <- ZCRITDATA %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, critmet) 

# Join critmet filter out the No's
e.spaceaggregatefinal <- e.spaceaggregatefinal %>% 
  left_join(ZCRITDATA, by = c("WBID", "aggdate", "CharacteristicName", "ResultSampleFractionText", "NewUse", "aggtimespace")) %>% 
  filter(!critmet == "N" | is.na(critmet))

# Core and Seasonal Check - Looks for core parameters and seasonal distribution which is used in use support and attainment decisions

# Set the acute and e coli range which is three years before the end of the assessment
acuteDate <- endDate - 1095 # 3*365 = 1095 days.  

# Narrow dataset to just what is needed for core/seasonal check.  Filters for results that have core parameters for aggregated dates  
e.core <- e.spaceaggregatefinal %>%  
  filter(!is.na(aggtimespace)) %>% # Added
  filter(!(CharacteristicName == "ESCHERICHIA COLI" & aggdate <= acuteDate)) %>% 
  filter(!(grepl("Acute", NewUse, fixed = TRUE) & aggdate <= acuteDate)) %>% # filter out acute/ecoli not in last 3 years
  filter(CharacteristicName %in% c("DISSOLVED OXYGEN (DO)", "PH", "CADMIUM", "COPPER", "ZINC", "MERCURY", "ESCHERICHIA COLI", "INORGANIC NITROGEN (NITRATE AND NITRITE)", "FLUORIDE", "ARSENIC", "CHROMIUM", "CHROMIUM(VI)", "LEAD", "BORON", "MANGANESE", "HARDNESS, CA, MG", "TOTAL HARDNESS", "HARDNESS, NON-CARBONATE", "NITRATE", "PHOSPHORUS", "KJELDAHL NITROGEN", "NITROGEN")) %>% 
  select(WBID, CharacteristicName, ResultSampleFractionText, NewUse, aggdate) %>% 
  mutate(Core = 1) %>% 
  distinct(WBID, CharacteristicName, ResultSampleFractionText, NewUse, aggdate, Core)

# Concatenate fraction and characteristic name so spread works more efficently
e.core$fractionandchar <- paste(e.core$ResultSampleFractionText, e.core$CharacteristicName, sep = "zzz") 

e.core <- e.core %>% 
  ungroup() %>% 
  select(-ResultSampleFractionText, -CharacteristicName)

# Spread data and resolve the hardnesses, chromiums, and nitrates.  Also add TKN/nitrate/ite
e.corespread <- spread(e.core, fractionandchar, Core)

# Make NA 0 so can sum columns
e.corespread[is.na(e.corespread)] <- 0

# Calculate the various if then items.  Just looking for presence. 
e.corespread <- e.corespread %>% 
  select(-DissolvedzzzARSENIC, -DissolvedzzzBORON, -DissolvedzzzCHROMIUM, -DissolvedzzzFLUORIDE, -DissolvedzzzPHOSPHORUS, -DissolvedzzzMERCURY, -SuspendedzzzNITROGEN,-TotalzzzCADMIUM, -`DissolvedzzzINORGANIC NITROGEN (NITRATE AND NITRITE)`, -`DissolvedzzzKJELDAHL NITROGEN`, -DissolvedzzzLEAD) %>% 
  mutate(Totalzzznewtn = `TotalzzzKJELDAHL NITROGEN` + `TotalzzzINORGANIC NITROGEN (NITRATE AND NITRITE)` + TotalzzzNITROGEN + DissolvedzzzNITROGEN) %>% 
  # mutate(Totalzzznewchrome = TotalzzzCHROMIUM + `TotalzzzCHROMIUM(VI)`) %>% # Needs better coding...if chrome iv exists then run this line of code
  mutate(Totalzzznewnitrateite = `TotalzzzINORGANIC NITROGEN (NITRATE AND NITRITE)` + TotalzzzNITRATE) %>% 
  mutate(Multizzznewhardness = `DissolvedzzzHARDNESS, CA, MG` + `DissolvedzzzTOTAL HARDNESS` + `DissolvedzzzHARDNESS, NON-CARBONATE` +  `TotalzzzHARDNESS, CA, MG` + `TotalzzzHARDNESS, NON-CARBONATE` + `TotalzzzTOTAL HARDNESS`) # Took out  ''2022 since doesn't exist;  need better way to exclude spread columns

# Gather results back to long format
e.coregather <- gather(e.corespread, fractionandchar, core, -WBID, -NewUse, -aggdate) # core >= 1 means core parameter is present while 0 not present

# Just look at samples where core parameters are present
e.coregather <- filter(e.coregather, core >= 1)

# Separate parameter/fraction
e.coregather <- separate(e.coregather, fractionandchar, into = c("ResultSampleFractionText", "CharacteristicName"), sep = "zzz")

# Narrow to just AW...no chronic/acute
e.coregather <- left_join(e.coregather, XUSECROSSWALK, by = "NewUse")

# Load Core parameters.  These are the minimum parameters needed to make attainment decisions
load(file = "inputs/ZCORE.RData")

# ZCORE shows all needed core parameters.  e.coregather has what was actually sampled.  The join shows the missing (have vs. need).  Core = na there is no core parameter for use.  Y means present.
e.coregather <- e.coregather %>% select(-NewUse) 
e.coregather <- left_join(e.coregather, ZCORE, by = c("CharacteristicName", "ResultSampleFractionText", "Use"))

# Identify WBID with nutrient standards
ZDEQUSESHORT <- YWBHUCREACH %>% 
  select(WBID, NUTRIENT, AWC, AWW, AWE, AWEDW) %>% 
  filter(!is.na(NUTRIENT)) %>% # nutrient standards present
  mutate(TotalzzzPHOSPHORUS = 1) %>% 
  mutate(Totalzzznewtn = 1) %>% 
  select(-NUTRIENT) %>% 
  mutate(Use = "Null")

# Write the use
ZDEQUSESHORT[grep("Y", ZDEQUSESHORT$AWC), "Use"] <- "AWC"
ZDEQUSESHORT[grep("Y", ZDEQUSESHORT$AWW), "Use"] <- "AWW"
ZDEQUSESHORT[grep("Y", ZDEQUSESHORT$AWE), "Use"] <- "AWE"
ZDEQUSESHORT[grep("Y", ZDEQUSESHORT$AWEDW), "Use"] <- "AWEDW"

# Filter out NA's
ZDEQUSESHORT <- ZDEQUSESHORT %>% filter(!is.na(Use)) %>% select(WBID, Use, TotalzzzPHOSPHORUS, Totalzzznewtn)

# Identify nutrient site specific wbid
e.coregather <- e.coregather %>% 
  left_join(ZDEQUSESHORT, by = c("WBID", "Use"))

# Paste characteristic and if core present for grep to work
e.coregather$sitespec <- paste(e.coregather$CharacteristicName, e.coregather$Totalzzznewtn)

# Add a Y to core just the site specific for nitrogen and phosphorus
e.coregather[grep("newtn 1", e.coregather$sitespec), "Core"] <- "Y"
e.coregather[grep("PHOSPHORUS 1", e.coregather$sitespec), "Core"] <- "Y"

# Drop fields 
e.coregather <- select(e.coregather, WBID, aggdate, ResultSampleFractionText, CharacteristicName, core, Use, Core)

# Limits to just core parameter that are for the correct use
e.coregather <- filter(e.coregather, Core == "Y")

# Identify the core parameters we have taking into account aggregation
e.corehave <- e.coregather %>% 
  group_by(WBID, ResultSampleFractionText, CharacteristicName, Use, aggdate = floor_date(aggdate, "quarter"), Core) %>%
  summarize(corepresent = max(core)) 

# Create List of uses by WBID.  Note this is a ODBC connection.
ZDEQUSESCORE <- YWBHUCREACH %>% 
  select(WBID, AWC, AWW, AWE, AWEDW, FC, FBC, PBC, DWS, AGI, AGL) %>% 
  gather(Use, code, -WBID) %>% 
  filter(code == "Y") %>% 
  select(-code)

# Get full list of core parameters needed
e.coreneed <- e.corehave %>% 
  ungroup() %>% 
  select(-ResultSampleFractionText, -CharacteristicName, -Core, -corepresent) %>% 
  full_join(ZDEQUSESCORE, by = c("WBID", "Use"))

# This is every core parameter that SHOULD be present
e.coreneed <- e.coreneed %>% 
  full_join(ZCORE, by = "Use")

# Prepare to add site specific nutrients 
e.coreneed$fractionandchar <- paste(e.coreneed$ResultSampleFractionText, e.coreneed$CharacteristicName, sep = "zzz") 

# Select for just what is needed get ready for spread to ID all core have's
e.coreneed <- e.coreneed %>% 
  ungroup() %>% 
  select(-ResultSampleFractionText, -CharacteristicName, -Core) %>% 
  mutate(corenum = 1) %>% 
  distinct(WBID, Use, aggdate, fractionandchar, corenum)

# Spread data and resolve the hardnesses, chromiums, and nitrates.  Also add TKN/nitrate/ite
e.coreneed <- spread(e.coreneed, fractionandchar, corenum)

# Join nutrient standards
e.coreneed <- left_join(e.coreneed, ZDEQUSESHORT, by = c("WBID", "Use")) 

# Gather it back to long format so comparison can happen
e.coreneed <- gather(e.coreneed, fractionandchar, core, -WBID, -Use, -aggdate)

# Get rid of NA's 
e.coreneed <- e.coreneed %>% 
  filter(!is.na(core)) %>% 
  mutate(COREPARAMETER = "Y") %>% 
  select(-core)

# Separate parameter/fraction - THIS IS THE FINAL LIST FOR CORE PARAMETERS THAT SHOULD BE PRESENT
e.coreneed <- separate(e.coreneed, fractionandchar, into = c("ResultSampleFractionText", "CharacteristicName"), sep = "zzz")
e.coreneed <- arrange(e.coreneed, WBID, aggdate, Use, ResultSampleFractionText, CharacteristicName)

# Compare the core needs with the core haves
e.corecomp <- left_join(e.coreneed, e.corehave, by = c("WBID", "Use", "aggdate", "ResultSampleFractionText", "CharacteristicName"))
e.corecomp <- select(e.corecomp, -Core)

# Create seasons based on months
e.corecomp <- e.corecomp %>% 
  dplyr::mutate(month = lubridate::month(aggdate)) %>% 
  mutate(season = ifelse(month < 4, "spring", 
                         ifelse(month > 3 & month < 7, "summer",
                                ifelse(month > 6 & month < 10, "fall", "winter"))))

# Make NA 0 so can sum columns
e.corecomp$corepresent[is.na(e.corecomp$corepresent)] <- 0

# Determine if core parameters are present for more than 3 seasons
e.corecomp2 <- e.corecomp %>% 
  group_by(WBID, Use, CharacteristicName, ResultSampleFractionText, COREPARAMETER, season) %>% 
  summarise(corecount = sum(corepresent)) %>% 
  mutate(corehave = ifelse(corecount >= 1, 1, 0)) %>% 
  mutate(coreneed = 1)

# Determines what core parameters are present for each season.  Laundry list of what is missing by season. 
e.corecomp3 <- e.corecomp2 %>% 
  group_by(WBID, Use, CharacteristicName, ResultSampleFractionText, COREPARAMETER, season) %>% 
  summarise(sumneed = sum(coreneed, na.rm = TRUE), sumhave = sum(corehave, na.rm = TRUE)) %>% 
  mutate(sumcompare = ifelse(sumneed == sumhave, 1, 0)) # 1 means that season has all core present

# Join this with e.datagap to see which seasons missing
e.season <- e.corecomp3 %>% 
  spread(season, sumhave) 

e.season[is.na(e.season)] <- 0.2

# Determines if at least one sample collected (.2 + .2 + .2 + .2 = .8 so not a keeper)
e.season <- e.season %>% 
  mutate(keepers = spring + summer + fall + winter) %>% 
  filter(keepers >= 1.1)

e.season[grep("1", e.season$fall), "fall"] <- "fall"
e.season[grep("1", e.season$spring), "spring"] <- "spring"
e.season[grep("1", e.season$winter), "winter"] <- "winter"
e.season[grep("1", e.season$summer), "summer"] <- "summer"


e.season[grep("0.2", e.season$fall), "fall"] <- "open"
e.season[grep("0.2", e.season$spring), "spring"] <- "open"
e.season[grep("0.2", e.season$winter), "winter"] <- "open"
e.season[grep("0.2", e.season$summer), "summer"] <- "open"

# Identify seasons
e.season <- e.season %>% 
  ungroup() %>% 
  mutate(donotsampleseasons = paste(spring, summer, fall, winter, sep = ",")) %>% 
  select(WBID, Use, CharacteristicName, ResultSampleFractionText, donotsampleseasons)

# Determines how many seasons have core parameters present.  Need at least 3 for attainment.  List of what is missing by parameter
# cscount allows a count of core needed and core have.  So 15020001-009 has 3 core parameter slots that needed filled for AgI. If only 2 slots filled then core / season not met
# Itemized list of core parameters by waterbody 
e.corecomp4 <- e.corecomp3 %>% 
  group_by(WBID, Use, CharacteristicName, ResultSampleFractionText, COREPARAMETER) %>% 
  summarise(sumneed = sum(sumneed, na.rm = TRUE), sumhave = sum(sumhave, na.rm = TRUE)) %>% 
  mutate(COREANDSEASON = ifelse(sumhave >= 3, "Yes", "No")) %>% 
  mutate(cscount = 1) 

# For summary report that tells samplers exactly what needs to be sampled
e.datagap <- e.corecomp4 %>% 
  filter(COREANDSEASON == "No") %>% 
  mutate(sampleneed = 3 - sumhave) %>% 
  select(WBID, Use, CharacteristicName, ResultSampleFractionText, sampleneed) %>% 
  left_join(e.season, by = c("WBID", "Use", "CharacteristicName", "ResultSampleFractionText"))

e.corecomp5 <- e.corecomp4 %>% 
  group_by(WBID, Use) %>% 
  spread(COREANDSEASON, cscount) %>% 
  mutate(totcount = 1)

# Identify FC use that has no samples, has samples.  Will use later to make use support/Inconclusive and not assessed decisions.
e.fishcore <- e.corecomp5 %>% 
  filter(Use == "FC") %>% 
  mutate(fishcore = ifelse(sumhave > 0, "Sampled", "Not Sampled")) %>% 
  select(WBID, Use, fishcore)

# Roll up to the use level to be used when making use support decisions
e.corecomp6 <- e.corecomp5 %>% 
  ungroup() %>% 
  group_by(WBID, Use) %>% 
  summarise(totyes = sum(Yes), total = sum(totcount)) %>% 
  mutate(Coreandseason = ifelse(totyes >= total, "Y", "N")) %>% 
  left_join(e.fishcore, by = c("WBID", "Use"))

e.corecomp6$Coreandseason[is.na(e.corecomp6$Coreandseason)] <- "N"

# Combine Removed Data

# Align datatypes
r.nowbid$ActivityDepthHeightMeasure.MeasureValue <- as.numeric(r.nowbid$ActivityDepthHeightMeasure.MeasureValue)

r.tribal$WBID <- as.numeric(r.tribal$WBID)
r.tribal$ActivityDepthHeightMeasure.MeasureValue <- as.numeric(r.tribal$ActivityDepthHeightMeasure.MeasureValue)

r.unit$WBID <- as.numeric(r.unit$WBID)
r.unit$ActivityDepthHeightMeasure.MeasureValue <- as.numeric(r.unit$ActivityDepthHeightMeasure.MeasureValue)
r.all <- rbind(r.nodetect, r.duplicated, r.improve, r.nfl, r.notcredible, r.oxygenmgl, r.oxygenper, r.usgsnotcredible, r.nowbid, r.qc, r.tribal, r.unit)




#### F - COMPARE RESULTS TO STANDARDS ####



# ZSTDTYPE groups standards into hardness, regular, ammonia, oxygen, none, etc.  
ZSTDTYPE <- read_csv("inputs/ZSTDTYPE.csv",
                col_types = cols(
                  CharacteristicName = col_character(), 
                  ResultSampleFractionText = col_character(),
                  STDTYPE = col_character()))
                  
# Join stdtype to data
f.stdtypejoin <- full_join(e.spaceaggregatefinal, ZSTDTYPE, by = c("CharacteristicName", "ResultSampleFractionText")) 

# Filters out any acute samples (AWC,W, EDW, E) that are also older than 3 years old
f.stdtypejoin <- filter(f.stdtypejoin, !grepl("Acute", NewUse, fixed = TRUE) | aggdate > acuteDate) #! before grepl basically filps the filter

f.stdtypejoin <- f.stdtypejoin %>% drop_na(WBID)

# This table is the map for DEQ WQDB uses to uses listed in rule/assessments
load(file = "inputs/ZUSECROSSWALK2.RData")

# Put DEQ PROVISIONAL WQDB standards in long format so join can happen
load(file = "inputs/ZDEQSTANDARDS.RData")

# Nitrate/ite don't from standard table to characteristic name fix
ZDEQSTANDARDS[grep("NITROGEN \\(NITRATE AND NITRITE\\), INORGANIC", ZDEQSTANDARDS$SUBSTANCE_NAME), "SUBSTANCE_NAME"] <- "INORGANIC NITROGEN (NITRATE AND NITRITE)" 

# Gather standards
f.gatheredstds <- gather(ZDEQSTANDARDS, STDUSE, STD, -SUBSTANCE_NAME, -SUBSTANCE_CAS_NO, -ResultSampleFractionText, -Unit)
f.gatheredstds <- rename(f.gatheredstds, CharacteristicName = SUBSTANCE_NAME)
f.gatheredstds$STD <- as.numeric(f.gatheredstds$STD)

# Remove duplicates
f.gatheredstds <- distinct(f.gatheredstds, CharacteristicName, SUBSTANCE_CAS_NO, ResultSampleFractionText, STDUSE, STD, Unit)

# Get rid of blanks/na's
f.gatheredstds <- drop_na(f.gatheredstds, STD)

# Standard units for standards
f.gatheredstds <- inner_join(f.gatheredstds, ZSTDUNIT, by = c("Unit" = "ResultMeasure.MeasureUnitCode")) 
f.gatheredstds <- mutate(f.gatheredstds, STDNEW = Conversion * STD)

# 1 - Regular Standards
f1.stdregular <- filter(f.stdtypejoin, STDTYPE == "regular")

# Join for use map.  This aligns the use names in the database which are long to uses used in rule and by EPA 
f1.stdregular <- left_join(f1.stdregular, ZUSECROSSWALK2, by = "NewUse")

# Join for characteristic name map.  This is the step where data that is not matched by standards is removed. Inner join to match data shared by both tables (ie. exclude data without standards)
f1.stdregular <- inner_join(f1.stdregular, f.gatheredstds, by = c("CharacteristicName", "ResultSampleFractionText", "STDUSE")) 

# Identify if Standards Met
f1.stdregular <- f1.stdregular %>% 
  mutate(Exceed = ifelse(aggtimespace > STDNEW, "Yes", "No")) %>% # Confirmed > not >=
  select(-SUBSTANCE_CAS_NO) %>% 
  mutate(SUBSTANCE_CAS_NO = "None")

# Select the same fields to prep for rbind
f1.stdregular <- select(f1.stdregular, 
                        WBID,
                        aggdate, 
                        CharacteristicName,
                        ResultSampleFractionText,
                        NewUse,
                        aggtimespace,
                        STDTYPE,
                        STDUSE,
                        SUBSTANCE_CAS_NO,
                        STD,
                        STDNEW,
                        Exceed)

# Remove rows with NA in Exceed Column (missing either result or detection limit  or both)
f1.stdregular <- filter(f1.stdregular, !is.na(Exceed))

# 2 - Oxygen Standards
f2.stdoxygen <- f.stdtypejoin %>% 
  filter(CharacteristicName == "DISSOLVED OXYGEN SATURATION" | CharacteristicName == "DISSOLVED OXYGEN (DO)") %>% 
  filter(ResultSampleFractionText == "Dissolved")

# Filter for AW use
f2.stdoxygen <- filter(f2.stdoxygen, grepl("AW", NewUse, fixed = TRUE))
f2.stdoxygen <- filter(f2.stdoxygen, grepl("Chronic", NewUse, fixed = TRUE)) # Chronic is arbitrary and picked here so the full 5 years of data is pulled.  Acute pulls 3.
f2.stdoxygen <- filter(f2.stdoxygen, NewUse != "AWEAcute") # Exclude AWE...no standards for ephemerals

# Pair/spread %sat and oxygen concentration 
f2.stdoxygen <- spread(f2.stdoxygen, CharacteristicName, aggtimespace)

# Join for use map
f2.joinstdoxygen <- inner_join(f2.stdoxygen, ZUSECROSSWALK2, by = "NewUse")

# Switch Use back to Acute...that is how it is entered in the standards table
f2.joinstdoxygen[grep("AWC_CHRONIC_MAX", f2.joinstdoxygen$STDUSE), "STDUSE"] <- "AWC_ACUTE_MIN"
f2.joinstdoxygen[grep("AWEDW_CHRONIC_MAX", f2.joinstdoxygen$STDUSE), "STDUSE"] <- "AWEDW_ACUTE_MIN"
f2.joinstdoxygen[grep("AWW_CHRONIC_MAX", f2.joinstdoxygen$STDUSE), "STDUSE"] <- "AWW_ACUTE_MIN"

# Format, Add Standards, Determine if Standards Met
f2.joinstdoxygen <- f2.joinstdoxygen %>% 
  rename(aggtimespace = `DISSOLVED OXYGEN (DO)`) %>%  
  mutate(CharacteristicName = "DISSOLVED OXYGEN (DO)") %>%  
  left_join(f.gatheredstds, by = c("CharacteristicName", "STDUSE")) %>% 
  select(-ResultSampleFractionText.y) %>% 
  rename(ResultSampleFractionText = ResultSampleFractionText.x) %>% 
  rename(STDNEW2 = STD) %>% # do is already in mg/L so no need to create new column
  mutate(Exceed = ifelse(aggtimespace < STDNEW2, "Yes", "No")) %>% 
  drop_na(aggtimespace)

# Remove any rows where % saturation is na but has a yes for exceeds 
f2.joinstdoxygen <- f2.joinstdoxygen %>% 
  filter(Exceed != "Yes" | !is.na(`DISSOLVED OXYGEN SATURATION`))

f2.joinstdoxygen$Exceed[f2.joinstdoxygen$`DISSOLVED OXYGEN SATURATION` >= 90] <- "No"  
f2.joinstdoxygen$STD <- 999999.9
f2.joinstdoxygen$SUBSTANCE_CAS_NO <- "None"

# Select the same fields to prep for bind
f2.joinstdoxygen <- select(f2.joinstdoxygen, 
                           WBID,
                           aggdate, 
                           CharacteristicName,
                           ResultSampleFractionText,
                           NewUse,
                           aggtimespace,
                           STDTYPE,
                           STDUSE,
                           SUBSTANCE_CAS_NO,
                           STD,
                           STDNEW,
                           Exceed)

# 3 - Hardness Dependent Standards
f3.stdhard <- filter(f.stdtypejoin, STDTYPE == "hardness")

# Just need aquatic life
f3.stdhard <- filter(f3.stdhard, grepl("AW", NewUse, fixed = TRUE))

# Concatenate fraction and characteristic name so spread works
f3.stdhard$fractionandchar <- paste(f3.stdhard$ResultSampleFractionText, f3.stdhard$CharacteristicName, sep = "zzz") 

# Get rid of total dissolved and characteristicname.  Ungrouping important here.
f3.stdhard <- f3.stdhard %>%
  ungroup() %>% 
  select(-ResultSampleFractionText, -CharacteristicName) 

# Pair/spread all hardness data 
f3.stdhardgather <- spread(f3.stdhard, fractionandchar, aggtimespace)

# Combine all hardnesses based on priority
f3.stdhardgather$newhardness <- ifelse(!is.na(f3.stdhardgather$`DissolvedzzzHARDNESS, CA, MG`), f3.stdhardgather$`DissolvedzzzHARDNESS, CA, MG`, 
                                       ifelse(!is.na(f3.stdhardgather$`DissolvedzzzTOTAL HARDNESS`), f3.stdhardgather$`DissolvedzzzTOTAL HARDNESS`, 
                                              ifelse(!is.na(f3.stdhardgather$`DissolvedzzzHARDNESS, NON-CARBONATE`), f3.stdhardgather$`DissolvedzzzHARDNESS, NON-CARBONATE`, 
                                                     ifelse(!is.na(f3.stdhardgather$`TotalzzzTOTAL HARDNESS`), f3.stdhardgather$`TotalzzzTOTAL HARDNESS`,
                                                            ifelse(!is.na(f3.stdhardgather$`TotalzzzHARDNESS, CA, MG`), f3.stdhardgather$`TotalzzzHARDNESS, CA, MG`, NA)))))

# Hardness function.  Calculates hardness dependent standards and determines if exceedance
# Arguments are the data, standard name, hardness formula, exceedance name
hardness <- function(data, cdstd, cr3std, custd, pbstd, nistd, agstd, znstd,
                     cdform, cr3form, cuform, pbform, niform, agform, znform,
                     cdexceed, cr3exceed, cuexceed, pbexceed, niexceed, agexceed, znexceed){
  
  # Calculate hardness dependent standard and determine if result exceeded standard
  f3.hard <- data %>%
    mutate(cdstd = cdform) %>%
    mutate(cr3std = cr3form) %>%
    mutate(custd = cuform) %>%
    mutate(pbstd = pbform) %>%
    mutate(nistd = niform) %>%
    mutate(agstd = agform) %>%
    mutate(znstd = znform) %>%
    mutate(cdexceed = ifelse(DissolvedzzzCADMIUM > cdstd, "Yes", "No")) %>%
    mutate(cr3exceed = ifelse(DissolvedzzzCHROMIUM > cr3std, "Yes", "No")) %>%
    mutate(cuexceed = ifelse(DissolvedzzzCOPPER > custd, "Yes", "No")) %>%
    mutate(pbexceed = ifelse(DissolvedzzzLEAD > pbstd, "Yes", "No")) %>%
    mutate(niexceed = ifelse(DissolvedzzzNICKEL > nistd, "Yes", "No")) %>%
    mutate(agexceed = ifelse(DissolvedzzzSILVER > agstd, "Yes", "No")) %>%
    mutate(znexceed = ifelse(DissolvedzzzZINC > znstd, "Yes", "No"))
}

# Hardness formatting function.  Standardizes for use in f.exceed.
hardformat <- function(harddf, stduse, parameter, standard, exceed) {
  f3.hardformat <- harddf %>% 
    select(WBID, aggdate, NewUse, critmet, STDTYPE, parameter, newhardness, standard, exceed) %>% 
    mutate(ResultSampleFractionText = "Dissolved") %>% 
    mutate(SUBSTANCE_CAS_NO = paste("Hardness (mg/L) = ", newhardness)) %>%
    mutate(STDUSE = stduse) %>% 
    rename(aggtimespace = parameter) %>% 
    rename(STD = standard) %>% 
    mutate(STDNEW = STD) %>% 
    mutate(CharacteristicName = sub(pattern = "Dissolvedzzz", replacement = "", parameter)) %>% 
    rename(Exceed = exceed) %>% 
    select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
    drop_na(Exceed)
}

### AWCAcute

# Filter
f3.awcacute <- filter(f3.stdhardgather, NewUse == "AWCAcute")

# Determine standards/exceedances
f3.awcacute <- hardness(data = f3.awcacute, 
                         cdstd = "cdawcacute", cdform = (exp(0.9789*log(f3.awcacute$newhardness)-3.866)*(1.136672-log(f3.awcacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawcacute",
                         cr3std = "cr3awcacute", cr3form = (exp(0.819*log(f3.awcacute$newhardness)+3.7256)*(0.316))/1000, cr3exceed = "Exceedcr3acute",
                         custd = "cuawcacute", cuform = (exp(0.9422*log(f3.awcacute$newhardness)-1.7)*(0.96))/1000, cuexceed = "Exceedcuawcacute",
                         pbstd = "pbawcacute", pbform = (exp(1.273*log(f3.awcacute$newhardness)-1.46)*(1.46203-log(f3.awcacute$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawcacute",
                         nistd = "niawcacute", niform = (exp(0.846*log(f3.awcacute$newhardness)+2.255)*(0.998))/1000, niexceed = "Exceedniawcacute",
                         agstd = "agawcacute", agform = (exp(1.72*log(f3.awcacute$newhardness)-6.59)*(0.85))/1000, agexceed = "Exceedagawcacute",
                         znstd = "znawcacute", znform = (exp(0.8473*log(f3.awcacute$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawcacute")

# Format 
f3.awcacutecd <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awcacutecr3 <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX","DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awcacutecu <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awcacuteag <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzSILVER", "agstd", "agexceed")
f3.awcacuteni <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awcacutepb <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awcacutezn <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine Awcacute
f3.awcacutegather <- rbind(f3.awcacutecd, f3.awcacutecr3, f3.awcacutecu, f3.awcacutepb, f3.awcacuteni, f3.awcacuteag, f3.awcacutezn)

### AWCChronic

# Filter
f3.awcchronic <- filter(f3.stdhardgather, NewUse == "AWCChronic")

# Determine standards/exceedances
f3.awcchronic <- hardness(data = f3.awcchronic, 
                        cdstd = "cdawcchronic", cdform = (exp(0.7977*log(f3.awcchronic$newhardness)-3.909)*(1.101672-log(f3.awcchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawcchronic",
                        cr3std = "cr3awcchronic", cr3form = (exp(0.819*log(f3.awcchronic$newhardness)+0.6848)*(0.86))/1000, cr3exceed = "Exceedcr3chronic",
                        custd = "cuawcchronic", cuform = (exp(0.8545*log(f3.awcchronic$newhardness)-1.702)*(0.96))/1000, cuexceed = "Exceedcuawcchronic",
                        pbstd = "pbawcchronic", pbform = (exp(1.273*log(f3.awcchronic$newhardness)-4.705)*(1.46203-log(f3.awcchronic$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawcchronic",
                        nistd = "niawcchronic", niform = (exp(0.846*log(f3.awcchronic$newhardness)+0.0584)*(0.997))/1000, niexceed = "Exceedniawcchronic",
                        agstd = "agawcchronic", agform = "none", agexceed = "agexceed",
                        znstd = "znawcchronic", znform = (exp(0.8473*log(f3.awcchronic$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawcchronic")

# Format 
f3.awcchroniccd <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awcchroniccr3 <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awcchroniccu <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awcchronicni <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awcchronicpb <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awcchroniczn <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine Awcchronic
f3.awcchronicgather <- rbind(f3.awcchroniccd, f3.awcchroniccr3, f3.awcchroniccu, f3.awcchronicpb, f3.awcchronicni, f3.awcchroniczn)

### AWWacute

# Filter
f3.awwacute <- filter(f3.stdhardgather, NewUse == "AWWAcute")

# Determine standards/exceedances
f3.awwacute <- hardness(data = f3.awwacute, 
                        cdstd = "cdawwacute", cdform = (exp(0.9789*log(f3.awwacute$newhardness)-2.208)*(1.136672-log(f3.awwacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawwacute",
                        cr3std = "cr3awwacute", cr3form = (exp(0.819*log(f3.awwacute$newhardness)+3.7256)*(0.316))/1000, cr3exceed = "Exceedcr3acute",
                        custd = "cuawwacute", cuform = (exp(0.9422*log(f3.awwacute$newhardness)-1.7)*(0.96))/1000, cuexceed = "Exceedcuawwacute",
                        pbstd = "pbawwacute", pbform = (exp(1.273*log(f3.awwacute$newhardness)-1.46)*(1.46203-log(f3.awwacute$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawwacute",
                        nistd = "niawwacute", niform = (exp(0.846*log(f3.awwacute$newhardness)+2.255)*(0.998))/1000, niexceed = "Exceedniawwacute",
                        agstd = "agawwacute", agform = (exp(1.72*log(f3.awwacute$newhardness)-6.59)*(0.85))/1000, agexceed = "Exceedagawwacute",
                        znstd = "znawwacute", znform = (exp(0.8473*log(f3.awwacute$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawwacute")

# Format 
f3.awwacutecd <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awwacutecr3 <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awwacutecu <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awwacuteag <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzSILVER", "agstd", "agexceed")
f3.awwacuteni <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awwacutepb <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awwacutezn <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine awwacute
f3.awwacutegather <- rbind(f3.awwacutecd, f3.awwacutecr3, f3.awwacutecu, f3.awwacutepb, f3.awwacuteni, f3.awwacuteag, f3.awwacutezn)

### AWWChronic

# Filter
f3.awwchronic <- filter(f3.stdhardgather, NewUse == "AWWChronic")

# Determine standards/exceedances
f3.awwchronic <- hardness(data = f3.awwchronic, 
                          cdstd = "cdawwchronic", cdform = (exp(0.7977*log(f3.awwchronic$newhardness)-3.909)*(1.101672-log(f3.awwchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawwchronic",
                          cr3std = "cr3awwchronic", cr3form = (exp(0.819*log(f3.awwchronic$newhardness)+0.6848)*(0.86))/1000, cr3exceed = "Exceedcr3chronic",
                          custd = "cuawwchronic", cuform = (exp(0.8545*log(f3.awwchronic$newhardness)-1.702)*(0.96))/1000, cuexceed = "Exceedcuawwchronic",
                          pbstd = "pbawwchronic", pbform = (exp(1.273*log(f3.awwchronic$newhardness)-4.705)*(1.46203-log(f3.awwchronic$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawwchronic",
                          nistd = "niawwchronic", niform = (exp(0.846*log(f3.awwchronic$newhardness)+0.0584)*(0.997))/1000, niexceed = "Exceedniawwchronic",
                          agstd = "agawwchronic", agform = "none", agexceed = "agexceed",
                          znstd = "znawwchronic", znform = (exp(0.8473*log(f3.awwchronic$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawwchronic")

# Format 
f3.awwchroniccd <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awwchroniccr3 <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awwchroniccu <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awwchronicni <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awwchronicpb <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awwchroniczn <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine awwchronic
f3.awwchronicgather <- rbind(f3.awwchroniccd, f3.awwchroniccr3, f3.awwchroniccu, f3.awwchronicpb, f3.awwchronicni, f3.awwchroniczn)

### AWEDWacute

# Filter
f3.awedwacute <- f3.stdhardgather %>% filter(NewUse == "AWEDWAcute") 

# Determine standards/exceedances
f3.awedwacute <- hardness(data = f3.awedwacute, 
                        cdstd = "cdawedwacute", cdform = (exp(0.9789*log(f3.awedwacute$newhardness)-2.208)*(1.136672-log(f3.awedwacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawedwacute",
                        cr3std = "cr3awedwacute", cr3form = (exp(0.819*log(f3.awedwacute$newhardness)+3.7256)*(0.316))/1000, cr3exceed = "Exceedcr3acute",
                        custd = "cuawedwacute", cuform = (exp(0.9422*log(f3.awedwacute$newhardness)-1.7)*(0.96))/1000, cuexceed = "Exceedcuawedwacute",
                        pbstd = "pbawedwacute", pbform = (exp(1.273*log(f3.awedwacute$newhardness)-1.46)*(1.46203-log(f3.awedwacute$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawedwacute",
                        nistd = "niawedwacute", niform = (exp(0.846*log(f3.awedwacute$newhardness)+2.255)*(0.998))/1000, niexceed = "Exceedniawedwacute",
                        agstd = "agawedwacute", agform = (exp(1.72*log(f3.awedwacute$newhardness)-6.59)*(0.85))/1000, agexceed = "Exceedagawedwacute",
                        znstd = "znawedwacute", znform = (exp(0.8473*log(f3.awedwacute$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawedwacute")

# Format 
f3.awedwacutecd <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awedwacutecr3 <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awedwacutecu <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awedwacuteag <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzSILVER", "agstd", "agexceed")
f3.awedwacuteni <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awedwacutepb <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awedwacutezn <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine awedwacute
f3.awedwacutegather <- rbind(f3.awedwacutecd, f3.awedwacutecr3, f3.awedwacutecu, f3.awedwacutepb, f3.awedwacuteni, f3.awedwacuteag, f3.awedwacutezn)

### AWEDWChronic

# Filter
f3.awedwchronic <- filter(f3.stdhardgather, NewUse == "AWEDWChronic")

# Determine standards/exceedances
f3.awedwchronic <- hardness(data = f3.awedwchronic, 
                          cdstd = "cdawedwchronic", cdform = (exp(0.7977*log(f3.awedwchronic$newhardness)-3.909)*(1.101672-log(f3.awedwchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawedwchronic",
                          cr3std = "cr3awedwchronic", cr3form = (exp(0.819*log(f3.awedwchronic$newhardness)+0.6848)*(0.86))/1000, cr3exceed = "Exceedcr3chronic",
                          custd = "cuawedwchronic", cuform = (exp(0.8545*log(f3.awedwchronic$newhardness)-1.702)*(0.96))/1000, cuexceed = "Exceedcuawedwchronic",
                          pbstd = "pbawedwchronic", pbform = (exp(1.273*log(f3.awedwchronic$newhardness)-4.705)*(1.46203-log(f3.awedwchronic$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawedwchronic",
                          nistd = "niawedwchronic", niform = (exp(0.846*log(f3.awedwchronic$newhardness)+0.0584)*(0.997))/1000, niexceed = "Exceedniawedwchronic",
                          agstd = "agawedwchronic", agform = "none", agexceed = "agexceed",
                          znstd = "znawedwchronic", znform = (exp(0.8473*log(f3.awedwchronic$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawedwchronic")

# Format 
f3.awedwchroniccd <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awedwchroniccr3 <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awedwchroniccu <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awedwchronicni <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awedwchronicpb <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awedwchroniczn <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine awedwchronic
f3.awedwchronicgather <- rbind(f3.awedwchroniccd, f3.awedwchroniccr3, f3.awedwchroniccu, f3.awedwchronicpb, f3.awedwchronicni, f3.awedwchroniczn)

### AWEAcute

# Filter
f3.aweacute <- filter(f3.stdhardgather, NewUse == "AWEAcute")


# Determine standards/exceedances
f3.aweacute <- hardness(data = f3.aweacute, 
                        cdstd = "cdaweacute", cdform = (exp(0.9789*log(f3.aweacute$newhardness)-1.363)*(1.136672-log(f3.aweacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdaweacute",
                        cr3std = "cr3aweacute", cr3form = (exp(0.819*log(f3.aweacute$newhardness)+4.9361)*(0.316))/1000, cr3exceed = "Exceedcr3acute",
                        custd = "cuaweacute", cuform = (exp(0.9422*log(f3.aweacute$newhardness)-1.1514)*(0.96))/1000, cuexceed = "Exceedcuaweacute",
                        pbstd = "pbaweacute", pbform = (exp(1.273*log(f3.aweacute$newhardness)-0.7131)*(1.46203-log(f3.aweacute$newhardness)*0.145712))/1000, pbexceed = "Exceedpbaweacute",
                        nistd = "niaweacute", niform = (exp(0.846*log(f3.aweacute$newhardness)+4.4389)*(0.998))/1000, niexceed = "Exceedniaweacute",
                        agstd = "agaweacute", agform = (exp(1.72*log(f3.aweacute$newhardness)-6.59)*(0.85))/1000, agexceed = "Exceedagaweacute",
                        znstd = "znaweacute", znform = (exp(0.8473*log(f3.aweacute$newhardness)+3.1342)*(0.978))/1000, znexceed = "Exceedznaweacute")

# Format 
f3.aweacutecd <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.aweacutecr3 <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.aweacutecu <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.aweacuteag <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzSILVER", "agstd", "agexceed")
f3.aweacuteni <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.aweacutepb <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.aweacutezn <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine aweacute
f3.aweacutegather <- rbind(f3.aweacutecd, f3.aweacutecr3, f3.aweacutecu, f3.aweacutepb, f3.aweacuteni, f3.aweacuteag, f3.aweacutezn)

# 4 Suspended Sediment Concentration - Note: Uses acute dataset with full 5 years
f4.stdssc <- f.stdtypejoin %>% 
  ungroup() %>% 
  filter(CharacteristicName == "NEWSSC") %>% 
  filter(is.na(ResultSampleFractionText)) %>% 
  select(-CharacteristicName) %>% 
  mutate(CharacteristicName = "SUSPENDED SEDIMENT CONCENTRATION (SSC)")

# Define applicable uses
f4.uses <- c("AWWAcute", "AWCAcute")
f4.stdssc <- filter(f4.stdssc, NewUse %in% f4.uses)

# Calculate median of last 4 samples.  Rep creates a grouping
f4.stdssc <- f4.stdssc %>%
  group_by(WBID) %>%
  mutate(sscmedian = rep(1:55, each = 4, length.out = length(WBID))) #1:55 because assumed there aren't more than 55 aggregated samples for one waterbody

# Allows check to make sure there are at least 4 samples in each group to take a median of 
f4.stdssc <- f4.stdssc %>%
  group_by(WBID, sscmedian) %>%
  mutate(ssccount = 1:n())

# Determiens if a minimum of 4 samples taken
f4.stdssc <- f4.stdssc %>%
  group_by(WBID, sscmedian) %>%
  mutate(ssclast = last(ssccount))

# Exclude data without a minimum of 4 samples
f4.stdssc <- filter(f4.stdssc, ssclast == 4)

# Take median of last 4 samples
f4.stdssc <- f4.stdssc %>%
  group_by(WBID, CharacteristicName, ResultSampleFractionText, NewUse, STDTYPE, sscmedian) %>%
  summarise(aggtimespace = median(aggtimespace), aggdate = min(aggdate))

# Insert STDS
f4.stdssc <- mutate(f4.stdssc, STD = ifelse(NewUse == "AWCAcute", 25, 80))

# Determine if standard met
f4.stdssc <- mutate(f4.stdssc, Exceed = ifelse(aggtimespace > STD, "Yes", "No"))

# Get SSC Ready for rbind
f4.stdssc <- f4.stdssc %>% 
  ungroup() %>% 
  mutate(ResultSampleFractionText = "Suspended")

# Blank out unneeded fields
f4.stdssc <- mutate(f4.stdssc, STDUSE = "none")
f4.stdssc <- mutate(f4.stdssc, SUBSTANCE_CAS_NO = "none")
f4.stdssc$STDNEW <- f4.stdssc$STD

# Select the same fields to prep for rbind
f4.stdssc <- select(f4.stdssc, 
                    WBID,
                    aggdate, 
                    CharacteristicName,
                    ResultSampleFractionText,
                    NewUse,
                    aggtimespace,
                    STDTYPE,
                    STDUSE,
                    SUBSTANCE_CAS_NO,
                    STD,
                    STDNEW,
                    Exceed)

# Ammonia 
f5.ammonia <- filter(f.stdtypejoin, CharacteristicName == "AMMONIA-NITROGEN" | CharacteristicName == "PH" | CharacteristicName == "TEMPERATURE, WATER")

# Add in min and max values for ph and temp
f5.ammonia <- within(f5.ammonia, aggtimespace[CharacteristicName == 'PH' & aggtimespace >= 9] <- 9)
f5.ammonia <- within(f5.ammonia, aggtimespace[CharacteristicName == 'PH' & aggtimespace <= 6.5] <- 6.5)
f5.ammonia <- within(f5.ammonia, aggtimespace[CharacteristicName == 'TEMPERATURE, WATER' & aggtimespace >= 30] <- 30)
f5.ammonia <- within(f5.ammonia, aggtimespace[CharacteristicName == 'TEMPERATURE, WATER' & aggtimespace <= 0] <- 0)

# Just need aquatic life
f5.ammonia <- filter(f5.ammonia, grepl("AW", NewUse, fixed = TRUE))

# Get rid of total dissolved
f5.ammonia <- f5.ammonia %>%
  ungroup() %>% 
  select(-ResultSampleFractionText, -STDTYPE) 

# Make sure no duplicate data
f5.ammonia <- f5.ammonia %>% 
  select(WBID, aggdate, CharacteristicName, NewUse, aggtimespace) %>% 
  distinct(WBID, aggdate, CharacteristicName, NewUse, .keep_all = TRUE) %>% 
  filter(!is.na(aggtimespace))

# Spread so calculations can happen
f5.ammoniaspread <- f5.ammonia %>%
  group_by(WBID, aggdate, NewUse) %>%
  spread(CharacteristicName, aggtimespace)

f5.ammoniaspread <- rename(f5.ammoniaspread, TEMP = `TEMPERATURE, WATER`) #Rename mg/L to join

# Ammonia Chronic

# Add standards for each dependent parameter
f5.ammoniachronic <- drop_na(f5.ammoniaspread, `AMMONIA-NITROGEN`, PH, TEMP)
f5.ammoniachronic <- filter(f5.ammoniachronic, grepl("Chronic", NewUse, fixed = TRUE))

# New AW warm/cold chronic ammonia standard.  Uses uniodids present assumed.
f5.ammoniachronic <- mutate(f5.ammoniachronic, p1 = ((0.0278/(1+10^(7.688-PH)))+(1.1994/(1+(10^(PH-7.688))))))
f5.ammoniachronic <- mutate(f5.ammoniachronic, p2 = (20-max(TEMP,7))) 
f5.ammoniachronic <- mutate(f5.ammoniachronic, p3 = 2.126*(10^(0.028*(p2))))
f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awcchronic = 0.8876*p1*p3)
f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awwchronic = 0.8876*p1*p3)

# New AW EDW Chronic ammonia standard.  Uses the uniodids not present formula.
f5.ammoniachronic <- mutate(f5.ammoniachronic, e1 = ((0.0278/(1+10^(7.688-PH)))+(1.1994/(1+(10^(PH-7.688))))))
f5.ammoniachronic <- mutate(f5.ammoniachronic, e2 = (20-max(TEMP,7))) 
f5.ammoniachronic <- mutate(f5.ammoniachronic, e3 = 7.547*(10^(0.028*(e2))))
f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awedwchronic = 0.9405*e1*e3)

# Determine if chronic standards met
f5.ammoniachronic <- f5.ammoniachronic %>%
  mutate(Exceednh3awcchronic = ifelse(`AMMONIA-NITROGEN` > nh3awcchronic, "Yes", "No")) %>%
  mutate(Exceednh3awwchronic = ifelse(`AMMONIA-NITROGEN` > nh3awwchronic, "Yes", "No")) %>%
  mutate(Exceednh3awedwchronic = ifelse(`AMMONIA-NITROGEN` > nh3awedwchronic, "Yes", "No"))

# ammonia awcchronic
f5.ammoniachronicawc <- f5.ammoniachronic %>%
  filter(NewUse == "AWCChronic") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awcchronic, Exceednh3awcchronic) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWC_CHRONIC_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awcchronic) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceednh3awcchronic) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# ammonia awwchronic
f5.ammoniachronicaww <- f5.ammoniachronic %>%
  filter(NewUse == "AWWChronic") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awwchronic, Exceednh3awwchronic) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWW_CHRONIC_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awwchronic) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceednh3awwchronic) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# ammonia awedwchronic
f5.ammoniachronicawedw <- f5.ammoniachronic %>%
  filter(NewUse == "AWEDWChronic") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awedwchronic, Exceednh3awedwchronic) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWEDW_CHRONIC_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awedwchronic) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceednh3awedwchronic) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# Ammonia Acute
f5.ammoniaacute <- select(f5.ammoniaspread, -TEMP)
f5.ammoniaacute <- drop_na(f5.ammoniaspread, `AMMONIA-NITROGEN`, PH)

# New AW cold ammonia acute standard.  Uniodids present.  Now has temp
f5.ammoniaacute <- mutate(f5.ammoniaacute, p1 = ((0.275/(1+10^(7.204-PH)))+(39.0/(1+(10^(PH-7.204))))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, p2 = 20-TEMP)
f5.ammoniaacute <- mutate(f5.ammoniaacute, p3 = 23.12*(10^(0.036*(p2))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, p4 = (0.7249*((0.0114/(1+10^(7.204-PH)))+(1.6181/(1+(10^(PH-7.204)))))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, p5 = p4*p3)
f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awcacute = ifelse(p1 <= p5, p1, p5))

# New AW warm ammonia acute standard.  Uniodids present.  Now has temp
f5.ammoniaacute <- mutate(f5.ammoniaacute, w1 = (0.7249*((0.0114/(1+10^(7.204-PH)))+(1.6181/(1+(10^(PH-7.204)))))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, w2 = 20-TEMP)
f5.ammoniaacute <- mutate(f5.ammoniaacute, wt = 23.12*(10^(0.036*(w2))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, w3 = ifelse(51.93 <= wt, 51.93, wt))
f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awwacute = w1*w3)

# New AW edw ammonia acute.  Uniodids absent.  Now has temp
f5.ammoniaacute <- mutate(f5.ammoniaacute, e1 = (0.7249*((0.0114/(1+10^(7.204-PH)))+(1.6181/(1+(10^(PH-7.204)))))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, e2 = 20-TEMP)
f5.ammoniaacute <- mutate(f5.ammoniaacute, et = 62.15*(10^(0.036*(e2))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, e3 = ifelse(51.93 <= et, 51.93, et))
f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awedwacute = e1*e3)

# Determine if standards met
f5.ammoniaacute <- f5.ammoniaacute %>%
  mutate(Exceedawcacute = ifelse(`AMMONIA-NITROGEN` > nh3awcacute, "Yes", "No")) %>%
  mutate(Exceedawwacute = ifelse(`AMMONIA-NITROGEN` > nh3awwacute, "Yes", "No")) %>%
  mutate(Exceedawedwacute = ifelse(`AMMONIA-NITROGEN` > nh3awedwacute, "Yes", "No"))

# ammonia awcacute
f5.ammoniaacuteawc <- f5.ammoniaacute %>%
  filter(NewUse == "AWCAcute") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awcacute, Exceedawcacute) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWC_ACUTE_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awcacute) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceedawcacute) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# ammonia awwacute
f5.ammoniaacuteaww <- f5.ammoniaacute %>%
  filter(NewUse == "AWWAcute") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awwacute, Exceedawwacute) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWW_ACUTE_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awwacute) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceedawwacute) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# ammonia awedwacute
f5.ammoniaacuteawedw <- f5.ammoniaacute %>%
  filter(NewUse == "AWEDWAcute") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awedwacute, Exceedawedwacute) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWEDW_ACUTE_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awedwacute) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceedawedwacute) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# 6 Site Specific Nutrients
f6.nutrient <- filter(f.stdtypejoin, CharacteristicName == "INORGANIC NITROGEN (NITRATE AND NITRITE)" | CharacteristicName == "KJELDAHL NITROGEN" | CharacteristicName == "NITROGEN" | CharacteristicName == "PHOSPHORUS")

# Nutrient site specific standards apply to recreational uses and aquatic life uses. Filter out all the extra uses.  AW and Rec have same values so at the end duplicate and change use.
f6.nutrient <- f6.nutrient %>% 
  filter(grepl("AW", NewUse)) %>% 
  filter(grepl("Chronic", NewUse)) # just chronic for full 5 years and to get rid of duplicates.  Not a chronic standard just using for proxy.

# Filter for total
f6.nutrient <- f6.nutrient %>% 
  filter(ResultSampleFractionText == "Total") 

# Determine if Nitrogen is either already calculated (preferred) or needs to be calculated by adding Kjeldahl nitrogen and nitrate/ite

# Get rid of total dissolved and characteristicname.  Ungrouping important here.
f6.nutrient <- f6.nutrient %>%
  ungroup() %>% 
  select(WBID, aggdate, CharacteristicName, aggtimespace, -ResultSampleFractionText, NewUse, -STDTYPE) 

# Pair/spread all nutrient data 
f6.nutrientspread <- spread(f6.nutrient, CharacteristicName, aggtimespace)

# Add the TKN and Nitrate/ite
f6.nutrientspread <- mutate(f6.nutrientspread, TN = `INORGANIC NITROGEN (NITRATE AND NITRITE)` + `KJELDAHL NITROGEN`)

# Combine nitrogen based on priority
f6.nutrientspread$newTN <- ifelse(!is.na(f6.nutrientspread$NITROGEN), f6.nutrientspread$NITROGEN, 
                                  ifelse(!is.na(f6.nutrientspread$TN), f6.nutrientspread$TN, NA))

# Open nutrient standards
load(file = "inputs/ZNUTRIENTSTDS.RData")

# Find out which site specific standard applied to which WBID
f6.nutrientspread <- left_join(f6.nutrientspread, YWBHUCREACH, by = "WBID")

# Just need TN and TP and site specific
f6.nutrientspread <- f6.nutrientspread %>% 
  select(WBID, NewUse, aggdate, PHOSPHORUS, newTN, NUTRIENT) %>% 
  rename(NITROGEN = newTN) %>% 
  left_join(ZUSECROSSWALK2, by = "NewUse")

# Total Nitrogen 

# Filter and select for just nitrogen data to get ready for standards comparision
f6.tn <- f6.nutrientspread %>%
  select(-PHOSPHORUS) %>%
  filter(!is.na(NUTRIENT)) %>%
  filter(!is.na(NITROGEN)) %>%
  rename(aggtimespace = NITROGEN) %>%
  mutate(CharacteristicName = "NITROGEN")

# Join data with standards
f6.tn <- left_join(f6.tn, ZNUTRIENTSTDS, by = c("NUTRIENT", "CharacteristicName"))

# Single Sample Maximum
f6.tnssm <- f6.tn %>%
  select(-annualmean, -`90thpercentile`) %>%
  mutate(Exceed = ifelse(aggtimespace > ssm, "Yes", "No")) %>%
  mutate(ResultSampleFractionText = "Total") %>%
  mutate(STDTYPE = "Nutrient") %>%
  mutate(SUBSTANCE_CAS_NO = "None") %>%
  mutate(STD = 99999.9) %>%
  rename(STDNEW = ssm) %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# Annual Mean

# Step 1 get monthly means
f6.tnannualmean <- f6.tn %>%
  mutate(month = format(aggdate, "%m"), year = format(aggdate, "%Y")) %>%
  group_by(WBID, NUTRIENT, month, year) %>%
  summarise(monthmean = mean(aggtimespace))

# Need at least 2 monthly means
f6.tnannualmean <- f6.tnannualmean %>%
  group_by(WBID, NUTRIENT, month, year) %>%
  summarise(countmonthmean = n(), monthmean = mean(monthmean)) %>% 
  filter(countmonthmean > 1)

# Step 2 calculate annual mean then exclude any year with less or equal to 3
f6.tnannualmean <- f6.tnannualmean %>%
  group_by(WBID, NUTRIENT, year) %>%
  summarise(annmean = mean(monthmean), count = n()) %>%
  filter(count >= 3)

# Format fields
f6.tnannualmean <- f6.tnannualmean %>%
  mutate(CharacteristicName = "NITROGEN") %>%
  left_join(ZNUTRIENTSTDS, by = c("NUTRIENT", "CharacteristicName")) %>%
  mutate(CharacteristicName = "TNANNUALMEAN") %>%
  mutate(Exceed = ifelse(annmean > annualmean, "Yes", "No")) %>%
  mutate(ResultSampleFractionText = "Total") %>%
  mutate(aggdate = as.Date("1900-01-01")) %>%
  mutate(NewUse = "Site Specific Nutrient") %>%
  mutate(STDTYPE = "Nutrient") %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  mutate(STD = 99999.9) %>%
  rename(STDNEW = ssm) %>%
  rename(aggtimespace = annmean) %>%
  mutate(STDUSE = "none") %>%
  ungroup() %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed)) 

# Total Phosphorus 

# Filter and select for just nitrogen data to get ready for standards comparision
f6.tp <- f6.nutrientspread %>%
  select(-NITROGEN) %>%
  filter(!is.na(NUTRIENT)) %>%
  filter(!is.na(PHOSPHORUS)) %>%
  rename(aggtimespace = PHOSPHORUS) %>%
  mutate(CharacteristicName = "PHOSPHORUS")

# Join data with standards
f6.tp <- left_join(f6.tp, ZNUTRIENTSTDS, by = c("NUTRIENT", "CharacteristicName"))

# Single Sample Maximum
f6.tpssm <- f6.tp %>%
  select(-annualmean, -`90thpercentile`) %>%
  mutate(Exceed = ifelse(aggtimespace > ssm, "Yes", "No")) %>%
  mutate(ResultSampleFractionText = "Total") %>%
  mutate(STDTYPE = "Nutrient") %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  mutate(STD = 99999.9) %>%
  rename(STDNEW = ssm) %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# Annual Mean

# Step 1 get monthly means
f6.tpannualmean <- f6.tp %>%
  mutate(month = format(aggdate, "%m"), year = format(aggdate, "%Y")) %>%
  group_by(WBID, NUTRIENT, month, year) %>%
  summarise(monthmean = mean(aggtimespace))

# Need at least 2 monthly means
f6.tpannualmean <- f6.tpannualmean %>%
  group_by(WBID, NUTRIENT, month, year) %>%
  summarise(countmonthmean = n(), monthmean = mean(monthmean)) %>% 
  filter(countmonthmean > 1)

# Step 2 calculate annual mean then exclude any year with less or equal to 3
f6.tpannualmean <- f6.tpannualmean %>%
  group_by(WBID, NUTRIENT, year) %>%
  summarise(annmean = mean(monthmean), count = n()) %>%
  filter(count >= 3)

# Format fields
f6.tpannualmean <- f6.tpannualmean %>%
  mutate(CharacteristicName = "PHOSPHORUS") %>%
  left_join(ZNUTRIENTSTDS, by = c("NUTRIENT", "CharacteristicName")) %>%
  mutate(CharacteristicName = "TPANNUALMEAN") %>%
  mutate(Exceed = ifelse(annmean > annualmean, "Yes", "No")) %>%
  mutate(ResultSampleFractionText = "Total") %>%
  mutate(aggdate = as.Date("1900-01-01")) %>%
  mutate(NewUse = "Site Specific Nutrient") %>%
  mutate(STDTYPE = "Nutrient") %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  mutate(STD = 99999.9) %>%
  rename(STDNEW = ssm) %>%
  rename(aggtimespace = annmean) %>%
  mutate(STDUSE = "none") %>%
  ungroup() %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# 7 PH

# pH is the only parameter that functions using a range so can have both a min exceedance and max exceedance.

# Open PH STDS.  Has hydrogen ion concentrations for comparison.
load(file = "inputs/ZPHSTDS.RData")

# PH Min Exceedances

# Filter for hydrogen ion
# Compare to standards.  Note: Hion is opposite from pH.  So an hion of 0.0001 (ph 4) is below the standard of 0.00001 (pH 5) for DWS
f7.phmin <- f.stdtypejoin %>%
  filter(CharacteristicName == "HIONMIN") %>%
  left_join(ZPHSTDS, by = "NewUse") %>% # Join stds to data
  mutate(Exceedmin = ifelse(HIONMINSTD < aggtimespace, "Yes", "No")) %>% 
  rename(Exceed = Exceedmin) %>% 
  filter(!is.na(Exceed)) %>% 
  mutate(SUBSTANCE_CAS_NO = paste0("Standard between ", phmin, " and ", phmax, "su"))

f7.phmax <- f.stdtypejoin %>%
  filter(CharacteristicName == "HIONMAX") %>%
  left_join(ZPHSTDS, by = "NewUse") %>% # Join stds to data
  mutate(Exceedmin = ifelse(HIONMAXSTD > aggtimespace, "Yes", "No")) %>% 
  rename(Exceed = Exceedmin) %>% 
  filter(!is.na(Exceed)) %>% 
  mutate(SUBSTANCE_CAS_NO = paste0("Standard between ", phmin, " and ", phmax, "su"))

# Combine ph min and max taking the worst case scenario
f7.phall <- bind_rows(f7.phmin, f7.phmax)

# Remove duplicates (the no exceedances are repeated for min and max)
f7.phall <- f7.phall %>% 
  ungroup() %>% 
  select(WBID, aggdate, NewUse, aggtimespace, Exceed, SUBSTANCE_CAS_NO) %>% 
  distinct()

# Standard format
f7.phall <- f7.phall %>% 
  ungroup() %>% 
  mutate(CharacteristicName = "PH") %>% 
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(agg2 = -log10(aggtimespace)) %>% #use log 10 for ph...log give natural log
  select(-aggtimespace) %>% 
  rename(aggtimespace = agg2) %>% 
  mutate(STDTYPE = "none") %>% 
  mutate(STDUSE = "none") %>% 
  mutate(STD = 999999.9) %>% 
  mutate(STDNEW = 999999.9) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) 

# Prioritize exceedances / remove duplicates.  
f7.phall <- f7.phall %>% 
  arrange(WBID, NewUse, aggdate, desc(Exceed)) %>% 
  mutate(concate = paste(WBID, NewUse, aggdate, sep = "-")) %>% # the sort in previous step prioritizes exceedances
  mutate(duplicate = duplicated(concate)) %>% 
  filter(duplicate == "FALSE") %>% 
  select(-duplicate, -concate)

# 8 E coli 

# E coli regular

# E coli regular samples just look at last 3 years of data
f8.ecolireg <- f.stdtypejoin %>%
  filter(CharacteristicName == "ESCHERICHIA COLI" & aggdate > acuteDate) %>%
  filter(NewUse == "FBC" | NewUse == "PBC")

# Bring in the e coli standards
XECOLISTD <- setNames(data.frame(c("FBC", "PBC"), c(410, 576)), c("NewUse","EcoliSTD"))

# Join data to standards
f8.ecolireg <- left_join(f8.ecolireg, XECOLISTD, by = "NewUse")

# Compare to standards
f8.ecolireg <- mutate(f8.ecolireg, Exceedreg = ifelse(aggtimespace > EcoliSTD, "Yes", "No")) 

# Prepare for bind_row
f8.ecolireg <- f8.ecolireg %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  rename(STDNEW = EcoliSTD) %>%
  mutate(STD = 99999.9) %>%
  mutate(STDUSE = "none") %>%
  rename(Exceed = "Exceedreg") %>%
  ungroup() %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# E coli geomean

# Geomean exceedance = at least 4 samples taken in 30 day period. No time/space aggregation so full 5 year window.

# Step one see if any meet criteria
f8.ecoligeo <- filter(d.usejoin, CharacteristicName == "ESCHERICHIA COLI") # no aggregation
f8.ecoligeo <- filter(f8.ecoligeo, Use == "FBC" | Use == "PBC")

# Use median of duplicate.  Basically aggregation by day with time so that duplicate samples don't overweight samples.
f8.ecoligeo <- f8.ecoligeo %>% 
  group_by(WBID, MonitoringLocationIdentifier, ActivityStartTime.Time, aggdate = floor_date(ActivityStartDate, "1 day"), CharacteristicName, ResultSampleFractionText, NewUse) %>%
  summarize(ecoli = median(STDResult))

# Roll up to Waterbody based on Worst Case
f8.ecoligeo <- f8.ecoligeo %>% 
  group_by(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse) %>% 
  summarise(ecoli = max(ecoli))

# Create geomean function...this is from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
geomean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}

# Calculate Geomean
f8.ecoligeo <- f8.ecoligeo %>%
  mutate(month = format(aggdate, "%m"), year = format(aggdate, "%Y")) %>%
  group_by(WBID, NewUse, month, year) %>%
  summarise(monthgeo = geomean(ecoli), count = n()) %>%
  filter(count >= 4) %>%
  mutate(Exceedgeo = ifelse(monthgeo > 126, "Yes", "No")) # Standard is 126 for both PBC and FBC

# Prepare for bind_row
f8.ecoligeo <- f8.ecoligeo %>%
  mutate(CharacteristicName = "ECOLIGEO")   %>% # to distinguish from regular ecoli.  allows combining if impairment determination made  
  mutate(ResultSampleFractionText = "Total") %>%
  rename(aggtimespace = monthgeo) %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  mutate(STDTYPE = "none") %>%
  mutate(STDNEW = 126) %>%
  mutate(STD = 126) %>%
  mutate(STDUSE = "none") %>%
  rename(Exceed = "Exceedgeo") 

# Format
f8.ecoligeo <- f8.ecoligeo %>%
  mutate(aggdate = as.Date("1900-01-01")) %>% 
  ungroup() %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# JOIN ALL EXCEEDANCE RESULTS
f.joinstdall1 <- bind_rows(f1.stdregular, f2.joinstdoxygen, f3.awcacutegather, f3.awcchronicgather, f3.awwacutegather, f3.awwchronicgather, f3.awedwacutegather, f3.awedwchronicgather, f3.aweacutegather)
f.joinstdall2 <- bind_rows(f4.stdssc, f5.ammoniachronicawc, f5.ammoniachronicaww, f5.ammoniachronicawedw, f5.ammoniaacuteawc, f5.ammoniaacuteaww, f5.ammoniaacuteawedw)
f.joinstdall3 <- rbind(f6.tnssm, f6.tnannualmean, f6.tpssm, f6.tpannualmean, f7.phall, f8.ecoligeo, f8.ecolireg)

f.joinstdall <- bind_rows(f.joinstdall1, f.joinstdall2, f.joinstdall3)

f.joinstdall <- left_join(f.joinstdall, XUSECROSSWALK, by = "NewUse")

# Identify all exceedances
f.exceed <- left_join(f.joinstdall, d.dlagg, by = c("WBID", "aggdate", "CharacteristicName", "ResultSampleFractionText", "NewUse"))

# Remove any exceedances that have detection limit issues
f.dlissues <- f.exceed %>% 
  filter(Exceed == "Yes" & countdl > 0) %>% 
  filter(CharacteristicName != "ESCHERICHIA COLI")

# now just exclude values where exceed = Yes and countdl not na.  Just exceedances with no detection limit issues.  
f.exceed <- filter(f.exceed, !Exceed == "Yes" | is.na(countdl))

# Export to dashboard
save(f.exceed, file = "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZEXCEED.Rdata")

# F9 TDS.  TDS standards flow weighted annual mean for the Colorado River
# No need to code unless there are actual exceedances.  As on 5/22/2020 there were none.  Would need to be able to pull in flows from NWIS for full automation. 
# Also need to confirm that TDS standards apply to point locations not reaches.



#### G - SUMMARIZE EXCEEDANCES ####



# Summarize exceedances
g.excsum <- f.exceed %>%
  group_by(WBID, CharacteristicName, ResultSampleFractionText, NewUse, Exceed) %>%
  summarize(samplecount = n())

# Spread so can count samples
g.excsum <- spread(g.excsum, Exceed, samplecount)

# Calculate number of samples (aggregated)
g.excsum <- g.excsum %>%
  rowwise() %>%
  mutate(sampcount = sum(No,Yes, na.rm = TRUE)) %>% #na.rm handles any NA's
  select(WBID, CharacteristicName, ResultSampleFractionText, NewUse, No, Yes, sampcount)



#### H - ASSESS BY DESIGNATED USE PARAMETER AND WATERBDOY ####



# Add column that identifies if binomial or not.  AW and e coli = not.  pH/DO and everything else = binomial.
h.assess <- mutate(g.excsum, binomial = ifelse(NewUse == "AWCAcute" | NewUse == "AWCChronic" | NewUse == "AWWAcute" | NewUse == "AWWChronic" | NewUse == "AWEDWAcute"| NewUse == "AWEDWChronic"| NewUse == "AWEAcute", "No", "Yes"))

# Add in exceptions for DO, pH, e coli, tn, tp
h.assess[grep("DISSOLVED OXYGEN \\(DO)", h.assess$CharacteristicName), "binomial"] <- "Yes" 
h.assess[grep("ESCHERICHIA COLI", h.assess$CharacteristicName), "binomial"] <- "No"
h.assess[grep("ECOLIGEO", h.assess$CharacteristicName), "binomial"] <- "No"
h.assess[grep("TNANNUALMEAN", h.assess$CharacteristicName), "binomial"] <- "No"
h.assess[grep("TPANNUALMEAN", h.assess$CharacteristicName), "binomial"] <- "No"
h.assess[grep("^PH$", h.assess$CharacteristicName), "binomial"] <- "Yes" #Note added ^ and $ for exact match
h.assess[grep("^NITROGEN$", h.assess$CharacteristicName), "binomial"] <- "Yes" #Note added ^ and $ for exact match
h.assess[grep("^PHOSPHORUS$", h.assess$CharacteristicName), "binomial"] <- "Yes" #Note added ^ and $ for exact match

# Assess based on non-binomial
h.assessnotbi <- filter(h.assess, binomial == "No")

# Replace all na's with 0
h.assessnotbi[is.na(h.assessnotbi)] <- 0

# Add non binomial sampling needs
h.assessnotbi <- h.assessnotbi %>% 
  mutate(Impairnumexceed = 2) %>% 
  mutate(Inconclusivenumexceed = 1) %>% 
  mutate(Attainnumexceed = 0)

# Assign attainment/impairment.  Interesting fact...first ifelse overrides the following
h.assessnotbi <- mutate(h.assessnotbi, Assessed = ifelse(Yes > 1, "Not meeting criteria", 
                                                         ifelse(Yes == 1, "Not enough information", 
                                                                ifelse(No > 2, "Meeting criteria", "Not enough information"))))

# If 1 exceedance but 10 or more samples (< 10% exceedance) then change from inconclusive to meeting criteria
h.assessnotbi <- h.assessnotbi %>% 
  mutate(Assessed = ifelse(sampcount >= 10 & Yes == 1, "Meeting criteria", Assessed))

# Replace NA with INCONCLUSIVE
h.assessnotbi$Assessed[is.na(h.assessnotbi$Assessed)] <- "Not enough information"

# Add in number of samples needed to determine if criteria meeting or not at parameter level
h.assessnotbi <- h.assessnotbi %>% 
  mutate(totalsampneed = 3)

# No extra samples needed if impairment/attainment decision already made
h.assessnotbi <- h.assessnotbi %>% mutate(actualsampneed = totalsampneed - sampcount)

h.assessnotbi[grep("Meeting criteria|Not meeting criteria", h.assessnotbi$Assessed), "actualsampneed"] <- 0
h.assessnotbi[grep(1, h.assessnotbi$Yes), "actualsampneed"] <- 1 # 1 exceedance means need one more exceedance before inconclusive = impairment

# Assess based on binomial
h.assessbi <- filter(h.assess, binomial == "Yes")

# Opens binomial table
load(file = "inputs/ZBINOMIAL.RData")
# Note could do the table in code for impairment using the following base r code.  Would have to exclude the first 19 and then deal with inconclusives and attaining.
# NumberExceed <- 5
# TotalSamples <- 20
# Probability <- .1 # This is for the 90% confidence of a 10% exceedance rate
# pbinom(q = NumberExceed - 1, size = TotalSamples, prob = Probability)

h.assessbi <- inner_join(h.assessbi, ZBINOMIAL, by = c("sampcount" = "NumSamp"))

h.assessbi$Yes <- as.numeric(h.assessbi$Yes)

# Replace na's for number columns.  
h.assessbi$Yes[is.na(h.assessbi$Yes)] <- 0
h.assessbi$No[is.na(h.assessbi$No)] <- 0
h.assessbi$sampcount[is.na(h.assessbi$sampcount)] <- 0

# Use ATTAINS mapping words
h.assessbi <- mutate(h.assessbi, Assessed = ifelse(Yes >= Impairnumexceed, "Not meeting criteria", 
                                                   ifelse(Yes == Inconclusivenumexceed, "Not enough information", 
                                                          ifelse(Yes <= Attainnumexceed, "Meeting criteria", "Not enough information"))))

# 3 samples and no exceedances is the minimum requirement to determine if meeting criteria by parameter.  Core/season at use level determined later.
h.assessbi <- mutate(h.assessbi, attain = ifelse(sampcount >= 3 & Yes == 0, "Meeting criteria", "not applicable"))

h.assessbi[grep("Meeting criteria", h.assessbi$attain), "Assessed"] <- "Meeting criteria"

# Replace NA with INCONCLUSIVe
h.assessbi$Assessed[is.na(h.assessbi$Assessed)] <- "Not enough information"

# Identify how many samples are needed.  The minimum is 10 samples with exceptions below.
h.assessbi <- h.assessbi %>% 
  mutate(totalsampneed = ifelse(sampcount <= 10 & Yes == 0, 3,
                                ifelse(sampcount > 10, Impairnumexceed, 10)))

# Exclude where impairment/attainment decision already made
h.assessbi <- h.assessbi %>% mutate(actualsampneed = totalsampneed - sampcount)

# Exclude where impairment/attainment decision already made
h.assessbi[grep("Meeting criteria|Not meeting criteria", h.assessbi$Assessed), "actualsampneed"] <- 0

# If exceedances but less than 20 samples then calculate the number of actual samples needed
h.assessbi <- h.assessbi %>% 
  mutate(actualsampneed = ifelse(totalsampneed > 990000, 20 - sampcount, actualsampneed))

# Select columns
h.assessbi <- select(h.assessbi, WBID, CharacteristicName, ResultSampleFractionText, NewUse, No, Yes, sampcount, binomial, Assessed, actualsampneed)

# Combine binomial and not binomial
h.assessall <- bind_rows(h.assessnotbi, h.assessbi)

# # Aggregate acute/chronic to worst case
h.assessall <- left_join(h.assessall, XUSECROSSWALK, by = "NewUse")

# Drop sample counts and just have impairments.  Convert impaired, attaining, inconclusive and not assessed into 3, 2, 1, 0 so max summary works.
h.assessall <- h.assessall %>% mutate(newassess = 0) # 0 is not assessed, which is the default

# if new impairment add a 3 to new assessed
h.assessall[grep("Not meeting criteria", h.assessall$Assessed), "newassess"] <- 3
h.assessall[grep("Meeting criteria", h.assessall$Assessed), "newassess"] <- 2
h.assessall[grep("Not enough information", h.assessall$Assessed), "newassess"] <- 1

# Bring in historical impairments using ATTAINS parameters.  Note fields added to crosswalk WBID, characteristic names and uses.
ATTAINSPARAMETERS <- read_csv("inputs/ATTAINS2020PARAMETERS.csv", 
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

# Load parameter mapping
load(file = "inputs/ZATTAINSPARAMETERMAP.RData")

# Crosswalk use names to ATTAINS
XATTAINSUSE <- setNames(data.frame(
  c("Agricultural Irrigation", "Full Body Contact", "Agricultural Livestock Watering", "Aquatic and Wildlife (Warmwater Fishery)", "Fish Consumption", "Partial Body Contact", "Aquatic and Wildlife (Ephemeral)", "Aquatic and Wildlife (Effluent Dependent Water)", "Aquatic and Wildlife (Coldwater Fishery)", "Domestic Water Source"), 
  c("AGI", "FBC", "AGL", "AWW", "FC", "PBC", "AWE", "AWEDW", "AWC", "DWS")), 
  c("PARAM_USE_NAME","Use"))

# Most recent ATTAINS parameter file. Add WBID, Characteristic name mapping and use mapping
ATTAINSPARAMETERS <- ATTAINSPARAMETERS %>% 
  mutate(ASSESSMENT_UNIT_IDCOPY = ASSESSMENT_UNIT_ID) %>% 
  separate(ASSESSMENT_UNIT_IDCOPY, c("a", "b"), sep = "_") %>% 
  separate(a, c("c", "d", "e"), sep = "([\\Z\\L])") %>% 
  mutate(WBID = ifelse(is.na(e), d, e)) %>% 
  select(ASSESSMENT_UNIT_ID, WBID, PARAM_NAME, PARAM_USE_NAME, PARAM_STATUS_NAME, PARAM_ATTAINMENT_CODE) %>% 
  filter(PARAM_STATUS_NAME != "Removed") %>% 
  left_join(XATTAINSUSE, by = c("PARAM_USE_NAME")) %>% 
  left_join(ZATTAINSPARAMETERMAP, by = "PARAM_NAME")

# Need to reduce h.assessall for fraction and chronic/acute so direct comparision to ATTAINs can be made. 
h.assessall.attains <- h.assessall %>% 
  group_by(WBID, CharacteristicName, Use) %>%
  summarise(newassess = max(newassess)) %>% 
  rename(newassessattains = newassess)

# Join attains level info to h.assessall so no loss of info then Full join of ATTAINS to h.assessall
h.assessall2 <- h.assessall %>% 
  left_join(h.assessall.attains, by = c("WBID", "CharacteristicName", "Use")) %>% 
  full_join(ATTAINSPARAMETERS, c("WBID", "CharacteristicName", "Use")) %>% 
  mutate(existimpair = ifelse(PARAM_ATTAINMENT_CODE == "Not meeting criteria", "Existing Impairment", "No")) %>% 
  mutate(existimpair = replace(existimpair, is.na(existimpair), "No"))  

# Open carry forward map.  This resolves the problem due to ATTAINS not tracking fraction and acute/chronic which leaves these values null for carry forward decisions
load(file = "inputs/ZCARRYFORWARDMAP.RData")

# If newuse or fraction na then impute defaults
# May want to put a hard stop here if na present.  Map only addresses known missing.  New ones will need to be added or just do complete mapping.
h.assessall2 <- h.assessall2 %>% 
  left_join(ZCARRYFORWARDMAP, by = c("CharacteristicName", "Use")) %>% 
  mutate(NewUse = ifelse(is.na(NewUse), cfNewUse, NewUse)) %>% 
  mutate(ResultSampleFractionText = ifelse(is.na(ResultSampleFractionText), cfResultSampleFraction, ResultSampleFractionText)) %>% 
  select(-cfResultSampleFraction, -cfNewUse)

# See what is different between automated assessment and ATTAINS so logic easier to apply
h.assessall2 <- h.assessall2 %>% 
  mutate(newassessattains = replace(newassessattains, newassessattains == 2, "Meeting criteria")) %>% 
  mutate(newassessattains = replace(newassessattains, newassessattains == 3, "Not meeting criteria")) %>% 
  mutate(newassessattains = replace(newassessattains, newassessattains == 1, "Not enough information")) %>% 
  mutate(newassessattains = replace(newassessattains, is.na(newassessattains), "Not assessed")) %>% 
  mutate(PARAM_ATTAINMENT_CODE = replace(PARAM_ATTAINMENT_CODE, is.na(PARAM_ATTAINMENT_CODE), "Not assessed")) %>% 
  mutate(DifferentSame = ifelse(newassessattains == PARAM_ATTAINMENT_CODE, "Same", "Different"))

## Add Logic
# PARAM_ATTAINMENT_CODE  DEQParamDec            count
# <chr>                  <chr>                  <int>
# 1 Meeting criteria       Not enough information   426 = depends #1 (Need to know binomial/exceedances)
# 2 Not enough information Meeting criteria         774 = DEQ
# 3 Not enough information not applicable            14 = ATTAINS
# 4 Not enough information Not meeting criteria       5 = ATTAINS - override in provassess if applicable
# 5 Not meeting criteria   Meeting criteria           1 = DEQ
# 6 Not meeting criteria   not applicable             2 = ATTAINS

## Depends #1 The 426 meeting criteria attains but inconclusive deq will be split by binomial/exceedances.   
# non binomial all if any exceedance then DEQ, else attains.
# if binomial then # samples and # exceedances considered...should be < 10 samples then attains...> 10 = deq 
h.assessall2 <- h.assessall2 %>% 
  mutate(combinedassessed = ifelse(newassessattains == "Meeting criteria" & PARAM_ATTAINMENT_CODE == "Not enough information", newassessattains, 
                                   ifelse(newassessattains == "Meeting criteria" & PARAM_ATTAINMENT_CODE == "Not meeting criteria", newassessattains, 
                                          ifelse(newassessattains == "Not enough information" & PARAM_ATTAINMENT_CODE == "Meeting criteria" & binomial == "No" & Yes > 0, newassessattains,  
                                                 ifelse(newassessattains == "Not enough information" & PARAM_ATTAINMENT_CODE == "Meeting criteria"& binomial == "No" & sampcount >= 10 & Yes >= 3, newassessattains, 
                                                        ifelse(newassessattains == "Not assessed", PARAM_ATTAINMENT_CODE, 
                                                               ifelse(newassessattains == "Not meeting criteria", newassessattains,
                                                                      ifelse(PARAM_ATTAINMENT_CODE == "Not assessed", newassessattains, PARAM_ATTAINMENT_CODE))))))))

# Identify which parameter decisions for meeting criteria were carried forward
h.assessall2 <- h.assessall2 %>% 
  mutate(paramcarryforward = ifelse(combinedassessed == newassessattains, "Current", "Carry Forward - Parameter")) 

# Connection made now clean up file and put back into the pipeline
h.assessall3 <- h.assessall2 %>% 
  select(-newassess) %>% 
  rename(newassess = combinedassessed) %>% 
  select(WBID, CharacteristicName, ResultSampleFractionText, NewUse, No, Yes, sampcount, binomial,
         Impairnumexceed, Inconclusivenumexceed, Attainnumexceed, Assessed, totalsampneed, actualsampneed,
         Use, existimpair, paramcarryforward, newassess)

# Back to the original pipeline with same formatting
h.assessall <- h.assessall3

# Add in any reaches you split.  This is to account for an impairment that isn't picked up by ATTAINS because the old reach code is retired.
# Currently none...reactivate if splits occur
# SPLITPARAMETERS <- read_csv("inputs/SPLITPARAMETERS.csv")

# combine both
# ATTAINSPARAMETERS <- rbind(ATTAINSPARAMETERS, SPLITPARAMETERS)

# Keeps track of 'offical impairments' so can track automatic vs. provisional vs. official.  Existing shouldn't change except for EPA approval.
h.assessall <- h.assessall %>% 
  mutate(existimp = ifelse(existimpair == "Existing Impairment", 3, 0)) %>% 
  mutate(newassess = replace(newassess, newassess == "Meeting criteria", 2)) %>% 
  mutate(newassess = replace(newassess, newassess == "Not meeting criteria", 3)) %>% 
  mutate(newassess = replace(newassess, newassess == "Not enough information", 1)) %>% 
  mutate(newassess = replace(newassess, newassess == "Not assessed", 0)) 

h.assessall$newassess <- as.numeric(h.assessall$newassess)

# if existing impairment add 3 to new assessed
h.assessall[grep("Existing Impairment", h.assessall$existimpair), "newassess"] <- 3

# Identify New Impairments - Same field as existing.  
h.assessallaa <- filter(h.assessall, Assessed == "Not meeting criteria") 
h.assessallaa[grep("No", h.assessallaa$existimpair), "existimpair"] <- "New Impairment"

h.assessallbb <- filter(h.assessall, Assessed != "Not meeting criteria") 
h.assessallcc <- filter(h.assessall, is.na(Assessed)) 
h.assessall <- bind_rows(h.assessallaa, h.assessallbb, h.assessallcc)

# Grab the WBID Name and join here
YWATERBODYNAME <- YWBHUCREACH %>% select(WBID, WATERBODY_NAME)

# Enter Fish Advisories (Impairments)  Eventually make this an ODBC query that tells fish folk that data says there is an impairment
h.assessall <- h.assessall %>% 
  ungroup() %>% 
  select(WBID, NewUse, Use, CharacteristicName, ResultSampleFractionText, Assessed, binomial, No, Yes, sampcount, actualsampneed, existimpair, paramcarryforward, newassess)

# Load New Fish Impairments/Advisories
ZFISH <- read_csv("inputs/ZFISH.csv", col_types = cols(WBID = col_character(), 
                                                       NewUse = col_character(), 
                                                       Use = col_character(), 
                                                       CharacteristicName = col_character(), 
                                                       ResultSampleFractionText = col_character(), 
                                                       existimpair = col_character(), 
                                                       paramcarryforward = col_character(), 
                                                       newassess = col_number()))

# Bind ZFISH to h.assessall
h.assessall <- bind_rows(h.assessall, ZFISH)

# Adds WBID Name and Selects for columns needed for Human File
h.assessall <- h.assessall %>% 
  ungroup() %>% 
  left_join(YWATERBODYNAME, by = "WBID") %>% 
  select(WBID, WATERBODY_NAME, NewUse, Use, CharacteristicName, ResultSampleFractionText, Assessed, binomial, No, Yes, sampcount, actualsampneed, existimpair, paramcarryforward, newassess)

# Look for existing impairments that are meeting criteria and correct data.  These need more than 3 to delist and meet criteria
h.assessall <- h.assessall %>% 
  mutate(change = ifelse(binomial == "Yes" & Assessed == "Meeting criteria" & sampcount < 10 & existimpair == "Existing Impairment", "change", "ok")) %>% 
  mutate(actualsampneed2 = ifelse(change == "change", 10 - sampcount, 9999)) %>% 
  mutate(actualsampneed3 = ifelse(actualsampneed2 != 9999, actualsampneed2, actualsampneed)) %>% 
  mutate(Assessed = ifelse(change == "change", "Not enough information", Assessed)) %>% 
  select(-actualsampneed, -actualsampneed2, -change) %>% 
  rename(actualsampneed = actualsampneed3)

# Remove any duplicates.  The acute/chronic will be resolved at this point.  Rolled up values take the chronic first.
# Note all automated and primary key fields present for later join to provisional (human) fields
h.assessall <- h.assessall[!duplicated(h.assessall[,c("WBID", "NewUse", "Use", "CharacteristicName", "ResultSampleFractionText")]),]

# Open Critical Condition
ZCRITICALCONDITION <- read_csv("inputs/ZCRITICALCONDITION.csv", col_types = cols(
  WBID = col_character(),
  WATERBODY_NAME = col_character(),
  criticalcondition = col_character(), 
  criticallocation = col_character())) 

# Identify changes since last run. 8/10/20 update makes this a separate QA query.
# Add field that tells specialist if something changed since the last run (REEVALUATE).
load(file = "inputs/ZLASTDB.RData")

# Identifies what is different in the automated data from last run
h.diff <- h.assessall %>% mutate(source = "new")

# Identify what was different since last run.
h.diff$concate <- paste(h.diff$WBID, h.diff$Use, h.diff$CharacteristicName, h.diff$Assessed, h.diff$No, h.diff$Yes, h.diff$sampcount)
h.lastdb <- ZLASTDB %>% mutate(source = "old")
h.lastdb$concate <- paste(h.lastdb$WBID, h.lastdb$Use, h.lastdb$CharacteristicName, h.lastdb$Assessed, h.lastdb$No, h.lastdb$Yes, h.lastdb$sampcount)
h.reevaluate <- bind_rows(h.lastdb, h.diff)
h.reevaluate <- h.reevaluate %>%
  mutate(dbdifferent = duplicated(concate)) %>% # will say true which is opposite of meaning but this will be fixed later
  filter(source == "new") %>%
  select(WBID, WATERBODY_NAME, NewUse, Use, CharacteristicName, ResultSampleFractionText, Assessed, binomial, No, Yes, sampcount, actualsampneed, existimpair, paramcarryforward, dbdifferent,newassess) %>% 
  filter(dbdifferent == "FALSE") # False means need to reevaluate
view(h.reevaluate)

# Opens human file listing data types explictly.  Dates in excel must be in %d/%m/%Y format.
human <- read_csv("human.csv", col_types = cols(Assessed = col_character(), 
                                                CharacteristicName = col_character(), 
                                                NewUse = col_character(), No = col_double(), 
                                                ResultSampleFractionText = col_character(), 
                                                Use = col_character(), WATERBODY_NAME = col_character(), 
                                                WBID = col_character(), Yes = col_double(), 
                                                actualsampneed = col_double(), binomial = col_character(), 
                                                newassess = col_double(), paramcarryforward = col_character(), 
                                                provassess = col_double(), provcomment = col_character(), 
                                                provdate = col_date(format = "%m/%d/%Y"), 
                                                provdatetext = col_character(), sampcount = col_double()))

# Make a backup
file.copy("human.csv", paste("humancopies/humancopy", format(Sys.Date(), "%Y-%m-%d"), "csv", sep = "."))

# Select fields.  Basically just the key fields and the provisional fields.  Remove automated fields.
human <- human %>% select(WBID, NewUse, Use, CharacteristicName, ResultSampleFractionText, provassess, provdate, provcomment, provdatetext)

# Remove any duplicates.  The acute/chronic will be resolved at this point.  Rolled up values take the chronic first.
human <- human[!duplicated(human[,c("WBID", "NewUse", "Use", "CharacteristicName", "ResultSampleFractionText")]),]

# Need to fill in the missing uses for human just like automated so they match up during join
human <- human %>% 
  left_join(ZCARRYFORWARDMAP, by = c("CharacteristicName", "Use")) %>% 
  mutate(NewUse = ifelse(is.na(NewUse), cfNewUse, NewUse)) %>% 
  mutate(ResultSampleFractionText = ifelse(is.na(ResultSampleFractionText), cfResultSampleFraction, ResultSampleFractionText)) %>% 
  select(-cfResultSampleFraction, -cfNewUse)

# Full join human data to automatically calculated data.
human <- left_join(h.assessall, human, by = c("WBID", "NewUse", "Use", "CharacteristicName", "ResultSampleFractionText"))

# Replace 0 for NA in provisional Assessment field
human$provassess[is.na(human$provassess)] <- 0

# Human cleaner.  Gets rid of the NA results
human <- filter(human, !is.na(WATERBODY_NAME) | !is.na(existimpair))

# Deploy this if dates overwritten
# human <- human %>%
#   mutate(provdatetext = as.numeric(provdatetext)) %>%
#   mutate(provdate = as.Date(provdatetext, origin = "1899-12-30"))

# Write the new human file with the new automated data and database check
write.csv(human, "human.csv", row.names = FALSE)

# Write the last database run.  IMPORTANT.  CHANGES TO DATABASE ONLY CAPTURED ONCE SO SPECIALIST SHOULDN'T RUN THIS PART OF THE CODE UNLESS THEY HAVE TIME TO REEVALUATE
ZLASTDB <- h.assessall

save(ZLASTDB, file = "inputs/ZLASTDB.RData")



#### I - ASSESS BY USE AND WATERBODY or PARAMETER AND WATERBODY ####



# Create ATTAINS Assessment Unit Files.  location.csv, water_type.csv and assessment_units.csv

# Identify Retired Reaches
i.retired <- c("15070102-334", "15020001-011", "15030204-003", "15050100-12B", "15050100-12A")

# Remove from human File (this is not save to the permanent file but won't be saved to EPA ATTAINS files)
human <- human %>% 
  filter(!(WBID %in% i.retired)) %>% 
  filter(CharacteristicName != "ECOLIGEO")

# Load what is different from ATTAINS vs. Human file
# This file only needs generated when loading data to ATTAINs once every 2 years.
# You will need to pull assessment units from ATTAINS and then compare (anti_join) with distinct human WBIDs
ZAUNOTINATTAINS <- read_csv("inputs/ZAUNOTINATTAINS.csv", col_types = cols(WBID = col_character()))

# Everything needed to pull all the various locational pieces.
ATTAINSAUALL <- ZAUNOTINATTAINS %>% 
  distinct(WBID) %>% 
  left_join(YWBHUCREACH, by = "WBID") %>% 
  mutate(WATER_TYPE = ifelse(is.na(REACH_DISTANCE), "LAKE, FRESHWATER", "STREAM")) %>% 
  mutate(lors = ifelse(is.na(REACH_DISTANCE), "L", "")) %>% # this is for the ASSESSMENT_UNIT_ID
  mutate(WATER_SIZE = ifelse(is.na(REACH_DISTANCE), LAKE_ACRES, REACH_DISTANCE)) %>% 
  mutate(WATER_UNIT = ifelse(is.na(REACH_DISTANCE), "Acres", "Miles")) %>% 
  mutate(ASSESSMENT_UNIT_ID = paste0("AZ", lors, WBID, "_00")) %>% 
  mutate(HUC2 = as.character(HUC))

# water_type.csv
ATTAINSWATER_TYPE <- ATTAINSAUALL %>% 
  select(ASSESSMENT_UNIT_ID, WATER_TYPE, WATER_SIZE, WATER_UNIT)

# location.csv
ATTAINSLOCATION <- ATTAINSAUALL %>% 
  mutate(LOCATION_TYPE_CODE = "HUC-8") %>% 
  mutate(LOCATION_TYPE_CONTEXT = "21ARIZ") %>% 
  mutate(LOCATION_TEXT = HUC) %>% 
  select(ASSESSMENT_UNIT_ID, LOCATION_TYPE_CODE, LOCATION_TYPE_CONTEXT, LOCATION_TEXT)

# assessment_units.csv
ATTAINSASSESSMENT_UNITS <- ATTAINSAUALL %>% 
  mutate(ASSESSMENT_UNIT_STATE = "AZ") %>% 
  mutate(ASSESSMENT_UNIT_AGENCY = "S") %>% 
  mutate(ASSESSMENT_UNIT_COMMENT = "") %>% 
  mutate(LOCATION_DESCRIPTION = paste0("HUC: ", HUC)) %>% 
  mutate(USE_CLASS_NAME = "") %>% 
  mutate(ASSESSMENT_UNIT_NAME = WATERBODY_NAME) %>% 
  select(ASSESSMENT_UNIT_ID, ASSESSMENT_UNIT_NAME, ASSESSMENT_UNIT_STATE, ASSESSMENT_UNIT_AGENCY, ASSESSMENT_UNIT_COMMENT, LOCATION_DESCRIPTION, USE_CLASS_NAME)

# Output csv's.  Note.  Code will likely need adjusted as EPA changes domain names and other requirements.
# See what validation errors you get and adjust code accordingly.
write.csv(ATTAINSWATER_TYPE, "ATTAINS/water_type.csv", row.names = FALSE)
write.csv(ATTAINSLOCATION, "ATTAINS/location.csv", row.names = FALSE)
write.csv(ATTAINSASSESSMENT_UNITS, "ATTAINS/assessment_units.csv", row.names = FALSE)

# User needs to update any new TMDLs.  Note using 2018 because there were no new TMDLS in 2020.
# So far none for 2022.
ATTAINSACTIONS <- read_csv("inputs/ATTAINS2018ACTIONS.csv",
                           col_types = cols(ASSESSMENT_UNIT_ID = col_character(),
                                            ACTION_PARAM_NAME = col_character(),
                                            CharacteristicName = col_character(),
                                            ACTION_ID = col_character(),
                                            ACTION_TYPE = col_character(),
                                            WBID = col_character()))

# ATTAINS Assessments.  Similar to the assessment units but bundled with parameters, uses, associated-actions and sources.
# Note: The source of this data is the MATS Table of the same name.  
# To get last year assessed this table is filled manually with years (note missing 2020 draft).
# Automation of MATS into this table would help.
load(file = "inputs/ZASSESSMENTHIST.RData")

# Get Trophic Status.  Static table from ATTAINS for now.  If updates then change format.
load(file = "inputs/ZTROPHIC.RData")

# Map EPA assessment unit to WBID
ZTROPHIC <- ZTROPHIC %>% 
  mutate(TEMPWBID = ASSESSMENT_UNIT_ID) %>% 
  separate(TEMPWBID, c("a", "b"), sep = "_") %>% 
  separate(a, c("c", "d", "e"), sep = "([\\Z\\L])") %>% 
  rename(WBID = e) %>% 
  filter(!is.na(TROPHIC_STATUS)) %>% 
  select(WBID, TROPHIC_STATUS)

# na to ""
ZASSESSMENTHIST <- ZASSESSMENTHIST %>% 
  mutate(TEMPWBID = WBID) %>% 
  separate(TEMPWBID, c("c", "d", "e"), sep = "([\\Z\\L])")

ZASSESSMENTHIST$e[is.na(ZASSESSMENTHIST$e)] <- ""
ZASSESSMENTHIST$d[is.na(ZASSESSMENTHIST$d)] <- ""

ZASSESSMENTHIST <- ZASSESSMENTHIST %>% 
  mutate(TEMPWBID2 = paste0(d,e)) %>% 
  rename(XXWBID = WBID) %>% 
  rename(WBID = TEMPWBID2) %>% 
  select(WBID, AY_1998, AY_2000, AY_2002, AY_2004, AY_2006_08, AY_2010, AY_2012_14, AY_2016, AY_2018, d, e)

ZASSESSMENTHIST[is.na(ZASSESSMENTHIST)] <-0

# What assessment units assessed?  Link in Trophic status (YWBHUCREACH) and last year assessed (MATS).  
i.assessments <- human %>% 
  distinct(WBID) %>% 
  left_join(ZTROPHIC, by = "WBID") %>% 
  left_join(ZASSESSMENTHIST, by = "WBID") %>% 
  mutate(CYCLE_LAST_ASSESSED = ifelse(is.na(AY_2018), "2020", pmax(AY_1998, AY_2000, AY_2002, AY_2004, AY_2006_08, AY_2010, AY_2012_14, AY_2016, AY_2018))) %>% # max applies all argument values. pmax just one row. 
  left_join(YWBHUCREACH, by = "WBID") %>% 
  mutate(lors = ifelse(is.na(REACH_DISTANCE), "L", "")) %>% # this is for the ASSESSMENT_UNIT_ID
  mutate(ASSESSMENT_UNIT_ID = paste0("AZ", lors, WBID, "_00")) %>% 
  mutate(AGENCY_CODE = "S") %>% 
  mutate(YEAR_LAST_MONITORED = "") %>% 
  mutate(STATE_IR_CAT_CODE = "") %>% 
  mutate(ASSESSMENT_COMMENT = "") %>% 
  mutate(ASSESSMENT_RATIONALE = "") %>% 
  select(ASSESSMENT_UNIT_ID, AGENCY_CODE, CYCLE_LAST_ASSESSED, YEAR_LAST_MONITORED, STATE_IR_CAT_CODE, ASSESSMENT_COMMENT, ASSESSMENT_RATIONALE, TROPHIC_STATUS)

# Clean up NAs
i.assessments$TROPHIC_STATUS[is.na(i.assessments$TROPHIC_STATUS)] <- ""

write.csv(i.assessments, "ATTAINS/General.csv", row.names = FALSE)

# Assess by Parameter.  JUST NEEDED FOR PERFORMANCE METRIC.  No need for sample fraction since  unique (can't have total and disolved in same use).
i.param <- human %>% 
  group_by(WBID, WATERBODY_NAME, CharacteristicName) %>%
  filter(!WATERBODY_NAME == "15030202-005A" | CharacteristicName == "MANGANESE") %>% # Temporary fix for ATTAINS ERROR
  summarise(newassess = max(newassess), provassess = max(provassess))

# How many of these are ephemeral?
i.param <- i.param %>% 
  left_join(YWBHUCREACH, by = c("WBID", "WATERBODY_NAME")) %>% 
  select(WBID, WATERBODY_NAME, CharacteristicName, newassess, provassess, AWE)

# Save for Potential Restoration Tool
save(i.param, file = "C:/Users/65942/Desktop/impairmentdifficulty/inputs/paramimpair.Rdata")

# Parameter and Use
# Assess by Parameter.  No need for sample fraction since  unique (can't have total and disolved in same use).
i.paramuse <- human %>% 
  group_by(WBID, WATERBODY_NAME, CharacteristicName, Use) %>%
  summarise(newassess = max(newassess), provassess = max(provassess))

# Note this logic says a 2 (meeting) and a 1 (not enough info) for acute/chronic will default to a 2 (meeting)

# Insert ATTAINS Codes
i.paramuse <- i.paramuse %>% 
  mutate(PARAM_ATTAINMENT_CODE = ifelse(provassess == 1, "not enough information", # insufficent information
                                        ifelse(provassess == 2, "meeting criteria", #supporting
                                               ifelse(provassess == 3, "not meeting criteria", "not applicable")))) # not supporting, not assessed

# Import Previous ATTAINS Data.  Update with current priority.  
# Assessment specialist to work with water quality improvement grant / TMDL manager to get updated priorities.
ZPARAMPRIORITY <- read_csv("inputs/ZPARAMPRIORITY.csv",
                           col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                            WBID = col_character(),
                                            PARAM_NAME = col_character(),
                                            CharacteristicName = col_character(),
                                            PARAM_PRIORITY_RANKING = col_character()))

# Import Year Impaired.  Use assessment history and current impairments to update.
ZIMPAIRMENTYEAR <- read_csv("inputs/ZIMPAIRMENTYEAR.csv",
                            col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                             WBID = col_character(),
                                             PARAM_NAME = col_character(),
                                             CharacteristicName = col_character(),
                                             PARAM_YEAR_LISTED = col_character()))

# Parameter ATTAINS ready format
i.attainsparamuse <- i.paramuse %>% 
  ungroup %>% 
  left_join(XATTAINSUSE, by = "Use") %>% 
  left_join(ZPARAMPRIORITY, by = c("WBID", "CharacteristicName")) %>% 
  left_join(ZIMPAIRMENTYEAR, by = c("WBID", "CharacteristicName")) %>% 
  select(WBID, PARAM_USE_NAME, CharacteristicName, PARAM_ATTAINMENT_CODE, PARAM_PRIORITY_RANKING, PARAM_YEAR_LISTED) %>% 
  left_join(YWBHUCREACH, by = "WBID") %>% 
  mutate(lors = ifelse(is.na(REACH_DISTANCE), "L", "")) %>% # this is for the ASSESSMENT_UNIT_ID
  mutate(ASSESSMENT_UNIT_ID = paste0("AZ", lors, WBID, "_00")) %>% # Create ASSESSMENT_UNIT_ID from WBID
  rename(PARAM_NAME = CharacteristicName) %>% 
  mutate(PARAM_STATUS_NAME = "") %>% 
  mutate(PARAM_TREND = "") %>% 
  mutate(PARAM_COMMENT = "") %>% 
  mutate(PARAM_AGENCY_CODE = "S") %>% 
  mutate(PARAM_POLLUTANT_INDICATOR = "") %>% 
  mutate(PARAM_TARGET_TMDL_DATE = "") %>% 
  mutate(PARAM_EXPECTED_TO_ATTAIN = "") %>% 
  mutate(PARAM_CONSENT_DECREE_CYCLE = "") %>% 
  mutate(PARAM_ALT_LISTING_ID = "") %>% 
  mutate(PARAM_STATE_IR_CAT = "")	%>% 
  mutate(PARAM_ORG_QUALIFIER_FLAG = "")	%>% 
  mutate(PARAM_DELISTING_REASON = "") %>% 
  mutate(PARAM_DELISTING_COMMENT = "") %>% 
  mutate(PARAM_DELISTING_AGENCY = "") %>% 
  distinct(ASSESSMENT_UNIT_ID,	PARAM_NAME,	PARAM_USE_NAME,	PARAM_STATUS_NAME,	PARAM_ATTAINMENT_CODE,	PARAM_TREND, PARAM_COMMENT,	PARAM_AGENCY_CODE,	PARAM_POLLUTANT_INDICATOR,	PARAM_YEAR_LISTED,	PARAM_TARGET_TMDL_DATE,	PARAM_EXPECTED_TO_ATTAIN,	PARAM_PRIORITY_RANKING,	PARAM_CONSENT_DECREE_CYCLE,	PARAM_ALT_LISTING_ID,	PARAM_STATE_IR_CAT,	PARAM_ORG_QUALIFIER_FLAG,	PARAM_DELISTING_REASON,	PARAM_DELISTING_COMMENT,	PARAM_DELISTING_AGENCY) %>% 
  select(ASSESSMENT_UNIT_ID,	PARAM_NAME,	PARAM_USE_NAME,	PARAM_STATUS_NAME,	PARAM_ATTAINMENT_CODE,	PARAM_TREND, PARAM_COMMENT,	PARAM_AGENCY_CODE,	PARAM_POLLUTANT_INDICATOR,	PARAM_YEAR_LISTED,	PARAM_TARGET_TMDL_DATE,	PARAM_EXPECTED_TO_ATTAIN,	PARAM_PRIORITY_RANKING,	PARAM_CONSENT_DECREE_CYCLE,	PARAM_ALT_LISTING_ID,	PARAM_STATE_IR_CAT,	PARAM_ORG_QUALIFIER_FLAG,	PARAM_DELISTING_REASON,	PARAM_DELISTING_COMMENT,	PARAM_DELISTING_AGENCY) 

# Grep in Cause/Insufficient info to param_status
i.attainsparamuse[grep("not applicable", i.attainsparamuse$PARAM_ATTAINMENT_CODE), "PARAM_ATTAINMENT_CODE"] <- "not enough information"
i.attainsparamuse[grep("meeting criteria", i.attainsparamuse$PARAM_ATTAINMENT_CODE), "PARAM_STATUS_NAME"] <- "Meeting Criteria"
i.attainsparamuse[grep("not enough information", i.attainsparamuse$PARAM_ATTAINMENT_CODE), "PARAM_STATUS_NAME"] <- "Observed Effect"
i.attainsparamuse[grep("not meeting criteria", i.attainsparamuse$PARAM_ATTAINMENT_CODE), "PARAM_STATUS_NAME"] <- "Cause"

# Cause is applicable to the entire waterbody/pollutant regardless of use.  Isolate causes then join back into dataset
i.attainscause <- i.attainsparamuse %>% 
  select(ASSESSMENT_UNIT_ID, PARAM_NAME, PARAM_STATUS_NAME) %>% 
  filter(PARAM_STATUS_NAME == "Cause") %>% 
  rename(NewStatus = PARAM_STATUS_NAME)

# Causes for entire waterbody identified.  Now join back to dataset and create new field that uses newstatus over old
i.attainsparamuse <- i.attainsparamuse %>% 
  left_join(i.attainscause, by = c("ASSESSMENT_UNIT_ID", "PARAM_NAME")) 

# Grep in the cause from new status
i.attainsparamuse[grep("Cause", i.attainsparamuse$NewStatus), "PARAM_STATUS_NAME"] <- "Cause"

# Drop NewStatus
i.attainsparamuse <- i.attainsparamuse %>% 
  select(-NewStatus)

# EPA changed some domain names.  Map these.  Push EPA to use CharacteristicName.  The Parameter_Name is redundant and annoying to map.
# Note request EPA to create codes for .ALPHA.-HEXACHLOROCYCLOHEXANE, .DELTA.-HEXACHLOROCYCLOHEXANE,.BETA.-HEXACHLOROCYCLOHEXANE, N-NITROSODI-N-PROPYLAMINE, 4,6-DINITRO-O-CRESOL, P-CHLORO-M-CRESOL, N-NITROSODI-N-PROPYLAMINE, O-CHLOROPHENOL, TRIBROMOMETHANE, 
i.attainsparamuse[grep("DISSOLVED OXYGEN", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "DISSOLVED OXYGEN"
i.attainsparamuse[grep("ESCHERICHIA COLI", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "ESCHERICHIA COLI (E. COLI)"
i.attainsparamuse[grep("SUSPENDED SEDIMENT CONCENTRATION", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "SEDIMENTATION/SILTATION"
i.attainsparamuse[grep(".ALPHA.-ENDOSULFAN", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "ENDOSULFAN"
i.attainsparamuse[grep("CHROMIUM\\(VI\\)", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "CHROMIUM, HEXAVALENT" # note escape characters \\
i.attainsparamuse[grep("TRANS-1,3-DICHLOROPROPENE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "1,3-DICHLOROPROPENE"
i.attainsparamuse[grep("CIS-1,3-DICHLOROPROPENE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "1,3-DICHLOROPROPENE"
i.attainsparamuse[grep("P,P'-DDD", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "DDD (DICHLORODIPHENYLDICHLOROETHANE)"
i.attainsparamuse[grep("P,P'-DDE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "DDE (DICHLORODIPHENYLDICHLOROETHYLENE)"
i.attainsparamuse[grep("P,P'-DDT", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "DDT (DICHLORODIPHENYLTRICHLOROETHANE)"
i.attainsparamuse[grep("URANIUM-238", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "URANIUM"
i.attainsparamuse[grep("CHLORDANE, TECHNICAL", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "CHLORDANE"
i.attainsparamuse[grep("CHLOROBENZENE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "CHLOROBENZENE (MONO)"
i.attainsparamuse[grep("CIS-1,3-DICHLOROPROPENE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "1,3-DICHLOROPROPENE"
i.attainsparamuse[grep(".ALPHA.-HEXACHLOROCYCLOHEXANE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "HEXACHLOROCYCLOHEXANE"
i.attainsparamuse[grep(".DELTA.-HEXACHLOROCYCLOHEXANE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "HEXACHLOROCYCLOHEXANE"
i.attainsparamuse[grep(".BETA.-HEXACHLOROCYCLOHEXANE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "HEXACHLOROCYCLOHEXANE"
i.attainsparamuse[grep("N-NITROSODI-N-PROPYLAMINE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "N-NITROSODIPROPYLAMINE"
i.attainsparamuse[grep("4,6-DINITRO-O-CRESOL", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "DINITRO-O-CRESOL"
i.attainsparamuse[grep("P-CHLORO-M-CRESOL", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "M-CRESOL"
i.attainsparamuse[grep("N-NITROSODI-N-PROPYLAMINE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "N-NITROSODIPROPYLAMINE"
i.attainsparamuse[grep("O-CHLOROPHENOL", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "2-CHLOROPHENOL"
i.attainsparamuse[grep("TRIBROMOMETHANE", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "BROMOFORM"
i.attainsparamuse[grep("CHLOROPHYLL", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "CHLOROPHYLL-A"
i.attainsparamuse[grep("EPA-AQUATIC PLANTS", i.attainsparamuse$PARAM_NAME), "PARAM_NAME"] <- "AQUATIC PLANTS (MACROPHYTES)"

# If cause then add a Y to Pollution Indicator
i.attainsparamuse[grep("Cause", i.attainsparamuse$PARAM_STATUS_NAME), "PARAM_POLLUTANT_INDICATOR"] <- "Y"

# Clean up NA's
i.attainsparamuse$PARAM_PRIORITY_RANKING[is.na(i.attainsparamuse$PARAM_PRIORITY_RANKING)] <-""
i.attainsparamuse$PARAM_YEAR_LISTED[is.na(i.attainsparamuse$PARAM_YEAR_LISTED)] <-""

# One off addition of year listed for split reaches
ZPARAMYEAR <- read_csv("inputs/ZPARAMYEAR.csv",
                       col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                        PARAM_NAME = col_character(),
                                        PARAM_USE_NAME = col_character(),
                                        PARAM_STATUS_NAME = col_character(), 
                                        PARAM_ATTAINMENT_CODE = col_character(), 
                                        NEWYEAR = col_double()))

i.attainsparamuse <- i.attainsparamuse %>% 
  left_join(ZPARAMYEAR, by = c("ASSESSMENT_UNIT_ID", "PARAM_NAME", "PARAM_USE_NAME", "PARAM_STATUS_NAME", "PARAM_ATTAINMENT_CODE")) %>% 
  mutate(temp = ifelse(is.na(NEWYEAR), PARAM_YEAR_LISTED, NEWYEAR)) %>% 
  select(-PARAM_YEAR_LISTED) %>% 
  rename(PARAM_YEAR_LISTED = temp) %>% 
  select(ASSESSMENT_UNIT_ID,	PARAM_NAME,	PARAM_USE_NAME,	PARAM_STATUS_NAME,	PARAM_ATTAINMENT_CODE,	PARAM_TREND, PARAM_COMMENT,	PARAM_AGENCY_CODE,	PARAM_POLLUTANT_INDICATOR,	PARAM_YEAR_LISTED,	PARAM_TARGET_TMDL_DATE,	PARAM_EXPECTED_TO_ATTAIN,	PARAM_PRIORITY_RANKING,	PARAM_CONSENT_DECREE_CYCLE,	PARAM_ALT_LISTING_ID,	PARAM_STATE_IR_CAT,	PARAM_ORG_QUALIFIER_FLAG,	PARAM_DELISTING_REASON,	PARAM_DELISTING_COMMENT,	PARAM_DELISTING_AGENCY)

# Save for Upload to ATTAINS
write.csv(i.attainsparamuse, "ATTAINS/Parameters.csv", row.names = FALSE)

# Join human to core/season
i.human <- human %>% 
  left_join(ZCORE, by = c("Use", "CharacteristicName", "ResultSampleFractionText"))

# Open Latest ATTAINS USE FILE
ATTAINSUSES <- read_csv("inputs/ATTAINS2020USES.csv", 
                            col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                             USE_AGENCY_CODE = col_character(), 
                                             USE_ASMT_BASIS = col_character(), 
                                             USE_ASMT_DATE = col_date(format = "%m/%d/%Y"), 
                                             USE_ASSESSOR_NAME = col_character(), 
                                             USE_ATTAINMENT_CODE = col_character(), 
                                             USE_COMMENT = col_double(), USE_MONITORING_END = col_character(), 
                                             USE_MONITORING_START = col_character(), 
                                             USE_NAME = col_character(), USE_ORG_QUALIFIER_FLAG = col_character(), 
                                             USE_STATE_IR_CAT = col_character(), 
                                             USE_THREATENED = col_character(), 
                                             USE_TREND = col_character()))

# Add in WBID and Use
ATTAINSUSES <- ATTAINSUSES %>% 
  mutate(ASSESSMENT_UNIT_IDCOPY = ASSESSMENT_UNIT_ID) %>% 
  separate(ASSESSMENT_UNIT_IDCOPY, c("a", "b"), sep = "_") %>% 
  separate(a, c("c", "d", "e"), sep = "([\\Z\\L])") %>% 
  mutate(WBID = ifelse(is.na(e), d, e)) %>% 
  left_join(XATTAINSUSE, by = c("USE_NAME" = "PARAM_USE_NAME"))

# Assess by Use.  Takes into account core parameters and seasonal distribution and if parameter not meeting criteria the use is not supporting even if not core parameter.
i.maxuse <- i.human %>% 
  group_by(WBID, WATERBODY_NAME, Use) %>%
  summarise(maxuse = max(provassess))

# Use 3 then 2 (just core) then 1.  So Not meeting criteria = Entire Use Not supporting (3), then all core parameters present then use supporting (2) then inconclusive (1)
# min/max provassess used because the fish consumption designated use uses mercury as a core parameter which does not have a water column standard just a fish tissue standard.  Bring in the core parameter for fish from e.corecomp6
i.use <- i.human %>% 
  filter(Core == "Y") %>% 
  group_by(WBID, WATERBODY_NAME, Use) %>%
  summarise(newassess = max(newassess), minprov = min(provassess)) %>% 
  full_join(i.maxuse, by = c("WBID", "WATERBODY_NAME", "Use")) %>% # ensures that if parameter not meeting criteria overrules a meeting or insufficient information for the overall use.
  left_join(e.corecomp6, by = c("WBID", "Use")) 
  
# NA's to 0
i.use$newassess[is.na(i.use$newassess)] <- 0
i.use$minprov[is.na(i.use$minprov)] <- 0

# Prioritize based on logic
i.use <- i.use %>% 
  mutate(provassess = ifelse(minprov < maxuse, minprov, maxuse)) %>% # use the max unless min shows that core missing
  mutate(provassess = ifelse(Use == "FC" & Coreandseason == "Y", 2, minprov)) %>% # Add exception for FC.  This says if Use FC and mercury present in another use then switch to full support
  mutate(provassess = ifelse(maxuse == 3, 3, provassess)) %>% # this overrides the first two with a not supporting if a 3 is present
  arrange(maxuse, minprov, Coreandseason)

# If sites that were not meeting criteria for macroinvertebrates or bottom deposites are attaining for chemistry for the aquatic life use then downgrade to inconclusive
ZBUGSANDPEBBLES <- read_csv("inputs/ZBUGSANDPEBBLES.csv", 
                            col_types = cols(Decision = col_character(), 
                                             Number_meeting_criteria = col_double(), 
                                             Number_samples = col_double(), Parameter = col_character(), 
                                             Use = col_character(), WBID = col_character(), 
                                             comments = col_character()))

i.use <- i.use %>%
  left_join(ZBUGSANDPEBBLES, by = c("WBID", "Use")) %>% 
  mutate(tempid = ifelse((Decision == "Not meeting criteria" & provassess == 2), "Change", "NO"))  

i.use[grep("Change", i.use$tempid), "provassess"] <- 1

# Select just needed fields
i.use <- i.use %>%
  select(WBID, WATERBODY_NAME, Use, newassess, provassess, totyes, total, Coreandseason, fishcore)

# Insert ATTAINS Codes
i.use <- i.use %>% 
  mutate(USE_ATTAINMENT_CODE = ifelse(provassess == 1, "I", # insufficent information
                                      ifelse(provassess == 2, "F", #Fully supporting
                                             ifelse(provassess == 3, "N", "X")))) # not supporting, not assessed

i.use$fishcore[is.na(i.use$fishcore)] <- "Blank"

# Make filtering field...trouble inverting the filter...this is a work around for that
i.use <- i.use %>% 
  mutate(fishfilter = ifelse(USE_ATTAINMENT_CODE == "X" & fishcore == "Sampled", "Y", "N"))

# Use fishcore to change any 'not assessed' FC uses to I if sampled
i.use.p1 <- i.use %>% 
  filter(USE_ATTAINMENT_CODE == "X", fishcore == "Sampled") %>% 
  mutate(USE_ATTAINMENT_CODE = "I")

# Filter for everything but use code x and fish core sampled
i.use.p2 <- i.use %>% 
  filter(fishfilter == "N")

i.use <- bind_rows(i.use.p1, i.use.p2)
  
# If core/season = N then downgrade full support to inconclusive
i.use <- i.use %>%
  mutate(USE_ATTAINMENT_CODE = ifelse(provassess == 2 & Coreandseason == "N", "I", USE_ATTAINMENT_CODE))

# # Previous decisions roll forward unless overridden
i.use <- i.use %>%
  rename(DEQUSEDECISION = USE_ATTAINMENT_CODE) %>%
  full_join(ATTAINSUSES, by = c("WBID", "Use")) %>%
  mutate(combineduse = ifelse(DEQUSEDECISION == "F" & USE_ATTAINMENT_CODE == "I", DEQUSEDECISION,
                              ifelse(DEQUSEDECISION == "I" & is.na(USE_ATTAINMENT_CODE), DEQUSEDECISION,
                                     ifelse(is.na(DEQUSEDECISION), USE_ATTAINMENT_CODE,
                                            ifelse(DEQUSEDECISION == "N", DEQUSEDECISION, USE_ATTAINMENT_CODE))))) %>%
  mutate(combineduse = ifelse(is.na(USE_ATTAINMENT_CODE), DEQUSEDECISION, DEQUSEDECISION)) %>%
  mutate(combineduse = replace(combineduse, is.na(DEQUSEDECISION), USE_ATTAINMENT_CODE)) %>%
    mutate(EPAUSEDECISION = USE_ATTAINMENT_CODE) %>%
    select(-USE_ATTAINMENT_CODE) %>%
    rename(USE_ATTAINMENT_CODE = combineduse) %>% 
  mutate(filtered = ifelse(DEQUSEDECISION == "I" & EPAUSEDECISION == "F", "Y", 
                           ifelse(DEQUSEDECISION == "X" & EPAUSEDECISION == "F", "Y", 
                                  ifelse(DEQUSEDECISION == "I" & EPAUSEDECISION == "N", "Y", "NO")))) %>% 
  mutate(filtered = replace(filtered, is.na(filtered), "NO"))

# Filter out peices that didn't correctly map using if statement and change then bind rows.
i.use.a <- i.use %>% 
  filter(DEQUSEDECISION == "I" & EPAUSEDECISION == "F") %>% 
  mutate(USE_ATTAINMENT_CODE = replace(DEQUSEDECISION, DEQUSEDECISION == "I", "F")) %>% 
  mutate(filtered = replace(DEQUSEDECISION, DEQUSEDECISION == "I", "YES")) 

i.use.b <- i.use %>% 
  filter(DEQUSEDECISION == "X" & EPAUSEDECISION == "F") %>% 
  mutate(USE_ATTAINMENT_CODE = replace(DEQUSEDECISION, DEQUSEDECISION == "X", "F")) %>% 
  mutate(filtered = replace(DEQUSEDECISION, DEQUSEDECISION == "X", "YES")) 

i.use.c <- i.use %>% 
  filter(DEQUSEDECISION == "I" & EPAUSEDECISION == "N") %>% 
  mutate(USE_ATTAINMENT_CODE = replace(DEQUSEDECISION, DEQUSEDECISION == "I", "N")) %>% 
  mutate(filtered = replace(DEQUSEDECISION, DEQUSEDECISION == "I", "YES")) 

# Reverse the filter to exclude the peices filtered for...check that numbers match i.use total
i.use.x <- i.use %>%
  filter(filtered != "Y")

# Combine rows
i.use <- bind_rows(i.use.a, i.use.b, i.use.c, i.use.x)

# Identify carry forward at use level
i.use <- i.use %>% 
  mutate(usecarryforward = ifelse(USE_ATTAINMENT_CODE == DEQUSEDECISION, "Current", "Carry Forward - Use"))

# Pick up the na's and make EPA carry forward
i.use$usecarryforward[is.na(i.use$usecarryforward)] <- "Carry Forward - Use"

# Use ATTAINS format
i.attainsuse <- i.use %>% 
  ungroup() %>% 
  left_join(YWBHUCREACH, by = "WBID") %>% 
  mutate(lors = ifelse(is.na(REACH_DISTANCE), "L", "")) %>% # this is for the ASSESSMENT_UNIT_ID
  mutate(ASSESSMENT_UNIT_ID = paste0("AZ", lors, WBID, "_00")) %>% # Create ASSESSMENT_UNIT_ID from WBID
  select(ASSESSMENT_UNIT_ID, USE_NAME, USE_ATTAINMENT_CODE) %>% 
  mutate(USE_AGENCY_CODE = "S") %>% 
  mutate(USE_TREND = "") %>% 
  mutate(USE_THREATENED = "N") %>% 
  mutate(USE_ASMT_BASIS = "") %>% 
  mutate(USE_MONITORING_START = "") %>% 
  mutate(USE_MONITORING_END = "") %>% 
  mutate(USE_ASMT_DATE = "") %>% 
  mutate(USE_ASSESSOR_NAME = "") %>% 
  mutate(USE_COMMENT = "") 

# Save for Upload to ATTAINS
write.csv(i.attainsuse, "ATTAINS/Uses.csv", row.names = FALSE)

# ATTAINS carries over all previous assessments.  In the 2022 version of the code combine this next step with the previous use file.
# Identify ATTAINS copy/paste from last assessment that are not category 4/5 and make one use 'not assessed'
ZUSENOTASSESSED <- read_csv("inputs/ZUSENOTASSESSED.csv", 
                            col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                             EPA_IR_CATEGORY_ID = col_character(), 
                                             LAST_ASSESSED_CYCLE = col_double(), 
                                             LAST_MODIFIED_CYCLE = col_double(), 
                                             WBID = col_character()))

i.usenotassessed <- ZUSENOTASSESSED %>% 
  select(ASSESSMENT_UNIT_ID) %>% 
  mutate(USE_NAME = "Full Body Contact") %>% # note some uses don't actually have this use but it isn't worth the time to create them just to say they are not assessed
  mutate(USE_ATTAINMENT_CODE = "X") %>% # X is not assessed.
  mutate(USE_AGENCY_CODE = "S") %>% 
  mutate(USE_TREND = "") %>% 
  mutate(USE_THREATENED = "N") %>% 
  mutate(USE_ASMT_BASIS = "") %>% 
  mutate(USE_MONITORING_START = "") %>% 
  mutate(USE_MONITORING_END = "") %>% 
  mutate(USE_ASMT_DATE = "") %>% 
  mutate(USE_ASSESSOR_NAME = "") %>% 
  mutate(USE_COMMENT = "") 

# ATTAINS Associated Actions.  Note no TMDLs added during the assessment cycle so this is the same table as the 2018 cycle.
i.attainsactions <- ATTAINSACTIONS

# Save for Upload to ATTAINS
write.csv(i.attainsactions, "ATTAINS/associated-actions.csv", row.names = FALSE)

# ATTAINS Sources
ZSOURCES <- read_csv("inputs/ZSOURCES.csv", 
                     col_types = cols(ASSESSMENT_UNIT_ID = col_character(), 
                                      CharacteristicName = col_character(), 
                                      SOURCE_COMMENT = col_character(), 
                                      SOURCE_CONFIRMED = col_character(), 
                                      SOURCE_NAME = col_character(), SOURCE_PARAM_NAME = col_character(), 
                                      WBID = col_character()))

# Look for the NA's in Source and fill in using domain list from https://www.epa.gov/waterdata/attains.  If there is more than one source then copy and paste the row information.
# i.sources <- read_csv("ATTAINS/Sources.csv")

# Convert USE_ATTAINMENT to numeric
i.wbid <- i.use %>% 
  mutate(NUM_USE_ATTAIN = 999) 

i.wbid[grep("N", i.wbid$USE_ATTAINMENT_CODE), "NUM_USE_ATTAIN"] <- 3 # A single use not supporting (3) overrule all other use decisions
i.wbid[grep("F", i.wbid$USE_ATTAINMENT_CODE), "NUM_USE_ATTAIN"] <- 1 # All uses need to be 1 for the water body to be supporting. 3's and 2's overrule.
i.wbid[grep("I", i.wbid$USE_ATTAINMENT_CODE), "NUM_USE_ATTAIN"] <- 2 # A single use as inconclusive (1) overrules all other use decisions except 3's
i.wbid[grep("X", i.wbid$USE_ATTAINMENT_CODE), "NUM_USE_ATTAIN"] <- 0

# Group by WBID based on Worst Case
i.wbid <- i.wbid %>% 
  group_by(WBID, WATERBODY_NAME) %>%
  filter(!(is.na(WATERBODY_NAME))) %>% 
  summarise(newassess = max(newassess), provassess = max(NUM_USE_ATTAIN)) 

# # Flip attaining/inconclusive back

# Raw import from oracle treats REACH_DISTANCE as factor. Conversion to numeric messes with the stream miles so this workaround avoids having to figure out how to deal with the factor issue.
YWBHUCREACHSHORT <- YWBHUCREACH %>% 
  select(WBID, REACH_DISTANCE, LAKE_ACRES)

# NA's to 0.  Note need to convert reach distance from a factor to numeric (first line).  
YWBHUCREACHSHORT$REACH_DISTANCE <- as.numeric(levels(YWBHUCREACHSHORT$REACH_DISTANCE))[YWBHUCREACHSHORT$REACH_DISTANCE]
YWBHUCREACHSHORT$REACH_DISTANCE[is.na(YWBHUCREACHSHORT$REACH_DISTANCE)] <- 0
YWBHUCREACHSHORT$LAKE_ACRES[is.na(YWBHUCREACHSHORT$LAKE_ACRES)] <- 0
YWBHUCREACHSHORT$REACH_DISTANCE <- as.numeric(YWBHUCREACHSHORT$REACH_DISTANCE) # you'll get some warnings for the seeps which have "local" instread of a numeric.  OK to ignore.

# Add lake/stream combine miles/acres for summary
YWBHUCREACHSHORT <- YWBHUCREACHSHORT %>% 
  mutate(WATER_TYPE = ifelse(REACH_DISTANCE == 0, "Lake", "Stream")) %>% 
  mutate(ASSESSED = REACH_DISTANCE + LAKE_ACRES)

# How many of these are ephemeral?  Clean up columns
i.wbid <- i.wbid %>% 
  left_join(YWBHUCREACH, by = c("WBID", "WATERBODY_NAME")) %>% 
  select(WBID, WATERBODY_NAME, newassess, provassess, AWE) %>% 
  left_join(YWBHUCREACHSHORT, by = "WBID") 

# Summary by WBID and waterbody type
i.wbidsummary <- i.wbid %>% 
  ungroup() %>% 
  group_by(WATER_TYPE, provassess) %>% 
  summarise(count = n(), sumassessed = sum(ASSESSED))

# Summary by WBID and waterbody type and AWE
i.wbidsummaryawe <- i.wbid %>% 
  ungroup() %>% 
  group_by(WATER_TYPE, provassess, AWE) %>% 
  summarise(count = n(), sumassessed = sum(ASSESSED))

# Identify New Impairments/Not Meeting Criteria
i.newimp <- human %>% 
  filter(provassess == 3 & existimpair == "New Impairment") %>% 
  group_by(WBID, WATERBODY_NAME, Use, CharacteristicName) %>% 
  summarise(count = n())

# Which of these new impairment have existing impairments already?
i.wbidimp <- ATTAINSPARAMETERS %>% 
  filter(PARAM_STATUS_NAME == "Cause")

# Join then distill to find which already have impairments
i.newimp2 <- i.newimp %>% 
  left_join(i.wbidimp, by = "WBID") %>% 
  filter(!is.na(ASSESSMENT_UNIT_ID)) %>% 
  distinct(WBID, WATERBODY_NAME) %>% 
  ungroup() %>% 
  select(WBID, WATERBODY_NAME) %>% 
  mutate(PrevImp = "YES")

# Add to i.newimp for clear id of which waterbodies already were impaired but now have a new parameter impairment and which are completely new
i.newimp <- i.newimp %>% 
  left_join(i.newimp2, by = "WBID") %>% 
  left_join(YWBHUCREACHSHORT, by = "WBID") %>% 
  mutate(GISWBID = paste0(ifelse(WATER_TYPE == "Lake", "AZL", "AZ"), WBID))

# New delists
i.newdelist <- human %>% 
  filter(Assessed == "Meeting criteria" & existimpair == "Existing Impairment" & provassess == 2) 

# Figure out if all the parameters not meeting criteria for the waterbody have been delisted for complete wb delist
i.wbidclear <- i.wbid %>% 
  mutate(clear = ifelse(provassess == 3, "NO", "YES")) %>% 
  select(WBID, clear)

i.newdelist <- i.newdelist %>% 
  left_join(i.wbidclear, by = "WBID") %>% 
  distinct(WBID, WATERBODY_NAME, Use, CharacteristicName, clear) %>% 
  left_join(YWBHUCREACHSHORT, by = "WBID") %>% 
  mutate(GISWBID = paste0(ifelse(WATER_TYPE == "Lake", "AZL", "AZ"), WBID))





#### J - REPORTS AND METRICS ####



# Contribution by agency

# Focus on just decisions made.  Decisions happen on multiple levels.  Start with Meeting/Not meeting criteria at the parameter level.  Excludes existing impairments that are not delist
j.newimpaired <- human %>% 
  filter(existimpair == "New Impairment") %>% 
  filter(provassess == 3) %>% 
  distinct(WBID, WATERBODY_NAME, CharacteristicName) %>% 
  mutate(New = "2020")

j.newdelists <- human %>% 
  filter(existimpair == "Existing Impairment") %>% 
  filter(Assessed == "Meeting criteria") %>% 
  filter(provassess == 2) %>% 
  distinct(WBID, WATERBODY_NAME, CharacteristicName)

c.stddata2[grep("ARIZONA DEPT OF ENVIRONMENTAL QUALITY|HARRIS ENVIRONMENTAL|AZDEQ_SW", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "ADEQ"
c.stddata2[grep("U.S. Geological Survey-Water Resources Discipline|USGS-NV|USGS-AZ|USGS-UT|USGS - Arizona Water Science Center|U.S. Geological Survey|U.S. GEOLOGICAL SURVEY", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "USGS"
c.stddata2[grep("PRESCOTT CREEKS|BUTTE CREEK RESTORATION COUNCIL|FRIEND OF THE FOREST|VERDE RIVER INSTITUTE|SIERRA CLUB|ARAVAIPA GROUP|VOLUNTEER GROUPS|BCRC|SIER|FOF|OAK CREEK WATERSHED COUNCIL|GILA WATERSHED PARTNERSHIP|FRIENDS OF THE TONTO", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "Volunteer"
c.stddata2[grep("PIMA COUNTY WASTEWATER MANAGEMENT DEPT|CITY OF TUCSON|ARIZONA STATE PARKS|AGFD|U.S. National Park Service|11NPSWRD_WQX|SLIDE ROCK STATE PARK|Bureau of Reclamation|Bureau of Land Management", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "Government"
c.stddata2[grep("INTERNATIONAL BOUNDARY AND WATER COMMISSION|GOLDER AND ASSOCIATES, INC.|RESOLUTION COPPER|WALKER ECOLOGICAL SERVICES|UNIVERSITY OF ARIZONA - MARICOPA AGRICULTURAL CENTER", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "Other"

# Download for Dashboard
save(c.stddata2, file = "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZCDATA.Rdata")

# Count of who is doing what
c.org <- c.stddata2 %>% 
  group_by(ActivityConductingOrganizationText) %>% 
  summarise(count = n())

# Verify that all agencies resolved.  Note I opted for shorter names
unique(c.stddata2$ActivityConductingOrganizationText)

# Join c.stddata2 to human
j.org <- c.stddata2 %>% 
  mutate(CharacteristicName = toupper(CharacteristicName)) %>% 
  filter(!is.na(ActivityConductingOrganizationText)) %>% 
  left_join(human, by = c("WBID", "CharacteristicName", "ResultSampleFractionText")) %>% 
  ungroup() %>% 
  group_by(WBID, CharacteristicName, ResultSampleFractionText, Use, provassess, ActivityConductingOrganizationText) %>% 
  summarise(count = n())

# Spread and calculate percent
j.orgspread <- j.org %>%
  spread(ActivityConductingOrganizationText, count) %>% 
  filter(!is.na(provassess))

# Make all na = 0
j.orgspread[is.na(j.orgspread)] <- 0

# Make total column.  2 = sole, 1 = assist, 0 = not in the game
j.orgspread <- j.orgspread %>% 
  mutate(Total = sum(ADEQ, USGS, Volunteer, Government, Other)) %>% 
  mutate(ADEQDEC = ifelse(ADEQ == Total, 2, ifelse(ADEQ < Total & ADEQ >= 1, 1, 0))) %>% 
  mutate(USGSDEC = ifelse(USGS == Total, 2, ifelse(USGS < Total & USGS >= 1, 1, 0))) %>% 
  mutate(VOLUNTEERDEC = ifelse(Volunteer == Total, 2, ifelse(Volunteer < Total & Volunteer >= 1, 1, 0))) %>% 
  mutate(GOVERNMENTDEC = ifelse(Government == Total, 2, ifelse(Government < Total & Government >= 1, 1, 0))) %>% 
  mutate(OTHERDEC = ifelse(Other == Total, 2, ifelse(Other < Total & Other >= 1, 1, 0)))

# Just the results for gather
j.orggather <- j.orgspread %>%
  gather(Org, Orgcode, -WBID, - CharacteristicName, -ResultSampleFractionText, -Use, -provassess, -ADEQ, -USGS, -Volunteer, -Government, -Other, -Total) %>% 
  filter(Orgcode != 0) 

j.orggather[grep("ADEQDEC", j.orggather$Org), "Org"] <- "ADEQ"
j.orggather[grep("USGSDEC", j.orggather$Org), "Org"] <- "USGS"
j.orggather[grep("VOLUNTEERDEC", j.orggather$Org), "Org"] <- "Volunteer"
j.orggather[grep("GOVERNMENTDEC", j.orggather$Org), "Org"] <- "Government"
j.orggather[grep("OTHERDEC", j.orggather$Org), "Org"] <- "Other"

j.orggather[grep(1, j.orggather$Orgcode), "Orgcode"] <- "Assist"
j.orggather[grep(2, j.orggather$Orgcode), "Orgcode"] <- "Solo"
j.orggather[grep(0, j.orggather$provassess), "provassess"] <- "Not Assessed"
j.orggather[grep(1, j.orggather$provassess), "provassess"] <- "Insufficient Information"
j.orggather[grep(2, j.orggather$provassess), "provassess"] <- "Meeting Criteria"
j.orggather[grep(3, j.orggather$provassess), "provassess"] <- "Not Meeting Criteria"

# Summary Metrics by Parameter, Use and Assessment Unit/WBID

# Count of assessed results by parameter
j.parameter <- human %>% 
  group_by(provassess) %>% 
  summarise(Parameter = n()) %>% 
  rename(ParameterAssess = provassess) %>% 
  filter(ParameterAssess > 0)

j.parameter[grep(1, j.parameter$ParameterAssess), "ParameterAssess"] <- "Not enough information"
j.parameter[grep(2, j.parameter$ParameterAssess), "ParameterAssess"] <- "Meeting criteria"
j.parameter[grep(3, j.parameter$ParameterAssess), "ParameterAssess"] <- "Not meeting criteria"

# Count of assessed results by use
j.use <- i.use %>% 
  group_by(USE_ATTAINMENT_CODE) %>% 
  summarise(Use = n()) %>% 
  rename(UseAssess = USE_ATTAINMENT_CODE)

j.use[grep("X", j.use$UseAssess), "UseAssess"] <- "Not assessed"
j.use[grep("I", j.use$UseAssess), "UseAssess"] <- "Insufficient information"
j.use[grep("F", j.use$UseAssess), "UseAssess"] <- "Supporting"
j.use[grep("N", j.use$UseAssess), "UseAssess"] <- "Not supporting"

# Count of assessed results by assessment unit
j.wbid <- i.wbid %>% 
  group_by(provassess) %>% 
  summarise(WBID = n()) %>% 
  rename(WBIDAssess = provassess)

j.wbid[grep(0, j.wbid$WBIDAssess), "WBIDAssess"] <- "Not assessed"
j.wbid[grep(2, j.wbid$WBIDAssess), "WBIDAssess"] <- "Inconclusive"
j.wbid[grep(1, j.wbid$WBIDAssess), "WBIDAssess"] <- "Attaining"
j.wbid[grep(3, j.wbid$WBIDAssess), "WBIDAssess"] <- "Impaired"

# Appendix A - Assessment Decisions

# Assessment Report Showing Homework.  Break down by WBID, Use, Parameter.  
j.report <- human %>% 
  rename(WaterbodyName = WATERBODY_NAME) %>% 
  rename(HUMANDEC = provassess) %>% 
  select(-newassess) %>% 
  left_join(i.paramuse, by = c("WBID", "Use", "CharacteristicName")) %>% 
  rename(DecisionParameter = PARAM_ATTAINMENT_CODE) %>% 
  select(WBID, WaterbodyName, NewUse, Use, CharacteristicName, ResultSampleFractionText, binomial, No, Yes, sampcount, existimpair, provcomment, paramcarryforward, DecisionParameter) %>% 
  left_join(i.use, by = c("WBID", "Use")) %>% 
  rename(DecisionUse = USE_ATTAINMENT_CODE) %>% 
  select(WBID, WaterbodyName, NewUse, Use, CharacteristicName, ResultSampleFractionText, binomial, No, Yes, sampcount, existimpair, provcomment, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse) %>% 
  left_join(i.wbid, by = "WBID") %>% 
  rename(DecisionWBID = provassess) %>% 
  select(WBID, WaterbodyName, NewUse, Use, CharacteristicName, ResultSampleFractionText, binomial, No, Yes, sampcount, existimpair, provcomment, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse, DecisionWBID) %>% 
  rename(NumberCriteriaMet = No) %>% 
  rename(NumberCriteriaNotMet = Yes) %>% 
  rename(TotalSamples = sampcount) %>% 
  rename(ImpairmentType = existimpair) %>% 
  rename(Binomial = binomial) %>% 
  rename(Comment = provcomment) %>% 
  rename(AcuteChronic = NewUse) %>% 
  left_join(YWBHUCREACH, by = "WBID") %>% 
  select(WBID, WaterbodyName, AcuteChronic, Use, CharacteristicName, ResultSampleFractionText, Binomial, NumberCriteriaMet, NumberCriteriaNotMet, TotalSamples, ImpairmentType, Comment, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse, DecisionWBID, WATERSHED, REACH_DISTANCE, LAKE_ACRES, ORIGIN, TERMINUS)
  
j.report[grep("\\bN\\b", j.report$DecisionUse), "DecisionUse"] <- "Not supporting" #//b //b allows for exact find and replace which is important here.  Otherwise Grep N would pick up anything with n.
j.report[grep("X", j.report$DecisionUse), "DecisionUse"] <- "Not assessed"
j.report[grep("\\bI\\b", j.report$DecisionUse), "DecisionUse"] <- "Insufficient information"
j.report[grep("\\bF\\b", j.report$DecisionUse), "DecisionUse"] <- "Supporting"
j.report[grep(0, j.report$DecisionWBID), "DecisionWBID"] <- "Not assessed"
j.report[grep(2, j.report$DecisionWBID), "DecisionWBID"] <- "Inconclusive"
j.report[grep(1, j.report$DecisionWBID), "DecisionWBID"] <- "Attaining"
j.report[grep(3, j.report$DecisionWBID), "DecisionWBID"] <- "Impaired"
j.report[grep("not applicable", j.report$DecisionParameter), "DecisionParameter"] <- "not enough information"

# Subset Impaired Waters List for Category 5's
j.impairedlist <- j.report %>% 
  filter(DecisionWBID == "Impaired") %>% 
  left_join(ATTAINSACTIONS, by = c("WBID", "CharacteristicName")) %>% 
  filter(is.na(ACTION_PARAM_NAME)) %>% 
  filter(DecisionParameter == "not meeting criteria") 

# NA's to 0
j.impairedlist$LAKE_ACRES[is.na(j.impairedlist$LAKE_ACRES)] <- 0
j.impairedlist$REACH_DISTANCE <- as.numeric(j.impairedlist$REACH_DISTANCE)
j.impairedlist$REACH_DISTANCE[is.na(j.impairedlist$REACH_DISTANCE)] <- 0

# Add lake/stream combine miles/acres for summary
j.impairedlist <- j.impairedlist %>% 
  mutate(Assessed = REACH_DISTANCE + LAKE_ACRES) %>% 
  mutate(AssessedUnit = ifelse(REACH_DISTANCE == 0, "Acres", "Miles")) %>% 
  distinct(WBID, WaterbodyName, CharacteristicName, WATERSHED, Assessed, AssessedUnit) %>% 
  left_join(j.newimpaired, by = c("WBID", "WaterbodyName" = "WATERBODY_NAME", "CharacteristicName")) 

j.impairedlist$New[is.na(j.impairedlist$New)] <- ""

j.impairedlist <- j.impairedlist %>% 
  mutate(CharacteristicNameNew = ifelse(New == "2020", paste0(CharacteristicName, " - New in ", New), CharacteristicName))

# Now collapse for more traditional report
j.impairedlist <- j.impairedlist %>% 
  group_by(WBID) %>% 
  mutate(CharacteristicNameCollapsed = paste0("(",CharacteristicNameNew, ") ", collapse = "")) %>% 
  distinct(WBID, WaterbodyName, WATERSHED, Assessed, AssessedUnit, CharacteristicNameCollapsed) %>% 
  rename(Cause = CharacteristicNameCollapsed)
  
# EPA Categories 1 to 5 Assignments.  Note ATTAINS does this as well.  Comment out when it is working.
j.epa5s <- j.impairedlist %>% 
  select(WBID) %>% 
  mutate(EPACategory = "EPACat5")

j.epa4s <- ATTAINSACTIONS %>% 
  distinct(WBID) %>% 
  mutate(EPACategory = "EPACat4") %>% 
  filter(WBID != "15050302-0760")

j.epa4and5 <- j.epa5s %>% 
  bind_rows(j.epa4s) %>% 
  mutate(count = 1) %>% 
  spread(EPACategory, count) %>% 
  filter(WBID != "15020001-011") # LCR delisted but not a category 5 so not on offical delist report.  TMDL still in place.

# Grep 4's and 5's
j.epa4and5[grep("1", j.epa4and5$EPACat4), "EPACat4"] <- 4
j.epa4and5[grep("1", j.epa4and5$EPACat5), "EPACat5"] <- 5
j.epa4and5[is.na(j.epa4and5)] <- 0

# Resolve differences
j.epa4and5 <- j.epa4and5 %>% 
  mutate(EPACategory = EPACat4 + EPACat5)

# 5's override 4's at the assessment level.  
j.epa4and5[grep("9", j.epa4and5$EPACategory), "EPACategory"] <- 5

# Assign all categories
j.epacat <- i.use %>% 
  select(WBID, WATERBODY_NAME, Use, provassess) %>% 
  spread(Use, provassess) %>% 
  left_join(j.epa4and5, by = "WBID")

# Figure out which assessment units have every use supporting vs just one
j.epacat <- j.epacat %>% 
  mutate(maxcat = pmax(AGI, AGL, AWC, AWE, AWEDW, AWW, DWS, FBC, FC, PBC, na.rm = TRUE)) %>% 
  mutate(mincat = pmin(AGI, AGL, AWC, AWE, AWEDW, AWW, DWS, FBC, FC, PBC, na.rm = TRUE)) %>% 
  mutate(newepacat = ifelse(maxcat == 2 & mincat == 2, "1 Supporting All Uses",
                            ifelse(maxcat == 2 & mincat == 1, "2 Supporting Some Uses", "3 Inconclusive")))

# Overrule based on cat 4 & 5.  Keeping in mind that only santa cruz is a 4B
j.epacat[grep("5", j.epacat$EPACategory), "newepacat"] <- "5 Impaired"
j.epacat[grep("4", j.epacat$EPACategory), "newepacat"] <- "4A Not Attaining TMDL Complete"

# Clean up
j.epacat <- j.epacat %>% 
  left_join(YWBHUCREACHSHORT, by = "WBID") %>% 
  select(WBID, WATERBODY_NAME, newepacat, WATER_TYPE, ASSESSED) 

# GIS file splitting out lakes and streams
j.gisepacat <- j.epacat %>% 
  mutate(GISWBID = paste0(ifelse(WATER_TYPE == "Lake", "AZL", "AZ"), WBID)) 

# Streams
j.gisepacatstream <- j.gisepacat %>% 
  filter(WATER_TYPE == "Stream")

# Lakes
j.gisepacatlake <- j.gisepacat %>% 
  filter(WATER_TYPE == "Lake")

write.csv(j.gisepacatstream, "outputs/2022assessmentGISstream.csv", row.names = FALSE)
write.csv(j.gisepacatlake, "outputs/2022assessmentGISlake.csv", row.names = FALSE)

# Add epacategory to report
j.epacatshort <- j.epacat %>% 
  ungroup() %>% 
  select(WBID, newepacat) %>% 
  rename(EPACategory = newepacat)

j.report <- j.report %>% 
  left_join(j.epacatshort, by = "WBID") %>% 
  select(WBID, WaterbodyName, AcuteChronic, Use, CharacteristicName, ResultSampleFractionText, Binomial, NumberCriteriaMet, NumberCriteriaNotMet, TotalSamples, ImpairmentType, Comment, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse, DecisionWBID, EPACategory, WATERSHED, REACH_DISTANCE, LAKE_ACRES, ORIGIN, TERMINUS)

# Add if core parameter/exceedance to report comment.  Logic = comment priority 1. new impairment/delist 2. exceedance. 3. Core parameter/season
j.exceed <- f.exceed %>% 
  ungroup() %>% 
  filter(Exceed == "Yes") %>% 
  distinct(WBID, Use, NewUse, CharacteristicName, ResultSampleFractionText, Exceed)

# Get Everything in one table then apply logic
j.reporta <- j.report %>%
  left_join(e.corecomp5, by = c("WBID", "Use", "CharacteristicName", "ResultSampleFractionText")) %>% 
  rename(Coreseason = Yes) %>% 
  left_join(j.exceed, by = c("WBID", "Use", "CharacteristicName", "ResultSampleFractionText", "AcuteChronic" = "NewUse")) %>% 
  mutate(logic = ifelse(!is.na(Comment), Comment, 
                        ifelse(ImpairmentType == "Existing Impairment", "Existing impairment", 
                        ifelse(Exceed == "Yes", "Insufficient Information - Exceedance"))))

j.reporta <- j.reporta %>% 
  left_join(ZCARRYFORWARDMAP, by = c("CharacteristicName", "Use")) %>% 
  mutate(AcuteChronic = ifelse(is.na(AcuteChronic), cfNewUse, AcuteChronic)) %>% 
  mutate(ResultSampleFractionText = ifelse(is.na(ResultSampleFractionText), cfResultSampleFraction, ResultSampleFractionText)) %>% 
  select(-cfResultSampleFraction, -cfNewUse)

# Change any new impairments to no that are not listed as not meeting criteria
j.reporta <- j.reporta %>% 
  mutate(Commentnew = logic) %>% 
  mutate(newImpairmentType = ifelse(ImpairmentType == "New Impairment" & DecisionParameter != "not meeting criteria", "No", ImpairmentType))

# Clean up fields
j.reporta <- j.reporta %>%
  select(WBID, WaterbodyName, AcuteChronic, Use, CharacteristicName, ResultSampleFractionText, Binomial, NumberCriteriaMet, NumberCriteriaNotMet, TotalSamples, newImpairmentType, Commentnew, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse, DecisionWBID, EPACategory, WATERSHED, REACH_DISTANCE, LAKE_ACRES, ORIGIN, TERMINUS) %>% 
  rename(ImpairmentType = newImpairmentType, Comment = Commentnew)

# Appendix B - Assessment  Exceedance report

# Filter results to just exceedances
j.exceed <- f.exceed %>% 
  filter(Exceed == "Yes") %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, SUBSTANCE_CAS_NO, STDNEW, Exceed)

# Report with exceedances
j.reportexceedance <- j.reporta %>% 
  rename(NewUse = AcuteChronic) %>% 
  left_join(j.exceed, by = c("WBID", "CharacteristicName", "ResultSampleFractionText", "NewUse")) %>% 
  rename(AcuteChronic = NewUse) %>% 
  rename(SampleDate = aggdate) %>% 
  rename(SampleResult = aggtimespace) %>% 
  rename(ExceedanceComment = SUBSTANCE_CAS_NO) %>% 
  rename(Standard = STDNEW) %>% 
  filter(Exceed == "Yes") %>% 
  select(WBID, WaterbodyName, AcuteChronic, Use, CharacteristicName, ResultSampleFractionText, Binomial, NumberCriteriaMet, NumberCriteriaNotMet, TotalSamples, ImpairmentType, Comment, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse, DecisionWBID, EPACategory, WATERSHED, REACH_DISTANCE, LAKE_ACRES, ORIGIN, TERMINUS, SampleDate, SampleResult, ExceedanceComment, Standard, Exceed)

# Appendix D - Critical Conditions
j.critcond <- ZCRITICALCONDITION %>% 
  rename(WaterbodyName = WATERBODY_NAME) 
  
# Appendix E - Priority Ranking 
j.priority <- ZPARAMPRIORITY %>% 
  rename(PriorityRanking = PARAM_PRIORITY_RANKING) %>% 
  select(WBID, CharacteristicName, PriorityRanking) %>% 
  left_join(YWATERBODYNAME, by = "WBID") %>% 
  rename(WaterbodyName = WATERBODY_NAME) %>% 
  distinct(WBID, WaterbodyName, CharacteristicName, PriorityRanking) 

# Note report a output moved to j.explorer
write.csv(j.reportexceedance, "Appendix B - 2022- Assessment Exceedances.csv", row.names = FALSE)
write.csv(j.impairedlist, "Appendix D - 2022 Impaired Waters List.csv", row.names = FALSE)
write.csv(j.critcond, "Appendix C - 2022 Critical Conditions.csv", row.names = FALSE)
write.csv(j.priority, "Appendix E - 2022 TMDL Priority Ranking.csv", row.names = FALSE)

# Add in rta dashboard outputs
write.csv(j.reportexceedance, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZAPPB-EXCEEDANCE.csv", row.names = FALSE)
write.csv(j.critcond, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZAPPC-CRITICALCOND.csv", row.names = FALSE)

# WBID Acre/Miles and Count
j.wbidsum <- j.report %>% 
  mutate(WATER_TYPE = ifelse(is.na(REACH_DISTANCE), "LAKE", "STREAM"))

j.wbidsum$REACH_DISTANCE <- as.numeric(j.wbidsum$REACH_DISTANCE)
j.wbidsum[is.na(j.wbidsum)] <- 0

j.wbidsum <- j.wbidsum %>% 
  mutate(ASSESSED = REACH_DISTANCE + LAKE_ACRES) %>% 
  ungroup() %>% 
  distinct(WBID, DecisionWBID, WATER_TYPE, ASSESSED) %>% 
  group_by(DecisionWBID, WATER_TYPE) %>% 
  summarise(mileacres = sum(ASSESSED), count = n())

# Category Summary Count
j.wbidsumcat <- j.report %>% 
  mutate(WATER_TYPE = ifelse(is.na(REACH_DISTANCE), "LAKE", "STREAM"))

j.wbidsumcat$REACH_DISTANCE <- as.numeric(j.wbidsumcat$REACH_DISTANCE)
j.wbidsumcat[is.na(j.wbidsumcat)] <- 0

j.wbidsumcat <- j.wbidsumcat %>% 
  mutate(ASSESSED = REACH_DISTANCE + LAKE_ACRES) %>% 
  ungroup() %>% 
  distinct(WBID, EPACategory, WATER_TYPE, ASSESSED) %>% 
  group_by(EPACategory, WATER_TYPE) %>% 
  summarise(mileacres = sum(ASSESSED), count = n())

# Pulls in e.datagap at use level and crossreferences at parameter level to simplify monitoring datagap needs.  
# Datagaps include inconcluses and existing impaired sites that need confirmation
j.datagap <- human %>% 
  select(WBID, NewUse, Use, CharacteristicName, ResultSampleFractionText, No, Yes, sampcount, binomial, newassess, provassess, existimpair, actualsampneed) %>% 
  full_join(e.datagap, by = c("WBID", "Use", "CharacteristicName", "ResultSampleFractionText")) %>% # shows missing at use level
  rename(paramsampneed = actualsampneed) %>% # shows missing at parameter level
  rename(coresampneed = sampleneed) 

# Make na'a equal 0
j.datagap[is.na(j.datagap)] <- 0

# Identify Inconclusives.  Basically narrow down why is each waterbody's use = inconclusive? 
j.useinc <- i.use %>%
  ungroup() %>% 
  filter(USE_ATTAINMENT_CODE == "I") %>% 
  mutate(reason = "inconclusive") %>% 
  select(WBID, Use, reason)

# Clean up data.  Uses that are inconclusive
j.datagapi <-j.datagap %>% 
  left_join(j.useinc, by = c("WBID", "Use")) %>%
  filter(reason == "inconclusive")

# Identify existing impairments that could be delisted and need confirmation sampling
j.datagapc <- j.datagap %>%
  filter(existimpair == "Existing Impairment" & paramsampneed > 0 & Yes == 0 & provassess == 3) %>% 
  mutate(reason = "potential delist")  

# Combine
j.datagap <- rbind(j.datagapi, j.datagapc)

# More Clean Up
j.datagap <- j.datagap %>% 
  mutate(paramcoresampneed = ifelse(paramsampneed >= coresampneed, paramsampneed, coresampneed)) %>% # combines the use and parameter needs
  mutate(currentassessment = "AZ2022 CWA Assessment from 7/1/2016 to 6/30/21") %>%
  left_join(YWATERBODYNAME, by = "WBID") %>%
  select(WBID, WATERBODY_NAME, NewUse, Use, CharacteristicName, ResultSampleFractionText, No, Yes, sampcount, binomial, newassess,
         provassess, existimpair, paramsampneed, coresampneed, donotsampleseasons, paramcoresampneed, currentassessment, reason)

# Assign monitoring priority.  
# High = possible delists of existing impairments; active remediation sites, inconclusives with exceedances.  
# Low = No impairment/ no exceedance; not high/med.  Medium = Inconclusive with no exceedances & need just 1 sample.  
j.datagap <- j.datagap %>% 
  rename(Noexceedance = No) %>%
  rename(Yesexceedance = Yes) %>%
  rename(autoassess = newassess) %>% 
  mutate(exceed = ifelse(Yesexceedance > 0, "Yes", "No")) %>% 
  mutate(exceedorcore = ifelse(exceed == "Yes" | coresampneed > 0, "Yes", "No")) %>% 
  group_by(WBID) %>% 
  mutate(maxsampneed = max(paramcoresampneed)) %>% 
  mutate(monitoringpriority = ifelse(reason == "potential delist", "High - Potential Delist", 
                                     ifelse(Yesexceedance > 0 & provassess == 1, "High - Exceedances",
                                            ifelse(paramcoresampneed == 0, "NA - Sampling Complete",
                                                   ifelse(maxsampneed <= 2 & exceedorcore == "Yes", "Low - Need 1 or 2 Samples",
                                                          ifelse(Yesexceedance == 0 & provassess == 0, "Low", "Low")))))) 

# Either figure out an automated way to pull this or update periodically
j.datagap[grep("15030202-005A|15030202-005B|15030202-005C|15060103-018A", j.datagap$WBID), "monitoringpriority"] <- "High - Restoration"

j.datagap[grep(0, j.datagap$provassess), "provassess"] <- "Not assessed"
j.datagap[grep(1, j.datagap$provassess), "provassess"] <- "Not enough information"
j.datagap[grep(2, j.datagap$provassess), "provassess"] <- "Meeting criteria"
j.datagap[grep(3, j.datagap$provassess), "provassess"] <- "Not meeting criteria"
j.datagap[grep(0, j.datagap$autoassess), "autoassess"] <- "Not assessed"
j.datagap[grep(1, j.datagap$autoassess), "autoassess"] <- "Not enough information"
j.datagap[grep(2, j.datagap$autoassess), "autoassess"] <- "Meeting criteria"
j.datagap[grep(3, j.datagap$autoassess), "autoassess"] <- "Not meeting criteria"

# Open Cost
ZLABCOST <- read_csv("inputs/ZLABCOST.csv", 
                     col_types = cols(CharacteristicName = col_character(), 
                                      Cost = col_double(), ResultSampleFractionText = col_character()))
# Join cost to datagap
j.datagapcost <- j.datagap %>% 
  left_join(ZLABCOST, by = c("CharacteristicName", "ResultSampleFractionText")) %>% 
  mutate(unitcost = paramcoresampneed * Cost) %>% 
  group_by(WBID) %>% 
  summarise(Totalcost = sum(unitcost))

# Filter for just high or medium.  Add column with just high/med. 
j.datagap <- j.datagap %>% 
  ungroup() %>% 
  left_join(j.datagapcost, by = "WBID") %>% 
  mutate(shortnew = ifelse(grepl("High", monitoringpriority, fixed = TRUE), 0,
                           ifelse(grepl("Low", monitoringpriority, fixed = TRUE), 10, 
                                  ifelse(grepl("Low", monitoringpriority, fixed = TRUE), 20, 
                                         ifelse(monitoringpriority == "NA - Sampling Complete", 30, 40))))) %>%
  mutate(value = 1) %>% 
  mutate(lowcost = ifelse(shortnew == 20 & Totalcost <= 550, "Y", "N"))

j.datagap[grep("Y", j.datagap$lowcost), "shortnew"] <- 10

# Pick worst case based on WBID so if one is high then make sure all core parameters that might be in a low status are also sampled.
# From https://community.rstudio.com/t/how-to-use-any-within-mutate-and-group-by/22665
j.datagapgis <- j.datagap %>% 
  group_by(WBID) %>% 
  mutate(short = case_when(
    min(shortnew) == 0 ~ "High",
    min(shortnew) == 10 ~ "Low",
    min(shortnew) == 20 ~ "Low",
    min(shortnew) == 30 ~ "Complete"
  )) 

# Filter out completed
j.datagapgis <- j.datagapgis %>% 
  filter(monitoringpriority != "NA - Sampling Complete") %>% 
  mutate(season = ifelse(donotsampleseasons == 0, "Any", donotsampleseasons)) %>% 
  filter(exceedorcore == "Yes" | reason == "potential delist") # important filter.  The no's are datagaps but don't influence the parameter/use decisions

# Combine all the parameters
j.datagapgis <- j.datagapgis %>% 
  ungroup() %>% 
  mutate(combo = paste("(", j.datagapgis$ResultSampleFractionText, j.datagapgis$CharacteristicName, "-", j.datagapgis$paramcoresampneed, "-", season, ")")) %>% 
  distinct(WBID, WATERBODY_NAME, short, combo, maxsampneed, Totalcost) 

# Collapse into one row by WBID so if you click on a site all the information appears that is needed for sampling.
j.datagapgis <- j.datagapgis %>% 
  group_by(WBID, short, maxsampneed) %>% 
  arrange(WBID) %>% 
  summarise(sampleneeds = paste(combo, collapse = ","), cost = max(Totalcost))

# Join to sites with most data or recent data
load(file = "inputs/ZNORMALIZEDSITES.RData")

j.datagapgis2 <- left_join(j.datagapgis, ZNORMALIZEDSITES, by = "WBID")

# Add directions from WQDB
YSW_SITESshort <- YSW_SITES %>% 
  select(WBID, STATION_ACCESS) %>% 
  distinct(WBID, .keep_all = TRUE)

# Import datagap ranking and ranking info
load(file = "inputs/ZDATAGAPRANK.RData")

# Clean up datagap ranking
ZDATAGAPRANK <- ZDATAGAPRANK %>% 
  select(WBID, Flowstatus, Ownership, Directions, DriveMinutes, HikeMinutes, Restrictions, BestSite, DirectionsLastUpdate, Comment, STAFFREVIEW, Ranknorm, Group) %>% 
  rename(googledirections = Directions)

# Rank index by WBID and # Pick The most recent/highest count sites
j.datagapgis3 <- j.datagapgis2 %>% 
  group_by(WBID) %>% 
  # mutate(rank = order(order(Index, decreasing = TRUE))) %>% 
  # filter(rank == 1) %>% 
  drop_na(WBID) %>% 
  # drop_na(LATITUDE_MEASURE) %>% 
  left_join(YWATERBODYNAME, by = "WBID") %>% 
  select(WBID, WATERBODY_NAME, short, maxsampneed, sampleneeds, LATITUDE_MEASURE, LONGITUDE_MEASURE, STATION_CD, STATION_TYPE_NAME, cost) %>% 
  rename(monitoringpriority = short) %>% 
  mutate(cycle = "AZ2022 CWA Assessment from 7/1/2016 to 6/30/21") %>% 
  mutate(type = "Datagap") %>% 
  left_join(j.useinc, by = "WBID") %>% 
  left_join(YSW_SITESshort, by = "WBID") %>% 
  left_join(ZDATAGAPRANK, by = "WBID") %>% 
  mutate(Rank = ifelse(Ranknorm >= 0.27, "Hard", "Easy")) %>% 
  distinct(WBID, .keep_all = TRUE)  

# Append Source ID
load(file = "inputs/YSOURCEID.RData")

# Datagap Input file with decisions and champions/lead samplers
ZDATAGAPINPUT <- read_csv("inputs/ZDATAGAPINPUT.csv", 
                          col_types = cols(Champion = col_character(), 
                                           Decision = col_character(), WBID = col_character()))

# Make Source ID into one row 
YSOURCEID2 <- YSOURCEID %>% 
  filter(!is.na(LATITUDE_MEASURE)) %>% 
  rename(temp = STATION_CD) %>% 
  rename(slat = LATITUDE_MEASURE) %>% 
  rename(slong = LONGITUDE_MEASURE) %>% 
  rename(SourceIDNeeds = sampleneeds) %>% 
  group_by(WBID, SourceIDNeeds) %>% 
  summarise(SourceIDSites = paste(temp, collapse = ","), slat = mean(slat), slong = mean(slong))

# Identify potential delists from j.datagapc add to reason
j.datagapcshort <- j.datagapc %>% 
  distinct(WBID, reason) %>% 
  rename(PotentialDelist = reason)

# Join with datagaps and identify potential delists.
j.datagapgis4 <- j.datagapgis3 %>% 
  full_join(YSOURCEID2, by = "WBID") %>% 
  rename(DatagapNeeds = sampleneeds) %>% 
  rename(DatagapNumbersamples = maxsampneed) %>% 
  rename(DatagapSite = STATION_CD) %>% 
  mutate(latitude = ifelse(!is.na(LATITUDE_MEASURE), LATITUDE_MEASURE, slat)) %>% 
  mutate(longitude = ifelse(!is.na(LONGITUDE_MEASURE), LONGITUDE_MEASURE, slong)) %>% 
  select(WBID, monitoringpriority, DatagapNumbersamples, latitude, longitude, DatagapSite, DatagapNeeds, Flowstatus, Ownership, googledirections, DriveMinutes, HikeMinutes, Restrictions, SourceIDNeeds, SourceIDSites) %>% 
  left_join(YSW_SITESshort, by = "WBID") %>%
  left_join(YWATERBODYNAME, by = "WBID") %>% 
  left_join(ZDATAGAPINPUT, by = "WBID") %>%
  select(WBID, WATERBODY_NAME, monitoringpriority, latitude, longitude, Flowstatus, Ownership, STATION_ACCESS, googledirections, DriveMinutes, HikeMinutes, Restrictions, DatagapNumbersamples, DatagapSite, DatagapNeeds, SourceIDSites, SourceIDNeeds, Champion, Decision) %>% 
  left_join(j.datagapcshort, by = "WBID") %>% 
  mutate(SourceID = ifelse(!is.na(SourceIDSites), "YES", "NO")) %>% 
  mutate(NewSinceOriginal = ifelse(is.na(HikeMinutes) & is.na(SourceIDSites), "YES", "NO")) %>% 
  mutate(type = ifelse(!is.na(Decision), "Decision Made", # 1st ifelse overrides the following
                       ifelse(!is.na(Champion), "Assigned",
                              ifelse(is.na(Champion) & monitoringpriority == "High", "Not Assigned - High Priority",
                                     ifelse(monitoringpriority == "Low" & is.na(Champion), "Not Assigned - Low Priority", 
                                            "Low - Other")))))

# Get Rid of Line Breaks.  Line breaks create problems for GIS process.  
j.datagapgis4$STATION_ACCESS <- gsub("[\r\n]", "", j.datagapgis4$STATION_ACCESS)
j.datagapgis4$googledirections <- gsub("[\r\n]", "", j.datagapgis4$googledirections)

# This line is a little dangerous...assumes all na's are source id, which might not always be true
j.datagapgis4$type[is.na(j.datagapgis4$type)] <- "Not Assigned - SourceID"

# Clean up.  Just one newest/highest record count site per WBID
j.normalsite <- ZNORMALIZEDSITES %>% 
  group_by(WBID) %>% 
  filter(Index == max(Index)) %>% 
  distinct(WBID, .keep_all = TRUE) # resolves if multiple indexes are equal in previous step

# Clean up.  Add lat longs and sites to decisions made
j.datagapgis5 <- j.datagapgis4 %>% 
  left_join(j.normalsite, by = "WBID") %>% 
  mutate(latitude = ifelse(is.na(latitude), LATITUDE_MEASURE, latitude)) %>% 
  mutate(longitude = ifelse(is.na(longitude), LONGITUDE_MEASURE, longitude)) %>% 
  mutate(DatagapSite = ifelse(is.na(DatagapSite), STATION_CD, DatagapSite)) %>% 
  mutate(WATERBODY_NAME = ifelse(is.na(WATERBODY_NAME), STATION_ALT_NAME, WATERBODY_NAME)) %>% 
  select(WBID, WATERBODY_NAME, monitoringpriority, latitude, longitude, Flowstatus, Ownership, STATION_ACCESS, googledirections, DriveMinutes, HikeMinutes, Restrictions, DatagapNumbersamples, DatagapSite, DatagapNeeds, SourceIDSites, SourceIDNeeds, PotentialDelist, Champion, Decision, type) 
  
# Combine the itemized datagap list with appendix A for explorer tool
j.dgshort <- j.datagap %>% 
  filter(exceedorcore == "Yes") %>% 
  select(WBID, ResultSampleFractionText, CharacteristicName, NewUse, Use, donotsampleseasons, paramcoresampneed)

# Fill in missing acute/chronic using carryforward map (has nothing to do with carry forward but map already exists)
j.dgshort <- j.dgshort %>% 
  left_join(ZCARRYFORWARDMAP, by = c("CharacteristicName", "Use")) %>% 
  mutate(NewUse = ifelse(NewUse == 0, cfNewUse, NewUse)) %>% 
  mutate(NewUse = ifelse(CharacteristicName == "INORGANIC NITROGEN (NITRATE AND NITRITE)", "DWS", NewUse)) %>% 
  select(-cfResultSampleFraction, -cfNewUse)

# For Dashboard.  Combines appendix a decisions with what is missing and identifies core parameters and if core and seasonal distribution met
j.core <- e.corecomp4 %>% 
  select(WBID, Use, CharacteristicName, ResultSampleFractionText, COREPARAMETER, COREANDSEASON)

# Load Paste Function that ignores NA's https://stackoverflow.com/questions/13673894/suppress-nas-in-paste
paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}

# Add lake/stream combine miles/acres for summary
YWBHUCREACHSHORT2 <- YWBHUCREACHSHORT %>% 
  select(WBID, WATER_TYPE) 

# Report for Appendix A showing how every assessment decision is made
j.explorer <- j.reporta %>% 
  select(WBID, WaterbodyName, AcuteChronic, Use, CharacteristicName, ResultSampleFractionText, Binomial, NumberCriteriaMet, NumberCriteriaNotMet, TotalSamples, ImpairmentType, Comment, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse, DecisionWBID, EPACategory, WATERSHED, REACH_DISTANCE, LAKE_ACRES, ORIGIN, TERMINUS) %>% 
  full_join(j.dgshort, by = c("WBID", "ResultSampleFractionText", "CharacteristicName", "AcuteChronic" = "NewUse", "Use")) %>% 
# fix wb
  group_by(WBID) %>% 
  arrange(WBID, DecisionWBID) %>%
  fill(WaterbodyName, DecisionWBID, EPACategory, .direction = "down") %>% 
# fix use.  No need to fix parameter just leave na as these don't roll up.  A na is an na.
  ungroup() %>% 
  group_by(WBID, Use) %>% 
  arrange(WBID, Use, DecisionUse) %>% 
  fill(DecisionUse, .direction = "down") %>% 
  ungroup() %>% 
  select(-WaterbodyName) %>% 
  left_join(YWATERBODYNAME, by = "WBID") %>% 
  rename(WaterbodyName = WATERBODY_NAME) %>% 
  left_join(j.core, by = c("WBID", "Use", "CharacteristicName", "ResultSampleFractionText")) %>% 
  arrange(WBID, Use, CharacteristicName) %>% 
  rename(Season = donotsampleseasons) %>% 
  mutate(c1 = ifelse(COREANDSEASON == "Yes", "Full Core and Seasonal Distribution Coverage", NA)) %>% 
  mutate(c2 = ifelse(COREANDSEASON == "No" & COREPARAMETER == "Y", "Insufficient Information - Missing Core Parameter Coverage and/or Seasonal Distribution", NA)) %>% 
  mutate(Comment = paste3(Comment, c1, c2)) %>% 
  replace_na(list(Comment = "No comment")) %>% 
  select(-c1, -c2) %>% 
  left_join(YWBHUCREACHSHORT2, by = "WBID") %>% 
  rename(SampleNeed = paramcoresampneed, WaterType = WATER_TYPE) %>% 
  select(WBID, WaterbodyName, WaterType, everything())

write.csv(j.explorer, "Appendix A - 2022 Assessment Decisions.csv", row.names = FALSE)

# Export for RTA Dashboard
write.csv(j.explorer, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZAPPA-DECISIONS.csv", row.names = FALSE)
write.csv(j.datagapgis5, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZDATAGAP.csv", row.names = FALSE)

# Meghan's volunteer metric.  Captures basically datagap5 with a date field filtered for just volunteers
# j.voldatagap <- j.orggather %>% 
#   ungroup() %>% 
#   select(WBID, CharacteristicName, ResultSampleFractionText, Use, provassess, Volunteer, Total, Org, Orgcode) %>% 
#   filter(Org == "Volunteer") %>% 
#   group_by(provassess) %>% 
#   summarise(sum = sum(Total)) %>% 
#   spread(provassess, sum) %>% 
#   mutate(paramdecision = `Meeting Criteria` + `Not Meeting Criteria`) %>% 
#   mutate(date = Sys.Date())

# Meghan's volunteer metric.  Captures basically datagap5 with a date field filtered for just volunteers
j.voldatagap <- j.orggather %>%
  ungroup() %>%
  select(WBID, CharacteristicName, ResultSampleFractionText, Use, provassess, Volunteer, Total, Org, Orgcode) %>%
  filter(Org == "Volunteer") %>%
  group_by(provassess) %>%
  summarise(sum = n()) %>%
  spread(provassess, sum) %>%
  mutate(paramdecision = `Meeting Criteria` + `Not Meeting Criteria`) %>%
  mutate(date = Sys.Date())

# Meg's metric but at the waterbody level.  First isolate the waterbody decisions.
j.wbidforvol <- j.explorer %>% 
  distinct(WBID, DecisionWBID)

# Then join to volunteer Data
j.volcurrent <- j.orggather %>%
  ungroup() %>%
  filter(Org == "Volunteer") %>% 
  distinct(WBID) %>% 
  left_join(j.wbidforvol, by = "WBID") %>% 
  count(DecisionWBID) %>% 
  spread(DecisionWBID, n) %>% 
  mutate(Date = Sys.Date())
  
# load previous r data
j.volprevious <- read_csv("J:/WQD/Surface Water Section/TMDL/Volunteer and Citizen Science/AWW Problem Solving LEAN/voldecisions.csv",
                         col_types = cols(
                           Date = col_date(format = "%Y-%m-%d"),
                           Attaining = col_double(),
                           Impaired = col_double(),
                           Inconclusive = col_double()))

j.voldecisions <- bind_rows(j.volcurrent, j.volprevious)

write.csv(j.voldecisions, "J:/WQD/Surface Water Section/TMDL/Volunteer and Citizen Science/AWW Problem Solving LEAN/voldecisions.csv", row.names = FALSE)

# Filter for just assigned sites
j.datagapassign <- j.datagapgis5 %>% 
  filter(type == "Assigned") %>% 
  select(WBID, WATERBODY_NAME, Champion, monitoringpriority, DatagapNumbersamples, DatagapSite, DatagapNeeds, SourceIDSites, SourceIDNeeds, PotentialDelist)

# Assigned joined to assessment report for metrics
j.assign <- j.datagapassign %>% 
  filter(!is.na(monitoringpriority)) %>% 
  left_join(j.reporta, by = "WBID")

# Record report date for dashboard
reportdate <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
write.csv(reportdate, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZREPORTDATE.csv", row.names = FALSE)

# Percent Assessed Metric
# Stream Universe is from perennial and intermittent streams in T:\data\adeq\water\hydrology\NHDStreams\Flow_Regimes.gdb.  Add P or I to definition query for flow.
# Lake Universe is from T:\data\adeq\water\Assessments\Appendix B waters.gdb
load(file = "inputs/ZUNIVERSE.RData")

j.universe <- nrow(ZUNIVERSE)

# Calculate percent assessed for waterbodies.  Assessed means either a attaining, inconclusive or impaired decision
j.percentassessed <- j.wbid %>%
  filter(WBIDAssess %in% c("Attaining", "Inconclusive", "Impaired")) %>% 
  summarise(CountAssessed = sum(WBID)) %>% 
  mutate(PercentAssessed = (CountAssessed / j.universe) *100) %>% 
  mutate(Date = Sys.Date()) %>% 
  mutate(Type = "Actual")

# Open previous data and append to the current decision
j.percentassessedfile <- read_csv("metrics/percentassessed.csv",
                                  col_types = cols(
                                    Date = col_date(format = "%Y-%m-%d"),
                                    CountAssessed = col_double(),
                                    PercentAssessed = col_double(),
                                    Type = col_character()))

j.percentassessedfile <- bind_rows(j.percentassessed, j.percentassessedfile)

write.csv(j.percentassessedfile, "metrics/percentassessed.csv", row.names = FALSE)

#  Aggregate to monthly
j.paagg <- j.percentassessedfile %>% 
  group_by(Type, aggdate = floor_date(Date, "1 week")) %>%
  summarize(aggpercentassessed = max(PercentAssessed)) %>% 
  filter(aggdate >= "2020-07-01")

# Make Monthly graph with goal = 26 + current level which is 374 = 400.  26 is used because 29 datagap samples in FY21 and expected that 25 can be visited
j.pagoal <- setNames(data.frame(c(40.0, 42.7), as.Date(c("2020-07-01", "2021-06-30")), c("Goal", "Goal")), c("aggpercentassessed", "aggdate", "Type"))

# Combine actual with goal.
j.pacomb <- bind_rows(j.paagg, j.pagoal)

# Graph
j.pacomb %>% 
  ggplot(aes(x = aggdate, y = aggpercentassessed, color = Type)) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  scale_x_date(date_breaks = "1 week") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = c("#5c896c", "grey")) +
  labs(title = "Percent of Important Waterbodies Assessed (LAG)", x = "", y = "% Assessed", caption = "Pulled Monthly by Jason Jones. SOP/Path = J:/WQD/Surface Water Section/Monitoring Unit/LEAN/RMetrics/percentassessed") +
  theme_light() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), legend.position = "bottom", legend.title = element_blank(),
        plot.caption = element_text(color = "grey"))

ggsave("percentassessed.jpeg", width = 8, height = 5, units = "in", dpi = 300)

# Number of Decisions

# Decision means making a attaining or impaired decision at the waterbody level
j.decisions <- i.wbid %>%
  filter(provassess %in% c(1, 3)) %>% 
  ungroup() %>% 
  summarise(CountDecision = n()) %>% 
  mutate(Date = Sys.Date()) %>% 
  mutate(Type = "Actual")

# Open previous data and append to the current decision
j.decisionsfile <- read_csv("metrics/decisions.csv",
                                  col_types = cols(
                                    Date = col_date(format = "%Y-%m-%d"),
                                    CountDecision = col_double(),
                                    Type = col_character()))

j.decisionsfile <- bind_rows(j.decisions, j.decisionsfile)

write.csv(j.decisionsfile, "metrics/decisions.csv", row.names = FALSE)

#  Aggregate to monthly
j.decagg <- j.decisionsfile %>% 
  group_by(Type, aggdate = floor_date(Date, "1 week")) %>%
  summarize(aggdecision = max(CountDecision)) %>% 
  filter(aggdate >= "2020-07-01")

# Make Monthly graph with goal = 26 + current level which is 374 = 400.  26 is used because 29 datagap samples in FY21 and expected that 25 can be visited
j.decdf <- setNames(data.frame(c(192, 222), as.Date(c("2020-07-01", "2021-06-30"))), c("aggdecision", "aggdate"))
j.decdf <- mutate(j.decdf, Type = "Goal")

# Combine actual with goal.
j.deccomb <- bind_rows(j.decagg, j.decdf)

# Graph
j.deccomb %>% 
  ggplot(aes(x = aggdate, y = aggdecision, color = Type)) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  scale_x_date(date_breaks = "1 week") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = c("#5c896c", "grey")) +
  labs(title = "Number of Decisions Made by Waterbody (LAG)", x = "", y = "Count of Decisions", caption = "Pulled Monthly by Jason Jones. SOP/Path = J:/WQD/Surface Water Section/Monitoring Unit/LEAN/RMetrics/decisions") +
  theme_light() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), legend.position = "bottom", legend.title = element_blank(),
        plot.caption = element_text(color = "grey"))

ggsave("decisions.jpeg", width = 8, height = 5, units = "in", dpi = 300)



#### K - QUALITY ASSURANCE ####



# How many parameter impairments in most recent parameter delist graph?
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

# As on 7/31/20 ELT wants draft WOTUS tracked as the key performance indicator.  
# The draft WOTUS metric takes over the old parameter delist graph and WQDB field.
# The ZQAPARAM is now the 'goto' document for provisional history.
# Pulls just the most recent decision at the parameter level
k.qaparam <- ZQAPARAM %>% 
  group_by(WBID, CharacteristicName) %>% 
  arrange(WBID, CharacteristicName, desc(Date)) %>% 
  distinct(WBID, CharacteristicName, .keep_all = TRUE) %>% 
  filter(Action == "Impaired")

# How many parameter impairments are identified by the tool?
k.param <- i.param %>% 
  filter(provassess == 3)

# Identify differences...both should be 0

# In human but not on delist graph
k.param.a <- k.param %>% 
  anti_join(k.qaparam, by = c("WBID", "CharacteristicName"))

# In delist graph but not human
k.param.b <- k.qaparam %>% 
  anti_join(k.param, by = c("WBID", "CharacteristicName"))

# Agreement between newassess and provassess in human file
k.agreementall <- human %>% 
  filter(newassess != provassess, is.na(provcomment))

# Logic to show user if QA passed.  Could configure this to actually prompt the user to fix the human file and then rerun a block of code 
run = nrow(k.param.a) + nrow(k.param.b)
if(run==0){
  print("QA Passed")
}else{
  print("Fix human file then rerun from read human to end")
}

end_time <- Sys.time()
end_time - start_time

#### Charts ####



# Impairment Causes by Parameter
human %>% 
  filter(provassess == 3) %>% 
  distinct(WBID, CharacteristicName) %>% # Resolves double counting due to use, fraction
  group_by(CharacteristicName) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = reorder(CharacteristicName, Count), y = Count)) +
    geom_col(alpha = 0.6, fill = "#c00000", color = "#c00000") +
    coord_flip() +
    labs(y = "Count", x = "") +
    scale_y_continuous(breaks = seq(0, 60, 10)) +
    theme_light() +
    theme(panel.grid.major.y = element_blank(), legend.position = "none")

# Dashboard Export - Impairment Causes

ZPLOTCAUSE <- human %>% 
  filter(provassess == 3) %>% 
  distinct(WBID, CharacteristicName) %>% # Resolves double counting due to use, fraction
  group_by(CharacteristicName) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))  

write.csv(ZPLOTCAUSE, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZPLOTCAUSE.csv", row.names = FALSE)

# Use Decisions
j.reporta %>%
  distinct(WBID, Use, DecisionUse) %>% 
  group_by(Use, DecisionUse) %>% 
  summarise(Count = n()) %>% 
  filter(DecisionUse %in% c("Insufficient information", "Not supporting", "Supporting")) %>% 
  ggplot(aes(x = reorder(Use, Count), y = Count, fill = DecisionUse, color = DecisionUse)) +
    geom_col(alpha = 0.6) +
    coord_flip() +
    labs(y = "Count", x = "") +
    scale_y_continuous(breaks = seq(0, 350, 50), expand = c(0.01, 0)) +
    scale_fill_manual(values = c("#ffc000", "#c00000", "#70ad47")) +
    scale_color_manual(values = c("#ffc000", "#c00000", "#70ad47")) +
    theme_light() +
    theme(panel.grid.major.y = element_blank(), legend.position = "bottom", legend.title = element_blank())

# Dashboard Export
ZPLOTUSE <- j.reporta %>%
  distinct(WBID, Use, DecisionUse) %>% 
  group_by(Use, DecisionUse) %>% 
  summarise(Count = n()) %>% 
  filter(DecisionUse %in% c("Insufficient information", "Not supporting", "Supporting"))  
  
write.csv(ZPLOTUSE, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZPLOTUSE.csv", row.names = FALSE)

# Organization Decisions
c.stddata2 %>% 
  mutate(Year = year(ActivityStartDate)) %>% 
  group_by(Year, ActivityConductingOrganizationText) %>% 
  summarise(Count = n()) %>% 
  filter(!is.na(ActivityConductingOrganizationText)) %>% 
  ggplot(aes(x = Year, y = Count, fill = factor(ActivityConductingOrganizationText, levels = c("Volunteer", "USGS", "Other", "Government", "ADEQ")), color = factor(ActivityConductingOrganizationText, levels = c("Volunteer", "USGS", "Other", "Government", "ADEQ")))) +
    geom_col(alpha = 0.6) +
    labs(y = "Number of Records", x = "") +
    scale_fill_manual(values = c("#70ad47", "#ed7d31", "#7030a0", "#c00000", "#4472c4")) +
    scale_color_manual(values = c("#70ad47", "#ed7d31", "#7030a0", "#c00000", "#4472c4")) +
    scale_y_continuous(breaks = seq(0, 60000, 10000), expand = c(0.01, 0)) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(), 
          legend.position = "bottom", 
          legend.title = element_blank())

# FISH ADVISORY CHECKER 
# Open connection object.
conn <- odbcConnect("com", uid="<username>", pwd="<password>")

# Check connection object is open.
odbcGetInfo(conn)

# Query the database and put the results into the data frame
FISH <- sqlQuery(conn,"select STATION_CD, STATION_ALT_NAME, ACTIVITY_END_DATE, FINALID, LAB_QA_FLAGS, SUBSTANCE_NAME, LAB_RESULT, LAB_RESULT_UNIT, DETECTION_LIMIT, DETECTION_LIMIT_UNIT from VW_FISH_QUERY", rows_at_time = 1,believeNRows = FALSE)

#Close connection object.
close(conn)

# Join to c.sites to show WBID
FISH <- FISH %>%
  left_join(c.sites, by = c("STATION_CD" = "SiteID"))

# Summarize Fish Results by Waterbody, Fish and Just Mercury...expand to more parameters if needed.
FISH2 <- FISH %>%
  filter(SUBSTANCE_NAME == "MERCURY") %>%
  mutate(STDDETLIM = ifelse(is.na(DETECTION_LIMIT), 0.012, DETECTION_LIMIT)) %>%
  mutate(STDRESULT = ifelse(is.na(LAB_RESULT), STDDETLIM/2, LAB_RESULT)) %>%
  select(WBID, STATION_ALT_NAME, ACTIVITY_END_DATE, FINALID, LAB_QA_FLAGS, SUBSTANCE_NAME, LAB_RESULT, LAB_RESULT_UNIT, STDRESULT, STDDETLIM, DETECTION_LIMIT, DETECTION_LIMIT_UNIT)

FISH2$FINALID <- as.character(FISH2$FINALID)
FISH2[grep("APACHE TROUT", FISH2$FINALID), "FINALID"] <- "Apache Trout"

FISH2 %>% 
  ungroup() %>% 
  group_by(FINALID) %>% 
  mutate(median = median(STDRESULT)) %>% 
  mutate(lowquantile = quantile(STDRESULT, probs = 0.25, type = 6)) %>% 
  mutate(count = n()) %>% 
  filter(count >= 10) %>% 
  ggplot(aes(x=reorder(FINALID, median, na.rm = TRUE), y = STDRESULT)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  geom_hline(aes(yintercept = 0.3), col = "red") + #adds a red line for the standard
  labs(y = "Mercury in Fish Tissue (mg/kg)", x = "", title = "Mercury in Arizona Fish by Species. Red Line = 0.3 mg/kg Standard.") +
  scale_y_continuous(breaks = seq(0, 4, 0.25), expand = c(0.01, 0)) +
  # facet_wrap(. ~ CurrentAdvisory)+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("fish.tiff", width = 6.5, height = 6, units = "in", dpi = 300)


# Do the advisory based on current criteria.  Need 5 or more fish.  Red = Do not eat, Orange = Limit Consumption, Green is Unlimited Consumption, Yellow is Inconclusive
FISHAdvisory <- FISH2 %>%
  group_by(WBID, FINALID) %>%
  summarise(count = n(), lastdate = max(ACTIVITY_END_DATE), max = max(STDRESULT), mean = mean(STDRESULT), sd = sd(STDRESULT), meanminussd = mean(STDRESULT)-sd(STDRESULT), meanplussd = mean(STDRESULT)+sd(STDRESULT)) %>%
  left_join(YWATERBODYNAME, by = "WBID") %>%
  rename(Species = FINALID) %>%
  mutate(automatedadvisory = ifelse(max > 2, "Red",
                           ifelse(count >= 5 & mean >= 0.75, "Red",
                                  ifelse(count >= 5 & mean - sd > 0.3, "Orange",
                                         ifelse(count >= 5 & mean + sd < 0.3, "Green", "Yellow")))))

# Load Existing Fish Advisories
ZFISHADVISORIES <- read_csv("inputs/ZFISHADVISORIES.csv", 
                            col_types = cols(Advice = col_character(), 
                                             Color = col_character(), `Date Issued` = col_date(format = "%m/%d/%Y"), 
                                             Parameter = col_character(), Species = col_character(), 
                                             WBID = col_character(), `Waterbody Name` = col_character()))

# Any Differences?
FISHDifferences <- FISHAdvisory %>%
  full_join(ZFISHADVISORIES, by = c("WBID", "Species"))

# # Output to Fish Folder
write.csv(FISHDifferences, paste0("J:/WQD/Surface Water Section/Monitoring Unit/Fish/Automated Fish Advisories R/FishAdvisories",Sys.Date(),".csv"))
write.csv(FISH2, "J:/WQD/Surface Water Section/Monitoring Unit/Fish/Automated Fish Advisories R/Fishdata.csv")



#### L - Extras ####

# Subset data for WOTUS

# WOTUS (google sheet). WBID level.  https://docs.google.com/spreadsheets/d/1oDB1BX_gENyCjOqErUtwIZvcol4d2gwR8Hz0ZLqIwMs/edit?ts=5f048ba9#gid=964163254

# Inputs.  Credentials will need to be entered at this point
load_data <- function() {
  read_sheet("1oDB1BX_gENyCjOqErUtwIZvcol4d2gwR8Hz0ZLqIwMs", "data")
}

l.wotus <- load_data()

# Clean.  
l.wotus <- l.wotus %>% 
  select(WBID, WOTUSSTATUS)

# Itemized wotus impaired parameters
l.wotusparam <- k.param %>% 
  left_join(l.wotus, by = "WBID") %>% 
  filter(WOTUSSTATUS == "WOTUS")

# Itemized wotus impaired waterbodies
l.wotuswb <- l.wotusparam %>% 
  distinct(WBID, WOTUSSTATUS)

# Export dataframes for rta dashboard value boxes
l.wotusparamvb <- nrow(l.wotusparam)
l.wotuswbvb <- nrow(l.wotuswb)

write.csv(l.wotusparamvb, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZWOTUSPARAMVB.csv", row.names = FALSE)

# Write the provisional numbers to the dashboard as well
l.provparamvb <- nrow(k.param)
l.provwbvb <- nrow(distinct(k.param, WBID)) 

write.csv(l.provparamvb, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZPROVPARAMVB.csv", row.names = FALSE)
write.csv(l.provwbvb, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZPROVWBVB.csv", row.names = FALSE)

# Join wotus with current impairments
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

l.provimpairments <- ZQAPARAM %>% 
  left_join(l.wotus, by = "WBID") %>% 
  select(WBID, NAME, CharacteristicName, Date, Stage, Action, WOTUSSTATUS, Current, Comment)

write.csv(l.provimpairments, "C:/Users/65942/Desktop/Assessment Dashboard/inputs/ZPROVIMPAIRMENTS.csv", row.names = FALSE)

# Key Performance Indicator Tracking

# Shorten Impairment Year for Join
ZIMPAIRMENTYEARSHORT <- ZIMPAIRMENTYEAR %>%
  mutate(ImpDate = as.Date(paste(PARAM_YEAR_LISTED, 1, 1, sep = "-"))) %>% 
  select(WBID, CharacteristicName, ImpDate) 

# Assess by Parameter.  JUST NEEDED FOR PERFORMANCE METRIC.  No need for sample fraction since  unique (can't have total and disolved in same use).
l.parambase <- human %>% 
  group_by(WBID, WATERBODY_NAME, CharacteristicName) %>%
  filter(!WATERBODY_NAME == "15030202-005A" | CharacteristicName == "MANGANESE") %>% # Temporary fix for ATTAINS ERROR
  mutate(newassess = max(newassess), provassess = max(provassess)) %>% 
  distinct(WBID, WATERBODY_NAME, CharacteristicName, provassess, provcomment, provdatetext) %>% 
  mutate(provdatetext = as.numeric(provdatetext)) %>%
  mutate(provdate = as.Date(provdatetext, origin = "1899-12-30")) %>% 
  left_join(ZIMPAIRMENTYEARSHORT, by = c("WBID", "CharacteristicName"))  

# Change NA to date so 
l.parambase$provdate[is.na(l.parambase$provdate)] <- as.Date("1900-01-01")
l.parambase$ImpDate[is.na(l.parambase$ImpDate)] <- as.Date("1900-01-01")

l.parambase <- l.parambase %>% 
  mutate(Date = if_else(provdate >= ImpDate, provdate, ImpDate)) %>% # using dplyr if_else because of date confusion with ifelse
  left_join(l.wotus, by = "WBID")

# Parameter Status with WOTUS
l.param <- l.parambase %>%
  # filter(provassess == 3, WOTUSSTATUS == "WOTUS") %>%
  distinct(WBID, CharacteristicName, .keep_all = TRUE) %>%   
  select(WBID, WATERBODY_NAME, CharacteristicName, Date, WOTUSSTATUS, provassess)

# Waterbody Status with WOTUS
l.waterbody <- l.param %>%
  ungroup() %>% 
  arrange(desc(provassess)) %>% 
  # filter(provassess == 3, WOTUSSTATUS == "WOTUS") %>% 
  distinct(WBID, .keep_all = TRUE) %>% 
  select(WBID, WATERBODY_NAME, Date, WOTUSSTATUS, provassess)

