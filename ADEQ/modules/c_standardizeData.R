#### C - STANDARDIZE DATA ####

# Import Standard Unit table

# Join STDUNIT to azdata.  This standardizes units and resolves issues with speciation
c.stddata <- left_join(b.azdata, ZSTDUNIT, by = "ResultMeasure.MeasureUnitCode")

# Removed Records Documentation
r.unit <- c.stddata %>% 
  filter(STDUNIT == "remove") %>% 
  mutate(WBID = NA) %>% 
  mutate(removereason = "Unit") %>% 
  select(r.select)

# Remove any data with STDUNIT = remove.  Some parameters like TKN are in mg/kg.  This removes those units
c.stddata <- filter(c.stddata, STDUNIT != "remove" | is.na(STDUNIT))

# Remove any sediment data not picked up by unit filter
c.stddata <- filter(c.stddata, ActivityMediaName == "Water" | ActivityMediaName == "Other")

# Apply Conversion.  This standardizes results to common units.
c.stddata <- mutate(c.stddata, STDResult = ResultMeasureValue * Conversion)



# # Connect to ADEQ Water Quality Database *** Don't share ***
# conn <- odbcConnect("com", uid="<username>", pwd="<password>")
# 
# # Check connection object is open.
# odbcGetInfo(conn)
# 
# # Query the database and put the results into the data frame
# YSTA_STATION <- sqlQuery(conn,"select * from STA_STATION", rows_at_time = 1,believeNRows = FALSE)
# YSW_SITES <- sqlQuery(conn,"select * from VW_SW_SITES", rows_at_time = 1,believeNRows = FALSE)
# YUSGS_SITES <- sqlQuery(conn,"select * from VW_USGS_WATERBODY_HUC_REACH", rows_at_time = 1,believeNRows = FALSE)
# YWBHUCREACH <- sqlQuery(conn,"select * from VW_WATERBODY_HUC_REACH", rows_at_time = 1,believeNRows = FALSE) # going to use this later for lake acres and stream miles also uses
# 
# write.csv(YSTA_STATION, "inputs/YSTA_STATION.csv", row.names = FALSE)
# write.csv(YSW_SITES, "inputs/YSW_SITES.csv", row.names = FALSE)
# write.csv(YUSGS_SITES, "inputs/YUSGS_SITES.csv", row.names = FALSE)
# write.csv(YWHUCREACH <- "inputs/YWBHUCREACH.csv", row.names = FALSE)
# 
# #Close connection object.
# close(conn)


# Get current site information from the ADEQ Water Quality Database through ODBC.

# Standardize WBID
# YWBHUCREACH <- YWBHUCREACH %>% 
#   mutate(WBIDCOPY = WBID) %>% 
#   separate(WBIDCOPY, c("c", "d", "e"), sep = "([\\Z\\L])") %>% 
#   mutate(WBID = ifelse(d == "", e, d)) %>% 
#   select(-c, -d, -e) %>% 
#   separate(WATERBODY_NAME, c("WATERBODY_NAME"), sep = "-") %>% 
#   mutate(ORIGIN = as.character(ORIGIN)) %>% 
#   mutate(TERMINUS = as.character(TERMINUS))

# YWBHUCREACH$WBID <- sub("^[AZL]","",substring(YWBHUCREACH$WBID,3))
# YWBHUCREACH <- YWBHUCREACH %>% 
#   separate(WATERBODY_NAME, c("WATERBODY_NAME"), sep = "-") %>% 
#   mutate(ORIGIN = as.character(ORIGIN)) %>% 
#   mutate(TERMINUS = as.character(TERMINUS))

# Alternate code for the above - gsh 11/9/2020
YWBHUCREACH <- mutate(YWBHUCREACH, WBIDCOPY = WBID)
YWBHUCREACH$WBIDCOPY <- sub("AZL","",YWBHUCREACH$WBIDCOPY)
YWBHUCREACH$WBIDCOPY <- sub("AZ","",YWBHUCREACH$WBIDCOPY)
YWBHUCREACH <- YWBHUCREACH %>% 
  mutate(WBID = WBIDCOPY) %>% 
  separate(WATERBODY_NAME, c("WATERBODY_NAME"), sep = "-") %>% 
  mutate(ORIGIN = as.character(ORIGIN)) %>% 
  mutate(TERMINUS = as.character(TERMINUS)) %>%
  select(-WBIDCOPY)
# end alternate code


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
# orig c.stddata$concate <- paste(c.stddata$WBID, c.stddata$CharacteristicName, c.stddata$ResultSampleFractionText, c.stddata$ActivityStartDate, c.stddata$ActivityStartTime.Time, c.stddata$ActivityDepthHeightMeasure.MeasureValue, c.stddata$ResultMeasureValue, c.stddata$ActivityTypeCode, c.stddata$MonitoringLocationIdentifier)
c.stddata$concate <- paste(c.stddata$CharacteristicName, c.stddata$ResultSampleFractionText, c.stddata$ActivityStartDate, c.stddata$ActivityStartTime.Time, c.stddata$ActivityDepthHeightMeasure.MeasureValue, c.stddata$ResultMeasureValue, c.stddata$ActivityTypeCode, c.stddata$MonitoringLocationIdentifier)


# gsh run to duplicate code

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
# after this c.stdata does not match


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
c.stddata[grep("Recoverable", c.stddata$ResultSampleFractionText), "ResultSampleFractionText"] <- "Total" 
c.stddata[grep("Total Recoverable", c.stddata$ResultSampleFractionText), "ResultSampleFractionText"] <- "Total" 

### Temporary Code.  Remove once USGS adds detection limits back to the portal ###
if(usgsTemp == 1){
  #modules/t_tempCode.R
  source("modules/t_tempCode.R")
} 
# end Temporary Code

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

# Free up memory
rm(b.azdata, c.stddata.p1, c.stddata.p2, dfLong, dfLong22, dfLong222, a.azdata2012to2014, a.azdata2014to2016)
gc()