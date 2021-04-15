##### - Alternate code to pull EPA data directly from EPA and not from the scheduled AWS s3 bucket #####

#### A - ESTABLISH LIVE CONNECTION WITH WATER QUALITY PORTAL AND GET RAW DATA ####


# Pick date range
# gh changed as per Jason -  7/1/16 to 5/1/21?
#startDate <- as.Date("2016-07-01")
#endDate <- as.Date("2021-05-01")

startDate <- "07-01-2016"
endDate <- "05-01-2021"

# Static file for testing...remove if want live data
#load(file = "inputs/ZAZDATA.Rdata") # note that this opens as a.azdata
#load(file = "inputs/ZAZSITES.Rdata") # note that this opens as a.azsites

# USGS Colorado River Results

# Create temp file
temp <- tempfile()

a.usgssite_list <- "&siteid=USGS-355241114401601&siteid=USGS-09429600&siteid=USGS-344459114313801&siteid=USGS-09424000&siteid=USGS-324343114364901&siteid=USGS-330131114364101&siteid=USGS-332203114421201&siteid=USGS-332604114373601&siteid=USGS-09429100&siteid=USGS-333038114340401&siteid=USGS-335832111425401&siteid=USGS-09421500&siteid=USGS-09423000&siteid=USGS-09423500&siteid=USGS-09423550&siteid=USGS-09423560&siteid=USGS-09427520&siteid=USGS-09429490&siteid=USGS-09429500&siteid=USGS-09522000&siteid=USGS-09521100"

# Download zip
url <- paste0("https://www.waterqualitydata.us/data/Result/search?", a.usgssite_list, "&startDateLo=", startDate,"&startDateHi=", endDate, "&mimeType=csv&zip=yes")
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
url <- paste0("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A04&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Stream&startDateLo=", startDate,"&startDateHi=", endDate, "&mimeType=csv&zip=yes")
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
a.azdata_current <- rbind(a.azstream, a.usgs)

# Rename columns with / to .
a.azdata_current <- a.azdata_current %>% 
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
url <- paste0("https://www.waterqualitydata.us/data/Station/search?statecode=US%3A04&startDateLo=", startDate,"&startDateHi=", endDate, "&mimeType=csv&zip=yes")
download.file(url ,temp, mode="wb")

# Extract and put in data frame
a.azsites_current <- read_csv(unz(temp, "station.csv"),
                      col_types = cols(MonitoringLocationIdentifier = col_character(), 
                                       OrganizationIdentifier = col_character(),
                                       MonitoringLocationTypeName = col_character(),
                                       LatitudeMeasure = col_double(),
                                       LongitudeMeasure = col_double()))

a.azsites_current <- a.azsites_current %>% 
  select(MonitoringLocationIdentifier, OrganizationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure)

unlink(temp)

# Download USGS Sites.

# Create temp file
temp <- tempfile()

# Download zip
url <- paste0("https://www.waterqualitydata.us/data/Station/search?", a.usgssite_list, "&startDateLo=", startDate,"&startDateHi=", endDate, "&mimeType=csv&zip=yes")
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

# Combine a.azsites with usgssites.
a.azsites_current <- bind_rows(a.azsites_current, a.usgssites)

# before binding to the historic data, filter on dates greater than or equal to 7/1/2016
a.azdata_current <- a.azdata_current %>%
  filter(ActivityStartDate >= as.Date("2016-07-01"))

# bind 2022 to 2014-2016 and 2012-2014
a.azdata <- rbind(a.azdata_current, a.azdata2014to2016,a.azdata2012to2014)
a.azsites <- rbind(a.azsites_current,a.azsites2014to2016,a.azsites2012to2014)

# Remove X1 field from a.azdata
# a.azdata <- select(a.azdata, -X1)
if("X1" %in% colnames(a.azdata)){a.azdata <- select(a.azdata,-X1)}

# Backups for testing
save(a.azdata, file = "inputs/ZAZDATA.Rdata")
save(a.azsites, file = "inputs/ZAZSITES.Rdata")

