#### A - EPA data loaded into s3 in loadConfigData_CSV.R ####

# Sample Data
# Read USGS sample data

a.usgs <- read_csv("s3/z_epa_usgs.csv",
                   col_types = cols(OrganizationIdentifier = col_character(), 
                                    OrganizationFormalName = col_character(), 
                                    ActivityIdentifier = col_character(), 
                                    ActivityTypeCode = col_character(),
                                    ActivityMediaName = col_character(),
                                    ActivityMediaSubdivisionName = col_skip(),
                                    ActivityStartDate = col_date(format = "%Y-%m-%d"), 
                                    ActivityStartTime.Time = col_character(),
                                    ActivityEndTime.TimeZoneCode = col_skip(),
                                    ActivityDepthHeightMeasure.MeasureValue = col_double(),
                                    ActivityDepthHeightMeasure.MeasureUnitCode = col_character(),
                                    ActivityDepthAltitudeReferencePointText = col_skip(),
                                    ActivityTopDepthHeightMeasure.MeasureValue = col_skip(),
                                    ActivityTopDepthHeightMeasure.MeasureUnitCode = col_skip(),
                                    ActivityBottomDepthHeightMeasure.MeasureValue = col_skip(),
                                    ActivityBottomDepthHeightMeasure.MeasureUnitCode = col_skip(),
                                    ProjectIdentifier = col_skip(),
                                    ActivityConductingOrganizationText = col_character(),
                                    MonitoringLocationIdentifier = col_character(),
                                    ActivityCommentText = col_character(),
                                    HydrologicCondition = col_character(),
                                    HydrologicEvent = col_skip(),
                                    SampleCollectionMethod.MethodIdentifier = col_skip(),
                                    SampleCollectionMethod.MethodIdentifierContext = col_skip(),
                                    SampleCollectionMethod.MethodName = col_skip(),
                                    SampleCollectionEquipmentName = col_skip(),
                                    ResultDetectionConditionText = col_character(),
                                    CharacteristicName = col_character(),
                                    ResultSampleFractionText = col_character(),
                                    ResultMeasureValue = col_double(),
                                    ResultMeasure.MeasureUnitCode = col_character(),
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
                                    ResultDepthHeightMeasure.MeasureValue = col_skip(),
                                    ResultDepthHeightMeasure.MeasureUnitCode = col_skip(),
                                    ResultDepthAltitudeReferencePointText = col_skip(),
                                    SubjectTaxonomicName = col_skip(),
                                    SampleTissueAnatomyName = col_skip(),
                                    ResultAnalyticalMethod.MethodIdentifier = col_character(),
                                    ResultAnalyticalMethod.MethodIdentifierContext= col_character(),
                                    ResultAnalyticalMethod.MethodName = col_character(),
                                    MethodDescriptionText = col_character(),
                                    LaboratoryName = col_skip(),
                                    AnalysisStartDate = col_skip(),
                                    ResultLaboratoryCommentText = col_skip(),
                                    DetectionQuantitationLimitTypeName = col_character(),
                                    DetectionQuantitationLimitMeasure.MeasureValue = col_double(),
                                    DetectionQuantitationLimitMeasure.MeasureUnitCode = col_character(),
                                    PreparationStartDate = col_skip(),
                                    ProviderName = col_skip(),
                                    SampleAquifer = col_skip(),
                                    ResultTimeBasisText = col_skip()))


# Read AZ Sample Data
a.azstream <- read_csv("s3/z_epa_azstream.csv",
                       col_types = cols(OrganizationIdentifier = col_character(), 
                                        OrganizationFormalName = col_character(), 
                                        ActivityIdentifier = col_character(), 
                                        ActivityTypeCode = col_character(),
                                        ActivityMediaName = col_character(),
                                        ActivityMediaSubdivisionName = col_skip(),
                                        ActivityStartDate = col_date(format = "%Y-%m-%d"), 
                                        ActivityStartTime.Time = col_character(),
                                        ActivityEndTime.TimeZoneCode = col_skip(),
                                        ActivityDepthHeightMeasure.MeasureValue = col_double(),
                                        ActivityDepthHeightMeasure.MeasureUnitCode = col_character(),
                                        ActivityDepthAltitudeReferencePointText = col_skip(),
                                        ActivityTopDepthHeightMeasure.MeasureValue = col_skip(),
                                        ActivityTopDepthHeightMeasure.MeasureUnitCode = col_skip(),
                                        ActivityBottomDepthHeightMeasure.MeasureValue = col_skip(),
                                        ActivityBottomDepthHeightMeasure.MeasureUnitCode = col_skip(),
                                        ProjectIdentifier = col_skip(),
                                        ActivityConductingOrganizationText = col_character(),
                                        MonitoringLocationIdentifier = col_character(),
                                        ActivityCommentText = col_character(),
                                        HydrologicCondition = col_character(),
                                        HydrologicEvent = col_skip(),
                                        SampleCollectionMethod.MethodIdentifier = col_skip(),
                                        SampleCollectionMethod.MethodIdentifierContext = col_skip(),
                                        SampleCollectionMethod.MethodName = col_skip(),
                                        SampleCollectionEquipmentName = col_skip(),
                                        ResultDetectionConditionText = col_character(),
                                        CharacteristicName = col_character(),
                                        ResultSampleFractionText = col_character(),
                                        ResultMeasureValue = col_double(),
                                        ResultMeasure.MeasureUnitCode = col_character(),
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
                                        ResultDepthHeightMeasure.MeasureValue = col_skip(),
                                        ResultDepthHeightMeasure.MeasureUnitCode = col_skip(),
                                        ResultDepthAltitudeReferencePointText = col_skip(),
                                        SubjectTaxonomicName = col_skip(),
                                        SampleTissueAnatomyName = col_skip(),
                                        ResultAnalyticalMethod.MethodIdentifier = col_character(),
                                        ResultAnalyticalMethod.MethodIdentifierContext= col_character(),
                                        ResultAnalyticalMethod.MethodName = col_character(),
                                        MethodDescriptionText = col_character(),
                                        LaboratoryName = col_skip(),
                                        AnalysisStartDate = col_skip(),
                                        ResultLaboratoryCommentText = col_skip(),
                                        DetectionQuantitationLimitTypeName = col_character(),
                                        DetectionQuantitationLimitMeasure.MeasureValue = col_double(),
                                        DetectionQuantitationLimitMeasure.MeasureUnitCode = col_character(),
                                        PreparationStartDate = col_skip(),
                                        ProviderName = col_skip(),
                                        SampleAquifer = col_skip(),
                                        ResultTimeBasisText = col_skip()))
# Bind rows
a.azdata_current <- rbind(a.azstream, a.usgs)
# Remove X1
if("X1" %in% colnames(a.azdata_current)){a.azdata_current <- select(a.azdata_current,-X1)}


# Read AZ Sites from csv
a.azsites_current <- read_csv("s3/z_epa_azsites.csv",
                           col_types = cols(MonitoringLocationIdentifier = col_character(), 
                                            OrganizationIdentifier = col_character(),
                                            MonitoringLocationTypeName = col_character(),
                                            LatitudeMeasure = col_double(),
                                            LongitudeMeasure = col_double()))

a.azsites_current <- a.azsites_current %>% 
  select(MonitoringLocationIdentifier, OrganizationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure)


# Read USGS sites from csv
a.usgssites <- read_csv("s3/z_epa_usgssites.csv",
                        col_types = cols(MonitoringLocationIdentifier = col_character(), 
                                         OrganizationIdentifier = col_character(),
                                         MonitoringLocationTypeName = col_character(),
                                         LatitudeMeasure = col_double(),
                                         LongitudeMeasure = col_double()))

a.usgssites <- a.usgssites %>% 
  select(MonitoringLocationIdentifier, OrganizationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure)

a.azsites_current <- bind_rows(a.azsites_current, a.usgssites)

# before binding to the historic data, filter on dates greater than or equal to 7/1/2016
a.azdata_current <- a.azdata_current %>%
  filter(ActivityStartDate >= as.Date("2016-07-01"))


# bind 2022 to 2014-2016 and 2012-2014
a.azdata <- rbind(a.azdata_current, a.azdata2014to2016,a.azdata2012to2014)
a.azsites <- rbind(a.azsites_current,a.azsites2014to2016,a.azsites2012to2014)

# Remove X1 field from a.azdata
if("X1" %in% colnames(a.azdata)){a.azdata <- select(a.azdata,-X1)}

# Backups for testing
save(a.azdata, file = "inputs/ZAZDATA.Rdata")
save(a.azsites, file = "inputs/ZAZSITES.Rdata")

