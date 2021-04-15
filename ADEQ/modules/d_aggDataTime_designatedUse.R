#### D - AGGREGATION BY TIME / ITERATION BY DESIGNATED USE ####

# Import current designated uses.  Note this is an ODBC connection.

# Join STDdata to uses by WBID
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

# Data missing from d.usejoin at this point

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

# gsh 'detect' is missing from d.dlagg run to here

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

# Free up memory
rm(d.timemean, d.timemax, d.timemin, d.timemeanagg, d.timemaxagg, d.timeminagg, d.usejoin2, d.usejoin3)
gc()
