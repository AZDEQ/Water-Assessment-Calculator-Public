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
  select(r.select)
# select(all_of(r.select))
# above add (all_of()) to r.select as per warning message
# gsh change back due to error - removed all_of

# Exclude Tribal Data
b.azdata <- b.azdata %>% 
  filter(!OrganizationIdentifier %in% c("AK-CHIN_WQX", "COCOPAH_INDIAN", "CRITEPO_WQX", "FMYN_WQX", "HOPI_WQX", "KBOPWQP", "QUECHAN_WQX", "SRPMIC", "SRPMIC_WQX", "WHITEMOUNTAIN_WQX", "WMAT_WQX", "YAN_WQX"))

