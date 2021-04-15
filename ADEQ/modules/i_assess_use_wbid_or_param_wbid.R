#### I - ASSESS BY USE AND WATERBODY or PARAMETER AND WATERBODY ####


# Create ATTAINS Assessment Unit Files.  location.csv, water_type.csv and assessment_units.csv

# Identify Retired Reaches
i.retired <- c("15070102-334", "15020001-011", "15030204-003", "15050100-12B", "15050100-12A")

# Remove from human File (this is not save to the permanent file but won't be saved to EPA ATTAINS files)
human <- human %>% 
  filter(!(WBID %in% i.retired)) %>% 
  filter(CharacteristicName != "ECOLIGEO")

# Load what is different from ATTAINS vs. Human file

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

# Output csv's
# gsh write
write.csv(ATTAINSWATER_TYPE, "ATTAINS/water_type.csv", row.names = FALSE)
csvTo_s3(ATTAINSWATER_TYPE,s3Bucket,"water_type.csv","ATTAINS")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"water_type",ATTAINSWATER_TYPE, overwrite = TRUE)
}

write.csv(ATTAINSLOCATION, "ATTAINS/location.csv", row.names = FALSE)
csvTo_s3(ATTAINSLOCATION,s3Bucket,"location.csv","ATTAINS")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"location",ATTAINSLOCATION, overwrite = TRUE)
}

# write.csv(ATTAINSASSESSMENT_UNITS, "outputs/assessment_units.csv", row.names = FALSE)
# csvTo_s3(ATTAINSASSESSMENT_UNITS,s3Bucket,"assessment_units.csv","outputs")

# User needs to update any new TMDLs

# ATTAINS Assessments.  Similar to the assessment units but bundled with parameters, uses, associated-actions and sources.


# Get Trophic Status

# Map EPA assessment unit to WBID
# Clean up WBID

# ZTROPHIC <- ZTROPHIC %>% 
#   mutate(TEMPWBID = ASSESSMENT_UNIT_ID) %>% 
#   separate(TEMPWBID, c("a", "b"), sep = "_") %>% 
#   separate(a, c("c", "d", "e"), sep = "([\\Z\\L])") %>% 
#   rename(WBID = e) %>% 
#   filter(!is.na(TROPHIC_STATUS)) %>% 
#   select(WBID, TROPHIC_STATUS)

# alternate code for the above - gsh - 11/9/2020
ZTROPHIC <- mutate(ZTROPHIC, TEMPWBID = ASSESSMENT_UNIT_ID)
ZTROPHIC$TEMPWBID <- sub("AZL","",ZTROPHIC$TEMPWBID)
ZTROPHIC$TEMPWBID <- sub("AZ","",ZTROPHIC$TEMPWBID)
ZTROPHIC <- ZTROPHIC %>% 
  separate(TEMPWBID, c("a", "b"), sep = "_") %>% 
  mutate(WBID = a) %>% 
  filter(!is.na(TROPHIC_STATUS)) %>% 
  select(WBID, TROPHIC_STATUS)
# end alternate code


# na to ""
# ZASSESSMENTHIST <- ZASSESSMENTHIST %>% 
#   mutate(TEMPWBID = WBID) %>% 
#   separate(TEMPWBID, c("c", "d", "e"), sep = "([\\Z\\L])")
# 
# ZASSESSMENTHIST$e[is.na(ZASSESSMENTHIST$e)] <- ""
# ZASSESSMENTHIST$d[is.na(ZASSESSMENTHIST$d)] <- ""
# 
# ZASSESSMENTHIST <- ZASSESSMENTHIST %>% 
#   mutate(TEMPWBID2 = paste0(d,e)) %>% 
#   rename(XXWBID = WBID) %>% 
#   rename(WBID = TEMPWBID2) %>% 
#   select(WBID, AY_1998, AY_2000, AY_2002, AY_2004, AY_2006_08, AY_2010, AY_2012_14, AY_2016, AY_2018, d, e)
# 
# ZASSESSMENTHIST[is.na(ZASSESSMENTHIST)] <-0

# alternate code to the above ZASSESSMENTHIST attempt to remove the leading AZL from WBID
# and reduce na to ""
# make a backup
# ZASSESSMENTHIST_back <- ZASSESSMENTHIST
ZASSESSMENTHIST <- mutate(ZASSESSMENTHIST, TEMPWBID = WBID)
ZASSESSMENTHIST$TEMPWBID <- sub("AZL","",ZASSESSMENTHIST$TEMPWBID)
ZASSESSMENTHIST$TEMPWBID <- sub("AZ","",ZASSESSMENTHIST$TEMPWBID)
ZASSESSMENTHIST <- ZASSESSMENTHIST %>% 
  mutate(WBID = TEMPWBID) %>%
  select(WBID, AY_1998, AY_2000, AY_2002, AY_2004, AY_2006_08, AY_2010, AY_2012_14, AY_2016, AY_2018)

ZASSESSMENTHIST[is.na(ZASSESSMENTHIST)] <-0

# What assessment units assessed?  Link in Trophic status (YWBHUCREACH) and last year assessed (MATS).  
i.assessments <- human %>% 
  distinct(WBID) %>% 
  left_join(ZTROPHIC, by = "WBID") %>% 
  left_join(ZASSESSMENTHIST, by = "WBID") %>% 
  mutate(CYCLE_LAST_ASSESSED = ifelse(is.na(AY_2018), "2022", pmax(AY_1998, AY_2000, AY_2002, AY_2004, AY_2006_08, AY_2010, AY_2012_14, AY_2016, AY_2018))) %>% # max applies all argument values. pmax just one row. 
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

write.csv(i.assessments, "ATTAINS/General.csv")
csvTo_s3(i.assessments,s3Bucket,"General.csv","ATTAINS")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"attains_general",i.assessments, overwrite = TRUE)
}

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
# save(i.param, file = "J:/WQD/Surface Water Section/LEAN/Restoration Potential/inputs/paramimpair.Rdata")
  save(i.param, file = "jdrive/paramimpair.Rdata")

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

# Save for Potential Restoration Tool
#save(i.paramuse, file = "J:/WQD/Surface Water Section/LEAN/Restoration Potential/inputs/ZIPARAMUSE.RData")
save(i.param, file = "jdrive/ZIPARAMUSE.Rdata")
# Import Previous ATTAINS Data.  Assessment specialist to work with water quality improvement grant / TMDL manager to get updated priorities.

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

i.attainsparamuse <- i.attainsparamuse %>% 
  left_join(ZPARAMYEAR, by = c("ASSESSMENT_UNIT_ID", "PARAM_NAME", "PARAM_USE_NAME", "PARAM_STATUS_NAME", "PARAM_ATTAINMENT_CODE")) %>% 
  mutate(temp = ifelse(is.na(NEWYEAR), PARAM_YEAR_LISTED, NEWYEAR)) %>% 
  select(-PARAM_YEAR_LISTED) %>% 
  rename(PARAM_YEAR_LISTED = temp) %>% 
  select(ASSESSMENT_UNIT_ID,	PARAM_NAME,	PARAM_USE_NAME,	PARAM_STATUS_NAME,	PARAM_ATTAINMENT_CODE,	PARAM_TREND, PARAM_COMMENT,	PARAM_AGENCY_CODE,	PARAM_POLLUTANT_INDICATOR,	PARAM_YEAR_LISTED,	PARAM_TARGET_TMDL_DATE,	PARAM_EXPECTED_TO_ATTAIN,	PARAM_PRIORITY_RANKING,	PARAM_CONSENT_DECREE_CYCLE,	PARAM_ALT_LISTING_ID,	PARAM_STATE_IR_CAT,	PARAM_ORG_QUALIFIER_FLAG,	PARAM_DELISTING_REASON,	PARAM_DELISTING_COMMENT,	PARAM_DELISTING_AGENCY)

# Save for Upload to ATTAINS
write.csv(i.attainsparamuse, "ATTAINS/Parameters.csv")
csvTo_s3(i.attainsparamuse,s3Bucket,"Parameters.csv","ATTAINS")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"attains_parameters",i.attainsparamuse, overwrite = TRUE)
}

# Join human to core/season
i.human <- human %>% 
  left_join(ZCORE, by = c("Use", "CharacteristicName", "ResultSampleFractionText"))

# Open Latest ATTAINS USE FILE

# Add in WBID and Use
# ATTAINSUSES <- ATTAINSUSES %>%
#   mutate(ASSESSMENT_UNIT_IDCOPY = ASSESSMENT_UNIT_ID) %>%
#   separate(ASSESSMENT_UNIT_IDCOPY, c("a", "b"), sep = "_") %>%
#   separate(a, c("c", "d", "e"), sep = "([\\Z\\L])") %>%
#   mutate(WBID = ifelse(is.na(e), d, e)) %>%
#   left_join(XATTAINSUSE, by = c("USE_NAME" = "PARAM_USE_NAME"))

# alternate code for the above - gsh - 11/9/2020
ATTAINSUSES <- mutate(ATTAINSUSES, ASSESSMENT_UNIT_IDCOPY = ASSESSMENT_UNIT_ID) 
ATTAINSUSES$ASSESSMENT_UNIT_IDCOPY <- sub("AZL","",ATTAINSUSES$ASSESSMENT_UNIT_IDCOPY)
ATTAINSUSES$ASSESSMENT_UNIT_IDCOPY <- sub("AZ","",ATTAINSUSES$ASSESSMENT_UNIT_IDCOPY)
ATTAINSUSES <- ATTAINSUSES %>% 
  separate(ASSESSMENT_UNIT_IDCOPY, c("a", "b"), sep = "_") %>%
  mutate(WBID = a) %>% 
  left_join(XATTAINSUSE, by = c("USE_NAME" = "PARAM_USE_NAME"))
# end alternate code

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

# # gsh testing - break apart the code chunk below
# i.use <- i.use %>%
#   rename(DEQUSEDECISION = USE_ATTAINMENT_CODE)
# 
# i.use <- i.use %>%
#   full_join(ATTAINSUSES, by = c("WBID", "Use")) %>%
#   mutate(combineduse = ifelse(DEQUSEDECISION == "F" & USE_ATTAINMENT_CODE == "I", DEQUSEDECISION,
#                               ifelse(DEQUSEDECISION == "I" & is.na(USE_ATTAINMENT_CODE), DEQUSEDECISION,
#                                      ifelse(is.na(DEQUSEDECISION), USE_ATTAINMENT_CODE,
#                                             ifelse(DEQUSEDECISION == "N", DEQUSEDECISION, USE_ATTAINMENT_CODE))))) %>%
#   mutate(combineduse = ifelse(is.na(USE_ATTAINMENT_CODE), DEQUSEDECISION, DEQUSEDECISION)) %>%
#   mutate(combineduse = replace(combineduse, is.na(DEQUSEDECISION), USE_ATTAINMENT_CODE)) %>%
#   mutate(EPAUSEDECISION = USE_ATTAINMENT_CODE) %>%
#   select(-USE_ATTAINMENT_CODE) %>%
#   rename(USE_ATTAINMENT_CODE = combineduse) %>% 
#   mutate(filtered = ifelse(DEQUSEDECISION == "I" & EPAUSEDECISION == "F", "Y", 
#                            ifelse(DEQUSEDECISION == "X" & EPAUSEDECISION == "F", "Y", 
#                                   ifelse(DEQUSEDECISION == "I" & EPAUSEDECISION == "N", "Y", "NO")))) %>% 
#   mutate(filtered = replace(filtered, is.na(filtered), "NO"))
# 
# # end gsh testing


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
write.csv(i.attainsuse, "ATTAINS/Uses.csv")
csvTo_s3(i.attainsuse,s3Bucket,"Uses.csv","ATTAINS")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"attains_uses",i.attainsuse, overwrite = TRUE)
}

# ATTAINS carries over all previous assessments.  In the 2022 version of the code combine this next step with the previous use file.

# Identify ATTAINS copy/paste from last assessment that are not category 4/5 and make one use 'not assessed'

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
write.csv(i.attainsactions, "ATTAINS/associated-actions.csv")
csvTo_s3(i.attainsactions,s3Bucket,"associated-actions.csv","ATTAINS")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"attains_associated_actions",i.attainsactions, overwrite = TRUE)
}

# ATTAINS Sources


# Look for the NA's in Source and fill in using domain list from https://www.epa.gov/waterdata/attains.  If there is more than one source then copy and paste the row information.

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