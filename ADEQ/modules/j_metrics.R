#### J - REPORTS AND METRICS ####


# Contribution by agency

# Focus on just decisions made.  Decisions happen on multiple levels.  Start with Meeting/Not meeting criteria at the parameter level.  Excludes existing impairments that are not delist
j.newimpaired <- human %>% 
  filter(existimpair == "New Impairment") %>% 
  filter(provassess == 3) %>% 
  distinct(WBID, WATERBODY_NAME, CharacteristicName) %>% 
  mutate(New = "2022")

j.newdelists <- human %>% 
  filter(existimpair == "Existing Impairment") %>% 
  filter(Assessed == "Meeting criteria") %>% 
  filter(provassess == 2) %>% 
  distinct(WBID, WATERBODY_NAME, CharacteristicName)

c.stddata2[grep("AZDEQ_WPD|ARIZONA DEPT OF ENVIRONMENTAL QUALITY|HARRIS ENVIRONMENTAL|AZDEQ_SW", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "ADEQ"
c.stddata2[grep("U.S. Geological Survey-Water Resources Discipline|USGS-NV|USGS-AZ|USGS-UT|USGS - Arizona Water Science Center|U.S. Geological Survey|U.S. GEOLOGICAL SURVEY", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "USGS"
c.stddata2[grep("OAK CREEK WATERSHED IMPROVEMENT COUNCIL|PRESCOTT CREEKS|FRIENDS OF THE SANTA CRUZ RIVER|CORONADO RESOURCE|BUTTE CREEK RESTORATION COUNCIL|FRIEND OF THE FOREST|VERDE RIVER INSTITUTE|SIERRA CLUB|ARAVAIPA GROUP|VOLUNTEER GROUPS|BCRC|SIER|FOF|OAK CREEK WATERSHED COUNCIL|GILA WATERSHED PARTNERSHIP|FRIENDS OF THE TONTO", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "Volunteer"
c.stddata2[grep("USEPA_REGION8|U.S. FISH|U.S. FOREST|TEMPE|National Park Service|NATIONAL PARK SERVICE|PIMA COUNTY WASTEWATER MANAGEMENT DEPT|ARIZONA GAME AND FISH|CITY OF TUCSON|ARIZONA STATE PARKS|AGFD|U.S. National Park Service|11NPSWRD_WQX|SLIDE ROCK STATE PARK|Bureau of Reclamation|Bureau of Land Management", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "Government"
c.stddata2[grep("ALLIED SIGNAL ENGINES|UNIVERSITY OF ARIZONA|HARGIS & ASSOC. INC.|CAPSTONE MINING|SALT RIVER PROJECT|PINAL CREEK GROUP|INTERNATIONAL BOUNDARY AND WATER COMMISSION|BHP|ASARCO|GOLDER AND ASSOCIATES, INC.|RESOLUTION COPPER|WALKER ECOLOGICAL SERVICES|UNIVERSITY OF ARIZONA - MARICOPA AGRICULTURAL CENTER", c.stddata2$ActivityConductingOrganizationText), "ActivityConductingOrganizationText"] <- "Other"

# Shiny Dashboard
save(c.stddata2, file = "shinyDashboardFiles/inputs/ZCDATA.Rdata")

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

# gsh - mutate to character due to error - 'provessess' can't be mofified becaue it's a grouping variable
j.orggather <- mutate(j.orggather,
                      provassess=as.character(provassess),
                      Orgcode=as.character(Orgcode))


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

# gsh - mutate to character
j.parameter <- mutate(j.parameter,
                     ParameterAssess=as.character(ParameterAssess))
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

#gsh - mutate to character
j.wbid <- mutate(j.wbid,
                WBIDAssess=as.character(WBIDAssess))

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

# gsh mutate DecisionsWBID to char due to error in conversion
j.report <- mutate(j.report,
                   DecisionWBID=as.character(DecisionWBID))

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
  mutate(CharacteristicNameNew = ifelse(New == "2022", paste0(CharacteristicName, " - New in ", New), CharacteristicName))

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
  distinct(WBID, WATERBODY_NAME, Use, provassess, .keep_all = TRUE) %>% 
  spread(Use, provassess) %>% 
  left_join(j.epa4and5, by = "WBID")

j.epacat <- j.epacat %>% 
  mutate(maxcat = pmax(AGI, AGL, AWC, AWE, AWEDW, AWW, DWS, FBC, FC, PBC, na.rm = TRUE)) %>% 
  mutate(mincat = pmin(AGI, AGL, AWC, AWE, AWEDW, AWW, DWS, FBC, FC, PBC, na.rm = TRUE)) %>% 
  replace_na(list("maxcat" = 0, "mincat" = 99999)) %>% # cannot put 0 for mincat or next step would pull 0 
  group_by(WBID) %>% 
  mutate(maxcat2 = max(maxcat)) %>% 
  mutate(mincat2 = min(mincat)) %>% 
  mutate(newepacat = ifelse(maxcat2 == 2 & mincat2 == 2, "1 Supporting All Uses",
                            ifelse(maxcat2 == 2 & mincat2 == 1, "2 Supporting Some Uses", "3 Inconclusive")))

# Overrule based on cat 4 & 5.  Keeping in mind that only santa cruz is a 4B
j.epacat[grep("5", j.epacat$EPACategory), "newepacat"] <- "5 Impaired"
j.epacat[grep("4", j.epacat$EPACategory), "newepacat"] <- "4A Not Attaining TMDL Complete"

# Clean up and narrow to one observation to one WBID
j.epacat <- j.epacat %>% 
  left_join(YWBHUCREACHSHORT, by = "WBID") %>% 
  distinct(WBID, WATERBODY_NAME, newepacat, WATER_TYPE, ASSESSED) 

# GIS file splitting out lakes and streams
j.gisepacat <- j.epacat %>% 
  mutate(GISWBID = paste0(ifelse(WATER_TYPE == "Lake", "AZL", "AZ"), WBID)) 

# Streams
j.gisepacatstream <- j.gisepacat %>% 
  filter(WATER_TYPE == "Stream")

# Lakes
j.gisepacatlake <- j.gisepacat %>% 
  filter(WATER_TYPE == "Lake")

# gsh write
write.csv(j.gisepacatstream, "outputs/2022assessmentGISstream.csv", row.names = FALSE)
# csvTo_s3(j.gisepacatstream,s3Bucket,"2022assessmentGISstream.csv","outputs")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"2022assessmentGISstream",j.gisepacatstream, overwrite = TRUE)
}
write.csv(j.gisepacatlake, "outputs/2022assessmentGISlake.csv", row.names = FALSE)
# csvTo_s3(j.gisepacatlake,s3Bucket,"2022assessmentGISlake.csv","outputs")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"2022assessmentGISlake",j.gisepacatlake, overwrite = TRUE)
}

# Add epacategory to report
j.epacatshort <- j.epacat %>% 
  ungroup() %>% 
  select(WBID, newepacat) %>% 
  rename(EPACategory = newepacat)

# gsh - error EPACategory doesn't exist

# j.reportGH <- j.report %>% 
#   left_join(j.epacatshort, by = "WBID")
# j.reportGH %>%
#   rename(
#     EPACategory = EPACategory.x
#   )

# j.report <- j.reportGH %>% 
#   left_join(j.epacatshort, by = "WBID") %>% 
#   select(WBID, WaterbodyName, AcuteChronic, Use, CharacteristicName, ResultSampleFractionText, Binomial, NumberCriteriaMet, NumberCriteriaNotMet, TotalSamples, ImpairmentType, Comment, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse, DecisionWBID, EPACategory, WATERSHED, REACH_DISTANCE, LAKE_ACRES, ORIGIN, TERMINUS)

j.report <- j.report %>%
  left_join(j.epacatshort, by = "WBID") %>%
  distinct(WBID, WaterbodyName, AcuteChronic, Use, CharacteristicName, ResultSampleFractionText, Binomial, NumberCriteriaMet, NumberCriteriaNotMet, TotalSamples, ImpairmentType, Comment, paramcarryforward, usecarryforward, DecisionParameter, DecisionUse, DecisionWBID, EPACategory, WATERSHED, REACH_DISTANCE, LAKE_ACRES, ORIGIN, TERMINUS)



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


write.csv(j.reportexceedance, "Appendix B - 2022- Assessment Exceedances.csv", row.names = FALSE)
csvTo_s3(j.reportexceedance,s3Bucket,"Appendix B - 2022- Assessment Exceedances.csv","none")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"Appendix_B_2022_Assessment_Exceedances",j.reportexceedance, overwrite = TRUE)
}

write.csv(j.impairedlist, "Appendix D - 2022 Impaired Waters List.csv", row.names = FALSE)
csvTo_s3(j.impairedlist,s3Bucket,"Appendix D - 2022 Impaired Waters List.csv","none")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"Appendix_D_2022_Impaired_Waters_List",j.impairedlist, overwrite = TRUE)
}

write.csv(j.critcond, "Appendix C - 2022 Critical Conditions.csv", row.names = FALSE)
csvTo_s3(j.critcond,s3Bucket,"Appendix C - 2022 Critical Conditions.csv","none")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"Appendix_C_2022_Critical_Conditions",j.critcond, overwrite = TRUE)
}

write.csv(j.priority, "Appendix E - 2022 TMDL Priority Ranking.csv", row.names = FALSE)
csvTo_s3(j.priority,s3Bucket,"Appendix E - 2022 TMDL Priority Ranking.csv","none")
if(user.writeDB == "y"){
  dbWriteTable(conDb,"Appendix_E_2022_TMDL_Priority_Ranking",j.priority, overwrite = TRUE)
}

# Shiny Dashboard
write.csv(j.reportexceedance, "shinyDashboardFiles/inputs/ZAPPB-EXCEEDANCE.csv", row.names = FALSE)
write.csv(j.critcond, "shinyDashboardFiles/inputs/ZAPPC-CRITICALCOND.csv", row.names = FALSE)

# WBID Acre/Miles and Count
j.wbidsum <- j.report %>% 
  mutate(WATER_TYPE = ifelse(is.na(REACH_DISTANCE), "LAKE", "STREAM"))

# gsh - run to here
j.wbidsum$REACH_DISTANCE <- as.numeric(j.wbidsum$REACH_DISTANCE)

# gsh error AcuteChronic
# look at just changing NA to char 0 

# gsh - replaced the following to acomodate error 
# Error occurred for column `AcuteChronic`.
# ✖ Can't convert <double> to <character>.
# j.wbidsum[is.na(j.wbidsum)] <- 0
# replaced with individual field conversion from NA to 0
j.wbidsum$AcuteChronic[is.na(j.wbidsum$AcuteChronic)] <- '0'
j.wbidsum$NumberCriteriaMet[is.na(j.wbidsum$NumberCriteriaMet)] <- 0
j.wbidsum$NumberCriteriaNotMet[is.na(j.wbidsum$NumberCriteriaNotMet)] <- 0
j.wbidsum$REACH_DISTANCE[is.na(j.wbidsum$REACH_DISTANCE)] <- 0
j.wbidsum$LAKE_ACRES[is.na(j.wbidsum$LAKE_ACRES)] <- 0
j.wbidsum$TotalSamples[is.na(j.wbidsum$TotalSamples)] <- 0


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

# gsh same na conversiont error with AcuteChronic
# j.wbidsumcat[is.na(j.wbidsumcat)] <- 0
# gsh replaced with
j.wbidsumcat$AcuteChronic[is.na(j.wbidsumcat$AcuteChronic)] <- '0'
j.wbidsumcat$NumberCriteriaMet[is.na(j.wbidsumcat$NumberCriteriaMet)] <- 0
j.wbidsumcat$NumberCriteriaNotMet[is.na(j.wbidsumcat$NumberCriteriaNotMet)] <- 0
j.wbidsumcat$TotalSamples[is.na(j.wbidsumcat$TotalSamples)] <- 0
j.wbidsumcat$REACH_DISTANCE[is.na(j.wbidsumcat$REACH_DISTANCE)] <- 0
j.wbidsumcat$LAKE_ACRES[is.na(j.wbidsumcat$LAKE_ACRES)] <- 0


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

# gsh na issue again - adjusted below
# j.datagap[is.na(j.datagap)] <- 0
# j.datagap[is.na(j.datagap)] <- '0'  -Use this to find out what columns are numeric 
# and need changed
j.datagap$WBID[is.na(j.datagap$WBID)] <- '0'
j.datagap$No[is.na(j.datagap$No)] <- 0
j.datagap$Yes[is.na(j.datagap$Yes)] <- 0
j.datagap$sampcount[is.na(j.datagap$sampcount)] <- 0
j.datagap$newassess[is.na(j.datagap$newassess)] <- 0
j.datagap$provassess[is.na(j.datagap$provassess)] <- 0
j.datagap$paramsampneed[is.na(j.datagap$paramsampneed)] <- 0
j.datagap$coresampneed[is.na(j.datagap$coresampneed)] <- 0

# gsh run to here

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

# gsh good to here
# all the following greps 'can't convert char to double'
# mutate data type
j.datagap <- mutate(j.datagap,
                    provassess=as.character(provassess),
                    autoassess=as.character(autoassess))

j.datagap[grep(0, j.datagap$provassess), "provassess"] <- "Not assessed"
j.datagap[grep(1, j.datagap$provassess), "provassess"] <- "Not enough information"
j.datagap[grep(2, j.datagap$provassess), "provassess"] <- "Meeting criteria"
j.datagap[grep(3, j.datagap$provassess), "provassess"] <- "Not meeting criteria"
j.datagap[grep(0, j.datagap$autoassess), "autoassess"] <- "Not assessed"
j.datagap[grep(1, j.datagap$autoassess), "autoassess"] <- "Not enough information"
j.datagap[grep(2, j.datagap$autoassess), "autoassess"] <- "Meeting criteria"
j.datagap[grep(3, j.datagap$autoassess), "autoassess"] <- "Not meeting criteria"

# Open Cost

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

j.datagapgis2 <- left_join(j.datagapgis, ZNORMALIZEDSITES, by = "WBID")

# Add directions from WQDB
YSW_SITESshort <- YSW_SITES %>% 
  select(WBID, STATION_ACCESS) %>% 
  distinct(WBID, .keep_all = TRUE)

# Import datagap ranking and ranking info

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

# Datagap Input file with decisions and champions/lead samplers

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

# For Shiny Dashboard.  Combines appendix a decisions with what is missing and identifies core parameters and if core and seasonal distribution met
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
if(user.writeDB == "y"){
  dbWriteTable(conDb,"Appendix_A_2022_Assessment_Decisions",j.explorer, overwrite = TRUE)
  }

# Shiny Dashboard
write.csv(j.explorer, "shinyDashboardFiles/inputs/ZAPPA-DECISIONS.csv", row.names = FALSE)

# Shiny Dashboard
write.csv(j.datagapgis5, "shinyDashboardFiles/inputs/ZDATAGAP.csv", row.names = FALSE)

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
# j.volprevious <- read_csv("J:/WQD/Surface Water Section/TMDL/Volunteer and Citizen Science/AWW Problem Solving LEAN/voldecisions.csv",
#                           col_types = cols(
#                             Date = col_date(format = "%Y-%m-%d"),
#                             Attaining = col_double(),
#                             Impaired = col_double(),
#                             Inconclusive = col_double()))

# j.volprevious load is now in loadConfigData_CSV.R
# j.volprevious <- read_csv("jdrive/voldecisions.csv",
#                           col_types = cols(
#                             Date = col_date(format = "%Y-%m-%d"),
#                             Attaining = col_double(),
#                             Impaired = col_double(),
#                             Inconclusive = col_double()))

j.voldecisions <- bind_rows(j.volcurrent, j.volprevious)

# write.csv(j.voldecisions, "J:/WQD/Surface Water Section/TMDL/Volunteer and Citizen Science/AWW Problem Solving LEAN/voldecisions.csv", row.names = FALSE)

csvTo_s3(j.voldecisions,s3_csv_bucket,"voldecisions.csv","inputs")
# dbWriteTable(conDb,"voldecisions",j.voldecisions, overwrite = TRUE)
# gsh write

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

# Shiny Dashboard
write.csv(reportdate, "shinyDashboardFiles/inputs/ZREPORTDATE.csv", row.names = FALSE)


# Percent Assessed Metric

# Stream Universe is from perennial and intermittent streams in T:\data\adeq\water\hydrology\NHDStreams\Flow_Regimes.gdb.  Add P or I to definition query for flow.
# Lake Universe is from T:\data\adeq\water\Assessments\Appendix B waters.gdb
#ZUNIVERSE <- read_csv("inputs/ZUNIVERSE.csv")

j.universe <- nrow(ZUNIVERSE)

# Calculate percent assessed for waterbodies.  Assessed means either a attaining, inconclusive or impaired decision
j.percentassessed <- j.wbid %>%
  filter(WBIDAssess %in% c("Attaining", "Inconclusive", "Impaired")) %>% 
  summarise(CountAssessed = sum(WBID)) %>% 
  mutate(PercentAssessed = (CountAssessed / j.universe) *100) %>% 
  mutate(Date = Sys.Date()) %>% 
  mutate(Type = "Actual")

# Open previous data and append to the current decision
# gsh - read from database in load
# j.percentassessedfile <- read_csv("metrics/percentassessed.csv",
#                                   col_types = cols(Date = col_date(format = "%Y-%m-%d")))

j.percentassessedfile <- bind_rows(j.percentassessed, j.percentassessedfile)

# gsh write to db
write.csv(j.percentassessedfile, "metrics/percentassessed.csv", row.names = FALSE)
if(user.writeDB == "y"){
  dbWriteTable(conDb,"percentassessedfile",j.percentassessedfile, overwrite = TRUE)
}

#  Aggregate to monthly
j.paagg <- j.percentassessedfile %>% 
  group_by(Type, aggdate = floor_date(Date, "1 month")) %>%
  summarize(aggpercentassessed = max(PercentAssessed)) %>% 
  filter(aggdate >= "2020-07-01")

# Make Monthly graph with goal = 26 + current level which is 374 = 400.  26 is used because 29 datagap samples in FY21 and expected that 25 can be visited
j.pagoal <- setNames(data.frame(c(40.0, 42.7), as.Date(c("2020-07-01", "2021-06-30")), c("Goal", "Goal")), c("aggpercentassessed", "aggdate", "Type"))

# gsh error object 'j.pa' not found
# Combine actual with goal.
j.pacomb <- bind_rows(j.paagg, j.pagoal)

# Graph
j.pacomb %>% 
  ggplot(aes(x = aggdate, y = aggpercentassessed, color = Type)) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
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

# gsh - open from database on load
# Open previous data and append to the current decision
# j.decisionsfile <- read_csv("metrics/decisions.csv",
#                                   col_types = cols(Date = col_date(format = "%Y-%m-%d")))

j.decisionsfile <- bind_rows(j.decisions, j.decisionsfile)

# gsh write to db
write.csv(j.decisionsfile, "metrics/decisions.csv", row.names = FALSE)
if(user.writeDB == "y"){
  dbWriteTable(conDb,"decisionsfile",j.decisionsfile, overwrite = TRUE)
}

#  Aggregate to monthly
j.decagg <- j.decisionsfile %>% 
  group_by(Type, aggdate = floor_date(Date, "1 month")) %>%
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
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = c("#5c896c", "grey")) +
  labs(title = "Number of Decisions Made by Waterbody (LAG)", x = "", y = "Count of Decisions", caption = "Pulled Monthly by Jason Jones. SOP/Path = J:/WQD/Surface Water Section/Monitoring Unit/LEAN/RMetrics/decisions") +
  theme_light() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), legend.position = "bottom", legend.title = element_blank(),
        plot.caption = element_text(color = "grey"))

ggsave("decisions.jpeg", width = 8, height = 5, units = "in", dpi = 300)

