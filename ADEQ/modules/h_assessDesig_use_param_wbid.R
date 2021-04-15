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

# Load parameter mapping

# Crosswalk use names to ATTAINS
XATTAINSUSE <- setNames(data.frame(
  c("Agricultural Irrigation", "Full Body Contact", "Agricultural Livestock Watering", "Aquatic and Wildlife (Warmwater Fishery)", "Fish Consumption", "Partial Body Contact", "Aquatic and Wildlife (Ephemeral)", "Aquatic and Wildlife (Effluent Dependent Water)", "Aquatic and Wildlife (Coldwater Fishery)", "Domestic Water Source"), 
  c("AGI", "FBC", "AGL", "AWW", "FC", "PBC", "AWE", "AWEDW", "AWC", "DWS")), 
  c("PARAM_USE_NAME","Use"))

# Most recent ATTAINS parameter file. Add WBID, Characteristic name mapping and use mapping
# Clean up WBID

# ATTAINSPARAMETERS <- ATTAINSPARAMETERS %>% 
#   mutate(ASSESSMENT_UNIT_IDCOPY = ASSESSMENT_UNIT_ID) %>% 
#   separate(ASSESSMENT_UNIT_IDCOPY, c("a", "b"), sep = "_") %>% 
#   separate(a, c("c", "d", "e"), sep = "([\\Z\\L])") %>% 
#   mutate(WBID = ifelse(is.na(e), d, e)) %>% 
#   select(ASSESSMENT_UNIT_ID, WBID, PARAM_NAME, PARAM_USE_NAME, PARAM_STATUS_NAME, PARAM_ATTAINMENT_CODE) %>% 
#   filter(PARAM_STATUS_NAME != "Removed") %>% 
#   left_join(XATTAINSUSE, by = c("PARAM_USE_NAME")) %>% 
#   left_join(ZATTAINSPARAMETERMAP, by = "PARAM_NAME")

# Add correction for split reaches or nutrient values
ATTAINSPARAMETERS <- bind_rows(ATTAINSPARAMETERS, ATTAINSSPLIT)

# alternate code for the above - gsh - 11/9/2020
ATTAINSPARAMETERS <- mutate(ATTAINSPARAMETERS, ASSESSMENT_UNIT_IDCOPY = ASSESSMENT_UNIT_ID)
ATTAINSPARAMETERS$ASSESSMENT_UNIT_IDCOPY <- sub("AZL","",ATTAINSPARAMETERS$ASSESSMENT_UNIT_IDCOPY)
ATTAINSPARAMETERS$ASSESSMENT_UNIT_IDCOPY <- sub("AZ","",ATTAINSPARAMETERS$ASSESSMENT_UNIT_IDCOPY)
ATTAINSPARAMETERS <- ATTAINSPARAMETERS %>%                           
  separate(ASSESSMENT_UNIT_IDCOPY, c("a", "b"), sep = "_") %>% 
  mutate(WBID = a) %>% 
  select(ASSESSMENT_UNIT_ID, WBID, PARAM_NAME, PARAM_USE_NAME, PARAM_STATUS_NAME, PARAM_ATTAINMENT_CODE) %>% 
  filter(PARAM_STATUS_NAME != "Removed") %>% 
  left_join(XATTAINSUSE, by = c("PARAM_USE_NAME")) %>% 
  left_join(ZATTAINSPARAMETERMAP, by = "PARAM_NAME")  
# end alternate code

# Need to reduce h.assessall for fraction and chronic/acute so direct comparison to ATTAINs can be made
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

# gsh ran successfully to here

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

# gsh ran successfully to here

# gsh error in next - Error in select(-newassess) : object 'newassess' not found

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
# ZCRITICALCONDITION <- read_csv("inputs/ZCRITICALCONDITION.csv", col_types = cols(
#  WBID = col_character(),
#  WATERBODY_NAME = col_character(),
#  criticalcondition = col_character(), 
#  criticallocation = col_character())) 

# Identify changes since last run. 8/10/20 update makes this a separate QA query.
# Add field that tells specialist if something changed since the last run (REEVALUATE).
# load(file = "inputs/ZLASTDB.Rdata")

# Identifies what is different in the automated data from last run
h.diff <- h.assessall %>% mutate(source = "new")

# gsh ran successfully to here

# Identify what was different since last run.
h.diff$concate <- paste(h.diff$WBID, h.diff$Use, h.diff$CharacteristicName, h.diff$Assessed, h.diff$No, h.diff$Yes, h.diff$sampcount)
# Error on next, ZLASTDB not found - the .Rdata file opens and loads as h.assessall, so changed
# h.lastdb <- ZLASTDB %>% mutate(source = "old")
h.lastdb <- h.assessall %>% mutate(source = "old")
h.lastdb$concate <- paste(h.lastdb$WBID, h.lastdb$Use, h.lastdb$CharacteristicName, h.lastdb$Assessed, h.lastdb$No, h.lastdb$Yes, h.lastdb$sampcount)
h.reevaluate <- bind_rows(h.lastdb, h.diff)
h.reevaluate <- h.reevaluate %>%
  mutate(dbdifferent = duplicated(concate)) %>% # will say true which is opposite of meaning but this will be fixed later
  filter(source == "new") %>%
  select(WBID, WATERBODY_NAME, NewUse, Use, CharacteristicName, ResultSampleFractionText, Assessed, binomial, No, Yes, sampcount, actualsampneed, existimpair, paramcarryforward, dbdifferent,newassess) %>% 
  filter(dbdifferent == "FALSE") # False means need to reevaluate
view(h.reevaluate)

# Opens human file.  Careful of NA's on top this will change the data type.
# Original location of reading in the csv

# Make a backup
#file.copy("human.csv", paste("humancopies/humancopy", format(Sys.Date(), "%Y-%m-%d"), "csv", sep = "."))
csvTo_s3(human,s3Bucket,paste("humancopy", format(Sys.Date(), "%Y-%m-%d"), "csv", sep = "."),"humancopies")
# gsh write

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

# Write the new human file with the new automated data and database check
write.csv(human, "human.csv")
# write to s3
csvTo_s3(human,s3_csv_bucket,"human.csv","inputs")

if(user.writeDB == "y"){
  dbWriteTable(conDb,"human",human, overwrite = TRUE)
}

# Write the last database run.  IMPORTANT.  CHANGES TO DATABASE ONLY CAPTURED ONCE SO SPECIALIST SHOULDN'T RUN THIS PART OF THE CODE UNLESS THEY HAVE TIME TO REEVALUATE
# write.csv(h.assessall, "inputs/ZLASTDB.csv")
csvTo_s3(h.assessall,s3Bucket,"ZLASTDB.csv","inputs")
# gsh write
# dbWriteTable(conDb,"ZLASTDB",h.assessall, overwrite = TRUE)

