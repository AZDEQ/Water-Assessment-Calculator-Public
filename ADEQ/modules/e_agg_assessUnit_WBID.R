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

# ZCORE shows all needed core parameters.  e.coregather has what was actually sampled.  The join shows the missing (have vs. need).  Core = na there is no core parameter for use.  Y means present.
e.coregather <- e.coregather %>% select(-NewUse) 
e.coregather <- left_join(e.coregather, ZCORE, by = c("CharacteristicName", "ResultSampleFractionText", "Use"))

# gsh change row
# orig - filter(!is.na(NUTRIENT)) %>% # nutrient standards present
# new - filter(NUTRIENT != '') %>% # nutrient standards present
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
# gsh edit to make code work on AWS
# e.season <- e.corecomp3
# e.season %>% drop_na(WBID)

e.season <- e.corecomp3 %>% 
  spread(season, sumhave) 

# e.season <- e.season %>% filter(!is.na("WBID"))
# gsh added this line to remove NA in WBID column
e.season <- filter(e.season, WBID != 'NA')

e.season[is.na(e.season)] <- 0.2

# Determines if at least one sample collected (.2 + .2 + .2 + .2 = .8 so not a keeper)
e.season <- e.season %>% 
  mutate(keepers = spring + summer + fall + winter) %>% 
  filter(keepers >= 1.1)

# gsh mutate columns to be char
e.season <- mutate(e.season,
                   fall=as.character(fall),
                   spring=as.character(spring),
                   winter=as.character(winter),
                   summer=as.character(summer))

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
# gsh - changed to character
# r.nowbid$WBID <- as.numeric(r.nowbid$WBID)
r.nowbid$WBID <- as.character(r.nowbid$WBID)
r.nowbid$ActivityDepthHeightMeasure.MeasureValue <- as.numeric(r.nowbid$ActivityDepthHeightMeasure.MeasureValue)

# gsh - changed to character
# r.tribal$WBID <- as.numeric(r.tribal$WBID)
r.tribal$WBID <- as.character(r.tribal$WBID)
r.tribal$ActivityDepthHeightMeasure.MeasureValue <- as.numeric(r.tribal$ActivityDepthHeightMeasure.MeasureValue)

# gsh - changed to character
# r.unit$WBID <- as.numeric(r.unit$WBID)
r.unit$WBID <- as.character(r.unit$WBID)

r.unit$ActivityDepthHeightMeasure.MeasureValue <- as.numeric(r.unit$ActivityDepthHeightMeasure.MeasureValue)
r.all <- bind_rows(r.nodetect, r.duplicated, r.improve, r.nfl, r.notcredible, r.oxygenmgl, r.oxygenper, r.usgsnotcredible, r.nowbid, r.qc, r.tribal, r.unit)

