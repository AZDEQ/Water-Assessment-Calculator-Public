#### L - Extras ####
# gs4_deauth()
# Subset data for WOTUS

# jdj note to gh 2/1/21.  Not sure how you want to handle the if statement now that there are multiple WOTUS sheets.

# gh 2/5. Add if statement to skip Google sheet if 'flag' set.  Will then read from CSV

# # add pull wotus from CSV vs Google Sheets
#  if(user.useWotusCSV == "y"){
#    l.wotus <- wotusnewCSV %>% 
#      select(WBID, WOTUSSTATUS) %>% 
#      mutate(WOTUSSTATUS = ifelse(WOTUSSTATUS == "Likely WOTUS", "WOTUS",
#                                  ifelse(WOTUSSTATUS == "Likely NON-WOTUS", "NON-WOTUS", WOTUSSTATUS))) %>% 
#      filter(WOTUSSTATUS %in% c("WOTUS", "NON-WOTUS")) %>% 
#      rename(NEWWOTUSSTATUS = WOTUSSTATUS)
#    
#    l.wotus <- l.wotus %>% 
#      group_by(WBID) %>% 
#      distinct(WBID, NEWWOTUSSTATUS, .keep_all = TRUE)
#    
#    l.wotusold <- select(wotusoldCSV, WBID, OLDWOTUSSTATUS = WOTUSSTATUS)
#    
#    # Prioritize new WOTUS.  If new WOTUS missing then add old WOTUS
#    l.wotus <- l.wotus %>% 
#      full_join(l.wotusold, by = "WBID") %>% 
#      mutate(WOTUSSTATUS = ifelse(is.na(NEWWOTUSSTATUS), OLDWOTUSSTATUS, NEWWOTUSSTATUS)) 
#    
#  } else {
#   # use google sheets
#    # This is the new WOTUS sheet with higher confidence
#    load_data <- function() {
#      read_sheet("1UZDoDavnnCt5ry8Do65uZKucP4S4l1qXTicWxIPA6gg", "1-Waterbodies")
#    }
#    
#    l.wotus <- load_data()
#    
#    # Resolve to WOTUSSTATUS and WBID
#    l.wotus <- l.wotus %>% 
#      select(WBID = `Waterbody ID`, WOTUSSTATUS) %>% 
#      mutate(WOTUSSTATUS = ifelse(WOTUSSTATUS == "Likely WOTUS", "WOTUS",
#                                  ifelse(WOTUSSTATUS == "Likely NON-WOTUS", "NON-WOTUS", WOTUSSTATUS))) %>% 
#      filter(WOTUSSTATUS %in% c("WOTUS", "NON-WOTUS")) %>% 
#      rename(NEWWOTUSSTATUS = WOTUSSTATUS)
#    
#    l.wotus <- l.wotus %>% 
#      group_by(WBID) %>% 
#      distinct(WBID, NEWWOTUSSTATUS, .keep_all = TRUE)
#    
#    # WOTUS (google sheet). WBID level.  https://docs.google.com/spreadsheets/d/1oDB1BX_gENyCjOqErUtwIZvcol4d2gwR8Hz0ZLqIwMs/edit?ts=5f048ba9#gid=964163254
#    # This WOTUS sheet is best professional judgement (low quality) only
#    # Credentials will need to be entered at this point
#    # Note this is temporary and will be removed once all
#    load_data <- function() {
#      read_sheet("1oDB1BX_gENyCjOqErUtwIZvcol4d2gwR8Hz0ZLqIwMs", "data")
#    }
#    
#    l.wotusold <- load_data()
#    
#    l.wotusold <- select(l.wotusold, WBID, OLDWOTUSSTATUS = WOTUSSTATUS)
#    
#    # Prioritize new WOTUS.  If new WOTUS missing then add old WOTUS
#    l.wotus <- l.wotus %>% 
#      full_join(l.wotusold, by = "WBID") %>% 
#      mutate(WOTUSSTATUS = ifelse(is.na(NEWWOTUSSTATUS), OLDWOTUSSTATUS, NEWWOTUSSTATUS)) 
#    
#    
#  }
# 
# # end pull wotus
# 

# 
# # Just select needed fields
# l.wotus <- l.wotus %>% 
#   select(WBID, WOTUSSTATUS)
# 
# 
# # if the user flag is set to use the mySQL database 
# if(user.writeDB == "y"){
# # new code to load l.wotus from mySQL
# l.wotus <- dbReadTable(conDb, "wotusImpaired")
# }

# gh moved to loadConfigData_CSV.R
# Load recent WOTUS determinations
# ZWOTUS <- read_csv("inputs/ZWOTUS.csv")

# Clean.  
l.wotus <- ZWOTUS %>% 
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

write.csv(l.wotusparamvb, "shinyDashboardFiles/inputs/ZWOTUSPARAMVB.csv", row.names = FALSE)
write.csv(l.wotuswbvb, "shinyDashboardFiles/inputs/ZWOTUSWBVB.csv", row.names = FALSE)

# gh 2/25 - put this in loacConfigData_CSV.R
# This is temporary until the WOTUS folks finish their review by 4/20/21
# Intended to see what was added since the last iteration.  Needs to be able to add WBID's each week without losing any.
# ZLASTWOTUS <- read_csv("inputs/ZLASTWOTUS.csv")

# LASTWOTUS <- ZLASTWOTUS$WBID
# 
# # List of waters that have flipped and will now be impaired (again).
# k.wotus <- phs %>% 
#   filter(!is.na(NEWWOTUSSTATUS)) %>% 
#   filter(Impaired == "Yes") %>% 
#   mutate(same = ifelse(OLDWOTUSSTATUS == NEWWOTUSSTATUS, "same", "different")) %>% 
#   filter(same == "different" | is.na(same)) %>% 
#   left_join(YWATERBODYNAME, by = "WBID")
# 
# # This is what is new...now just figure out how to keep adding.
# k.wotusfilter <- k.wotus %>% 
#   filter(!WBID %in% LASTWOTUS)
# 
# 
# write.csv(phs, "WOTUS_IMPAIRED_BPJ_SCREENING.csv", row.names = FALSE)
# write.csv(k.wotus, "inputs/ZLASTWOTUS.csv", row.names = FALSE)

# Write the provisional numbers to the dashboard as well
l.provparamvb <- nrow(k.param)
l.provwbvb <- nrow(distinct(k.param, WBID)) 

write.csv(l.provparamvb, "shinyDashboardFiles/inputs/ZPROVPARAMVB.csv", row.names = FALSE)
write.csv(l.provwbvb, "shinyDashboardFiles/inputs/ZPROVWBVB.csv", row.names = FALSE)

# Join wotus with current impairments
# ZQAPARAM <- read_csv("inputs/ZQAPARAM.csv", 
#                      col_types = cols(
#                        WBID = col_character(),
#                        NAME = col_character(),
#                        CharacteristicName = col_character(),
#                        Date = col_date(format = "%m/%d/%Y"),
#                        Action = col_character(),
#                        Stage = col_character(),
#                        Comment = col_character(),
#                        Current = col_character()))

l.provimpairments <- ZQAPARAM %>% 
  left_join(l.wotus, by = "WBID") %>% 
  select(WBID, NAME, CharacteristicName, Date, Stage, Action, WOTUSSTATUS, Current, Comment)

write.csv(l.provimpairments, "shinyDashboardFiles/inputs/ZPROVIMPAIRMENTS.csv", row.names = FALSE)

# Key Performance Indicator Tracking

# Shorten Impairment Year for Join
ZIMPAIRMENTYEARSHORT <- ZIMPAIRMENTYEAR %>%
  mutate(ImpDate = as.Date(paste(PARAM_YEAR_LISTED, 1, 1, sep = "-"))) %>% 
  select(WBID, CharacteristicName, ImpDate) 
ZIMPAIRMENTYEARSHORT

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
l.waterbody <- l.parambase %>%
  ungroup() %>% 
  # filter(provassess == 3, WOTUSSTATUS == "WOTUS") %>%
  arrange(desc(provassess)) %>% 
  distinct(WBID, .keep_all = TRUE) %>% 
  select(WBID, WATERBODY_NAME, Date, WOTUSSTATUS, provassess)
