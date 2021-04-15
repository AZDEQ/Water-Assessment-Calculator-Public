#### Charts ####


# Impairment Causes by Parameter
human %>% 
  filter(provassess == 3) %>% 
  distinct(WBID, CharacteristicName) %>% # Resolves double counting due to use, fraction
  group_by(CharacteristicName) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = reorder(CharacteristicName, Count), y = Count)) +
  geom_col(alpha = 0.6, fill = "#c00000", color = "#c00000") +
  coord_flip() +
  labs(y = "Count", x = "") +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  theme_light() +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")

# Dashboard Export - Impairment Causes

ZPLOTCAUSE <- human %>% 
  filter(provassess == 3) %>% 
  distinct(WBID, CharacteristicName) %>% # Resolves double counting due to use, fraction
  group_by(CharacteristicName) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))  

write.csv(ZPLOTCAUSE, "shinyDashboardFiles/inputs/ZPLOTCAUSE.csv", row.names = FALSE)

# Use Decisions
j.reporta %>%
  distinct(WBID, Use, DecisionUse) %>% 
  group_by(Use, DecisionUse) %>% 
  summarise(Count = n()) %>% 
  filter(DecisionUse %in% c("Insufficient information", "Not supporting", "Supporting")) %>% 
  ggplot(aes(x = reorder(Use, Count), y = Count, fill = DecisionUse, color = DecisionUse)) +
  geom_col(alpha = 0.6) +
  coord_flip() +
  labs(y = "Count", x = "") +
  scale_y_continuous(breaks = seq(0, 350, 50), expand = c(0.01, 0)) +
  scale_fill_manual(values = c("#ffc000", "#c00000", "#70ad47")) +
  scale_color_manual(values = c("#ffc000", "#c00000", "#70ad47")) +
  theme_light() +
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom", legend.title = element_blank())

# Dashboard Export
ZPLOTUSE <- j.reporta %>%
  distinct(WBID, Use, DecisionUse) %>% 
  group_by(Use, DecisionUse) %>% 
  summarise(Count = n()) %>% 
  filter(DecisionUse %in% c("Insufficient information", "Not supporting", "Supporting"))  

write.csv(ZPLOTUSE, "shinyDashboardFiles/inputs/ZPLOTUSE.csv", row.names = FALSE)

# Organization Decisions
c.stddata2 %>% 
  mutate(Year = year(ActivityStartDate)) %>% 
  group_by(Year, ActivityConductingOrganizationText) %>% 
  summarise(Count = n()) %>% 
  filter(!is.na(ActivityConductingOrganizationText)) %>% 
  ggplot(aes(x = Year, y = Count, fill = factor(ActivityConductingOrganizationText, levels = c("Volunteer", "USGS", "Other", "Government", "ADEQ")), color = factor(ActivityConductingOrganizationText, levels = c("Volunteer", "USGS", "Other", "Government", "ADEQ")))) +
  geom_col(alpha = 0.6) +
  labs(y = "Number of Records", x = "") +
  scale_fill_manual(values = c("#70ad47", "#ed7d31", "#7030a0", "#c00000", "#4472c4")) +
  scale_color_manual(values = c("#70ad47", "#ed7d31", "#7030a0", "#c00000", "#4472c4")) +
  scale_y_continuous(breaks = seq(0, 60000, 10000), expand = c(0.01, 0)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank())

# # gsh - uncomment when connection to DB is possible
# # FISH ADVISORY CHECKER 
# # Open connection object.
# conn <- odbcConnect("com", uid="<username>", pwd="<password>")
# 
# # Check connection object is open.
# odbcGetInfo(conn)
# 
# # Query the database and put the results into the data frame
# FISH <- sqlQuery(conn,"select STATION_CD, STATION_ALT_NAME, ACTIVITY_END_DATE, FINALID, LAB_QA_FLAGS, SUBSTANCE_NAME, LAB_RESULT, LAB_RESULT_UNIT, DETECTION_LIMIT, DETECTION_LIMIT_UNIT from VW_FISH_QUERY", rows_at_time = 1,believeNRows = FALSE)
# 
# #Close connection object.
# close(conn)

# Join to c.sites to show WBID
FISH <- FISH %>%
  left_join(c.sites, by = c("STATION_CD" = "SiteID"))

# Summarize Fish Results by Waterbody, Fish and Just Mercury...expand to more parameters if needed.
FISH2 <- FISH %>%
  filter(SUBSTANCE_NAME == "MERCURY") %>%
  mutate(STDDETLIM = ifelse(is.na(DETECTION_LIMIT), 0.012, DETECTION_LIMIT)) %>%
  mutate(STDRESULT = ifelse(is.na(LAB_RESULT), STDDETLIM/2, LAB_RESULT)) %>%
  select(WBID, STATION_ALT_NAME, ACTIVITY_END_DATE, FINALID, LAB_QA_FLAGS, SUBSTANCE_NAME, LAB_RESULT, LAB_RESULT_UNIT, STDRESULT, STDDETLIM, DETECTION_LIMIT, DETECTION_LIMIT_UNIT)
#
# # Do the advisory based on current criteria.  Need 5 or more fish.  Red = Do not eat, Orange = Limit Consumption, Green is Unlimited Consumption, Yellow is Inconclusive
FISHAdvisory <- FISH2 %>%
  group_by(WBID, FINALID) %>%
  summarise(count = n(), lastdate = max(ACTIVITY_END_DATE), max = max(STDRESULT), mean = mean(STDRESULT), sd = sd(STDRESULT), meanminussd = mean(STDRESULT)-sd(STDRESULT), meanplussd = mean(STDRESULT)+sd(STDRESULT)) %>%
  left_join(YWATERBODYNAME, by = "WBID") %>%
  rename(Species = FINALID) %>%
  mutate(automatedadvisory = ifelse(max > 2, "Red",
                                    ifelse(count >= 5 & mean >= 0.75, "Red",
                                           ifelse(count >= 5 & mean - sd > 0.3, "Orange",
                                                  ifelse(count >= 5 & mean + sd < 0.3, "Green", "Yellow")))))
#
# # Load Existing Fish Advisories
# ZFISHADVISORIES <- read_csv("inputs/ZFISHADVISORIES.csv", 
#                             col_types = cols(Advice = col_character(), 
#                                              Color = col_character(), `Date Issued` = col_date(format = "%m/%d/%Y"), 
#                                              Parameter = col_character(), Species = col_character(), 
#                                              WBID = col_character(), `Waterbody Name` = col_character()))

# Any Differences?
FISHDifferences <- FISHAdvisory %>%
  full_join(ZFISHADVISORIES, by = c("WBID", "Species"))

# # Output to Fish Folder
write.csv(FISHDifferences, paste0("jdrive/FishAdvisories",Sys.Date(),".csv"))
write.csv(FISH2, "jdrive/Fishdata.csv")

# How many parameter impairments in most recent parameter delist graph?
# gsh - no need to rename as ZQAPARAM already has CharacteristicName and not IMPAIRMENTS
# ZQAPARAM <- rename(ZQAPARAM, CharacteristicName = IMPAIRMENTS)

# How many parameter impairments are identified by the tool?
k.param <- i.param %>% 
  filter(provassess == 3)

# Identify differences...both should be 0

# In human but not on delist graph
k.param.a <- k.param %>% 
  anti_join(ZQAPARAM, by = c("WBID", "CharacteristicName"))

# In delist graph but not human
k.param.b <- ZQAPARAM %>% 
  anti_join(k.param, by = c("WBID", "CharacteristicName"))

# Agreement between newassess and provassess in human file
k.agreementall <- human %>% 
  filter(newassess != provassess, is.na(provcomment))

