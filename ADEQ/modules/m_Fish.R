# #### TEMP FISH ####

# 
# # Join to c.sites to show WBID
# FISH <- FISH %>%
#   left_join(c.sites, by = c("STATION_CD" = "SiteID"))
# # 
# # # Summarize Fish Results by Waterbody, Fish and Just Mercury...expand to more parameters if needed.
# FISH2 <- FISH %>%
#   filter(SUBSTANCE_NAME == "MERCURY") %>%
#   mutate(STDDETLIM = ifelse(is.na(DETECTION_LIMIT), 0.012, DETECTION_LIMIT)) %>%
#   mutate(STDRESULT = ifelse(is.na(LAB_RESULT), STDDETLIM/2, LAB_RESULT)) %>%
#   select(WBID, STATION_ALT_NAME, ACTIVITY_END_DATE, FINALID, LAB_QA_FLAGS, SUBSTANCE_NAME, LAB_RESULT, LAB_RESULT_UNIT, STDRESULT, STDDETLIM, DETECTION_LIMIT, DETECTION_LIMIT_UNIT)
# # 
# # # Do the advisory based on current criteria.  Need 5 or more fish.  Red = Do not eat, Orange = Limit Consumption, Green is Unlimited Consumption, Yellow is Inconclusive  
# FISHAdvisory <- FISH2 %>%
#   group_by(WBID, FINALID) %>%
#   summarise(count = n(), lastdate = max(ACTIVITY_END_DATE), max = max(STDRESULT), mean = mean(STDRESULT), sd = sd(STDRESULT), meanminussd = mean(STDRESULT)-sd(STDRESULT), meanplussd = mean(STDRESULT)+sd(STDRESULT)) %>%
#   left_join(ZWATERBODYNAME, by = "WBID") %>%
#   rename(Species = FINALID) %>%
#   mutate(automatedadvisory = ifelse(max > 2, "Red",
#                            ifelse(count >= 5 & mean >= 0.75, "Red",
#                                   ifelse(count >= 5 & mean + sd > 0.3, "Orange",
#                                          ifelse(count >= 5 & mean - sd < 0.3, "Green", "Yellow")))))
# #
# # # Load Existing Fish Advisories
# ZFISHADVISORIES <- read_csv("inputs/ZFISHADVISORIES.csv")
# 
# # Any Differences?
# FISHDifferences <- FISHAdvisory %>%
#   full_join(ZFISHADVISORIES, by = c("WBID", "Species"))
# 
# # # Output to Fish Folder
# write.csv(FISHDifferences, paste0("J:/WQD/Surface Water Section/Monitoring Unit/Fish/Automated Fish Advisories R/FishAdvisories",Sys.Date(),".csv"))
# write.csv(FISH2, "J:/WQD/Surface Water Section/Monitoring Unit/Fish/Automated Fish Advisories R/Fishdata.csv")