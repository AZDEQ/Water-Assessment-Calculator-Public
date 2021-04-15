#### K - QUALITY ASSURANCE ####

# As on 7/31/20 ELT wants draft WOTUS tracked as the key performance indicator.  
# The draft WOTUS metric takes over the old parameter delist graph and WQDB field.
# The ZQAPARAM is now the 'goto' document for provisional history.
# Pulls just the most recent decision at the parameter level
k.qaparam <- ZQAPARAM %>% 
  group_by(WBID, CharacteristicName) %>% 
  arrange(WBID, CharacteristicName, desc(Date)) %>% 
  distinct(WBID, CharacteristicName, .keep_all = TRUE) %>% 
  filter(Action == "Impaired")

# How many parameter impairments are identified by the tool?
k.param <- i.param %>% 
  filter(provassess == 3)

# Identify differences...both should be 0

# In human but not on delist graph
k.param.a <- k.param %>% 
  anti_join(k.qaparam, by = c("WBID", "CharacteristicName"))

# In delist graph but not human
k.param.b <- k.qaparam %>% 
  anti_join(k.param, by = c("WBID", "CharacteristicName"))

# Agreement between newassess and provassess in human file
k.agreementall <- human %>% 
  filter(newassess != provassess, is.na(provcomment))

# Logic to show user if QA passed.  Could configure this to actually prompt the user to fix the human file and then rerun a block of code 
run = nrow(k.param.a) + nrow(k.param.b)
if(run==0){
  print("QA Passed")
}else{
  print("Fix human file then rerun from read human to end")
}

end_time <- Sys.time()
end_time - start_time
