#### F - COMPARE RESULTS TO STANDARDS ####


# ZSTDTYPE groups standards into hardness, regular, ammonia, oxygen, none, etc.  

# Join stdtype to data
f.stdtypejoin <- full_join(e.spaceaggregatefinal, ZSTDTYPE, by = c("CharacteristicName", "ResultSampleFractionText")) 

# Filters out any acute samples (AWC,W, EDW, E) that are also older than 3 years old
f.stdtypejoin <- filter(f.stdtypejoin, !grepl("Acute", NewUse, fixed = TRUE) | aggdate > acuteDate) #! before grepl basically filps the filter

f.stdtypejoin <- f.stdtypejoin %>% drop_na(WBID)

# This table is the map for DEQ WQDB uses to uses listed in rule/assessments



# Clean up.  Note these are discrepancies in WQDB Standards table.
ZDEQSTANDARDS <- ZDEQSTANDARDSAPPROVED %>% 
  mutate(ResultSampleFractionText = ifelse(CAS_QUALIFIER_NAME == "DISSOLVED", "Dissolved", "Total")) %>% 
  replace_na(list(ResultSampleFractionText = "Total")) %>% 
  mutate(Unit = "ug/l")

# Nitrate/ite don't from standard table to characteristic name fix
ZDEQSTANDARDS[grep("NITROGEN \\(NITRATE AND NITRITE\\), INORGANIC", ZDEQSTANDARDS$SUBSTANCE_NAME), "SUBSTANCE_NAME"] <- "INORGANIC NITROGEN (NITRATE AND NITRITE)" 

# Gather standards
f.gatheredstds <- gather(ZDEQSTANDARDS, STDUSE, STD, -SUBSTANCE_NAME, -SUBSTANCE_CAS_NO, -ResultSampleFractionText, -Unit)
f.gatheredstds <- rename(f.gatheredstds, CharacteristicName = SUBSTANCE_NAME)
f.gatheredstds$STD <- as.numeric(f.gatheredstds$STD)

# Remove duplicates
f.gatheredstds <- distinct(f.gatheredstds, CharacteristicName, SUBSTANCE_CAS_NO, ResultSampleFractionText, STDUSE, STD, Unit)

# Get rid of blanks/na's
f.gatheredstds <- drop_na(f.gatheredstds, STD)

# gsh - issue f.gatheredstds count
# gsh - remove where 'unit' is blank
# f.gatheredstds <- f.gatheredstds %>%
#  select(CharacteristicName,SUBSTANCE_CAS_NO,ResultSampleFractionText,STDUSE,STD,Unit) %>%
#  filter(Unit != '')

# Standard units for standards
f.gatheredstds <- inner_join(f.gatheredstds, ZSTDUNIT, by = c("Unit" = "ResultMeasure.MeasureUnitCode")) 

f.gatheredstds <- mutate(f.gatheredstds, STDNEW = Conversion * STD)

# 1 - Regular Standards
f1.stdregular <- filter(f.stdtypejoin, STDTYPE == "regular")

# Join for use map.  This aligns the use names in the database which are long to uses used in rule and by EPA 
f1.stdregular <- left_join(f1.stdregular, ZUSECROSSWALK2, by = "NewUse")

# gsh - issue - The below join does not include Inorganic Nitrogen as it is not in f.gatheredstds

# Join for characteristic name map.  This is the step where data that is not matched by standards is removed. Inner join to match data shared by both tables (ie. exclude data without standards)
f1.stdregular <- inner_join(f1.stdregular, f.gatheredstds, by = c("CharacteristicName", "ResultSampleFractionText", "STDUSE")) 
# csvTo_s3(f1.stdregular_gh,s3Bucket,"f1.stdregular_aws.csv","PracticeWrite")

# Identify if Standards Met
f1.stdregular <- f1.stdregular %>% 
  mutate(Exceed = ifelse(aggtimespace > STDNEW, "Yes", "No")) %>% # Confirmed > not >=
  select(-SUBSTANCE_CAS_NO) %>% 
  mutate(SUBSTANCE_CAS_NO = "None")

# Select the same fields to prep for rbind
f1.stdregular <- select(f1.stdregular, 
                        WBID,
                        aggdate, 
                        CharacteristicName,
                        ResultSampleFractionText,
                        NewUse,
                        aggtimespace,
                        STDTYPE,
                        STDUSE,
                        SUBSTANCE_CAS_NO,
                        STD,
                        STDNEW,
                        Exceed)

# Remove rows with NA in Exceed Column (missing either result or detection limit  or both)
f1.stdregular <- filter(f1.stdregular, !is.na(Exceed))

# 2 - Oxygen Standards
f2.stdoxygen <- f.stdtypejoin %>% 
  filter(CharacteristicName == "DISSOLVED OXYGEN SATURATION" | CharacteristicName == "DISSOLVED OXYGEN (DO)") %>% 
  filter(ResultSampleFractionText == "Dissolved")

# Filter for AW use
f2.stdoxygen <- filter(f2.stdoxygen, grepl("AW", NewUse, fixed = TRUE))
f2.stdoxygen <- filter(f2.stdoxygen, grepl("Chronic", NewUse, fixed = TRUE)) # Chronic is arbitrary and picked here so the full 5 years of data is pulled.  Acute pulls 3.
f2.stdoxygen <- filter(f2.stdoxygen, NewUse != "AWEAcute") # Exclude AWE...no standards for ephemerals

# Pair/spread %sat and oxygen concentration 
f2.stdoxygen <- spread(f2.stdoxygen, CharacteristicName, aggtimespace)

# Join for use map
f2.joinstdoxygen <- inner_join(f2.stdoxygen, ZUSECROSSWALK2, by = "NewUse")

# Switch Use back to Acute...that is how it is entered in the standards table
f2.joinstdoxygen[grep("AWC_CHRONIC_MAX", f2.joinstdoxygen$STDUSE), "STDUSE"] <- "AWC_ACUTE_MIN"
f2.joinstdoxygen[grep("AWEDW_CHRONIC_MAX", f2.joinstdoxygen$STDUSE), "STDUSE"] <- "AWEDW_ACUTE_MIN"
f2.joinstdoxygen[grep("AWW_CHRONIC_MAX", f2.joinstdoxygen$STDUSE), "STDUSE"] <- "AWW_ACUTE_MIN"

# Format, Add Standards, Determine if Standards Met
f2.joinstdoxygen <- f2.joinstdoxygen %>% 
  rename(aggtimespace = `DISSOLVED OXYGEN (DO)`) %>%  
  mutate(CharacteristicName = "DISSOLVED OXYGEN (DO)") %>%  
  left_join(f.gatheredstds, by = c("CharacteristicName", "STDUSE")) %>% 
  select(-ResultSampleFractionText.y) %>% 
  rename(ResultSampleFractionText = ResultSampleFractionText.x) %>% 
  rename(STDNEW2 = STD) %>% # do is already in mg/L so no need to create new column
  mutate(Exceed = ifelse(aggtimespace < STDNEW2, "Yes", "No")) %>% 
  drop_na(aggtimespace)

# Remove any rows where % saturation is na but has a yes for exceeds 
f2.joinstdoxygen <- f2.joinstdoxygen %>% 
  filter(Exceed != "Yes" | !is.na(`DISSOLVED OXYGEN SATURATION`))

f2.joinstdoxygen$Exceed[f2.joinstdoxygen$`DISSOLVED OXYGEN SATURATION` >= 90] <- "No"  
f2.joinstdoxygen$STD <- 999999.9
f2.joinstdoxygen$SUBSTANCE_CAS_NO <- paste0("Percent Saturation = ", f2.joinstdoxygen$`DISSOLVED OXYGEN SATURATION`)

# Select the same fields to prep for bind
f2.joinstdoxygen <- select(f2.joinstdoxygen, 
                           WBID,
                           aggdate, 
                           CharacteristicName,
                           ResultSampleFractionText,
                           NewUse,
                           aggtimespace,
                           STDTYPE,
                           STDUSE,
                           SUBSTANCE_CAS_NO,
                           STD,
                           STDNEW,
                           Exceed)

# 3 - Hardness Dependent Standards
f3.stdhard <- filter(f.stdtypejoin, STDTYPE == "hardness")

# Just need aquatic life
f3.stdhard <- filter(f3.stdhard, grepl("AW", NewUse, fixed = TRUE))

# Concatenate fraction and characteristic name so spread works
f3.stdhard$fractionandchar <- paste(f3.stdhard$ResultSampleFractionText, f3.stdhard$CharacteristicName, sep = "zzz") 

# Get rid of total dissolved and characteristicname.  Ungrouping important here.
f3.stdhard <- f3.stdhard %>%
  ungroup() %>% 
  select(-ResultSampleFractionText, -CharacteristicName) 

# Pair/spread all hardness data 
f3.stdhardgather <- spread(f3.stdhard, fractionandchar, aggtimespace)

# Combine all hardnesses based on priority
f3.stdhardgather$newhardness <- ifelse(!is.na(f3.stdhardgather$`DissolvedzzzHARDNESS, CA, MG`), f3.stdhardgather$`DissolvedzzzHARDNESS, CA, MG`, 
                                       ifelse(!is.na(f3.stdhardgather$`DissolvedzzzTOTAL HARDNESS`), f3.stdhardgather$`DissolvedzzzTOTAL HARDNESS`, 
                                              ifelse(!is.na(f3.stdhardgather$`DissolvedzzzHARDNESS, NON-CARBONATE`), f3.stdhardgather$`DissolvedzzzHARDNESS, NON-CARBONATE`, 
                                                     ifelse(!is.na(f3.stdhardgather$`TotalzzzTOTAL HARDNESS`), f3.stdhardgather$`TotalzzzTOTAL HARDNESS`,
                                                            ifelse(!is.na(f3.stdhardgather$`TotalzzzHARDNESS, CA, MG`), f3.stdhardgather$`TotalzzzHARDNESS, CA, MG`, NA)))))

# Hardness function.  Calculates hardness dependent standards and determines if exceedance
# Arguments are the data, standard name, hardness formula, exceedance name
hardness <- function(data, cdstd, cr3std, custd, pbstd, nistd, agstd, znstd,
                     cdform, cr3form, cuform, pbform, niform, agform, znform,
                     cdexceed, cr3exceed, cuexceed, pbexceed, niexceed, agexceed, znexceed){
  
  # Calculate hardness dependent standard and determine if result exceeded standard
  f3.hard <- data %>%
    mutate(cdstd = cdform) %>%
    mutate(cr3std = cr3form) %>%
    mutate(custd = cuform) %>%
    mutate(pbstd = pbform) %>%
    mutate(nistd = niform) %>%
    mutate(agstd = agform) %>%
    mutate(znstd = znform) %>%
    mutate(cdexceed = ifelse(DissolvedzzzCADMIUM > cdstd, "Yes", "No")) %>%
    mutate(cr3exceed = ifelse(DissolvedzzzCHROMIUM > cr3std, "Yes", "No")) %>%
    mutate(cuexceed = ifelse(DissolvedzzzCOPPER > custd, "Yes", "No")) %>%
    mutate(pbexceed = ifelse(DissolvedzzzLEAD > pbstd, "Yes", "No")) %>%
    mutate(niexceed = ifelse(DissolvedzzzNICKEL > nistd, "Yes", "No")) %>%
    mutate(agexceed = ifelse(DissolvedzzzSILVER > agstd, "Yes", "No")) %>%
    mutate(znexceed = ifelse(DissolvedzzzZINC > znstd, "Yes", "No"))
}

# Hardness formatting function.  Standardizes for use in f.exceed.
hardformat <- function(harddf, stduse, parameter, standard, exceed) {
  f3.hardformat <- harddf %>% 
    select(WBID, aggdate, NewUse, critmet, STDTYPE, parameter, newhardness, standard, exceed) %>% 
    mutate(ResultSampleFractionText = "Dissolved") %>% 
    mutate(SUBSTANCE_CAS_NO = paste("Hardness (mg/L) = ", newhardness)) %>%
    mutate(STDUSE = stduse) %>% 
    rename(aggtimespace = parameter) %>% 
    rename(STD = standard) %>% 
    mutate(STDNEW = STD) %>% 
    mutate(CharacteristicName = sub(pattern = "Dissolvedzzz", replacement = "", parameter)) %>% 
    rename(Exceed = exceed) %>% 
    select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
    drop_na(Exceed)
}

### AWCAcute

# Filter
f3.awcacute <- filter(f3.stdhardgather, NewUse == "AWCAcute")

# Determine standards/exceedances
f3.awcacute <- hardness(data = f3.awcacute, 
                        # cdstd = "cdawcacute", cdform = (exp(0.9789*log(f3.awcacute$newhardness)-3.866)*(1.136672-log(f3.awcacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawcacute",
                        cdstd = "cdawcacute", cdform = (exp(1.0166*log(f3.awcacute$newhardness)-3.924)*(1.136672-log(f3.awcacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawcacute",
                        cr3std = "cr3awcacute", cr3form = (exp(0.819*log(f3.awcacute$newhardness)+3.7256)*(0.316))/1000, cr3exceed = "Exceedcr3acute",
                        custd = "cuawcacute", cuform = (exp(0.9422*log(f3.awcacute$newhardness)-1.7)*(0.96))/1000, cuexceed = "Exceedcuawcacute",
                        pbstd = "pbawcacute", pbform = (exp(1.273*log(f3.awcacute$newhardness)-1.46)*(1.46203-log(f3.awcacute$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawcacute",
                        nistd = "niawcacute", niform = (exp(0.846*log(f3.awcacute$newhardness)+2.255)*(0.998))/1000, niexceed = "Exceedniawcacute",
                        agstd = "agawcacute", agform = (exp(1.72*log(f3.awcacute$newhardness)-6.59)*(0.85))/1000, agexceed = "Exceedagawcacute",
                        znstd = "znawcacute", znform = (exp(0.8473*log(f3.awcacute$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawcacute")

# Format 
f3.awcacutecd <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awcacutecr3 <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX","DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awcacutecu <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awcacuteag <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzSILVER", "agstd", "agexceed")
f3.awcacuteni <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awcacutepb <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awcacutezn <- hardformat(harddf = f3.awcacute, "AWC_ACUTE_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine Awcacute
f3.awcacutegather <- rbind(f3.awcacutecd, f3.awcacutecr3, f3.awcacutecu, f3.awcacutepb, f3.awcacuteni, f3.awcacuteag, f3.awcacutezn)

### AWCChronic

# Filter
f3.awcchronic <- filter(f3.stdhardgather, NewUse == "AWCChronic")

# Determine standards/exceedances
f3.awcchronic <- hardness(data = f3.awcchronic, 
                          # cdstd = "cdawcchronic", cdform = (exp(0.7977*log(f3.awcchronic$newhardness)-3.909)*(1.101672-log(f3.awcchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawcchronic",
                          cdstd = "cdawcchronic", cdform = (exp(0.7409*log(f3.awcchronic$newhardness)-4.719)*(1.101672-log(f3.awcchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawcchronic",
                          cr3std = "cr3awcchronic", cr3form = (exp(0.819*log(f3.awcchronic$newhardness)+0.6848)*(0.86))/1000, cr3exceed = "Exceedcr3chronic",
                          custd = "cuawcchronic", cuform = (exp(0.8545*log(f3.awcchronic$newhardness)-1.702)*(0.96))/1000, cuexceed = "Exceedcuawcchronic",
                          pbstd = "pbawcchronic", pbform = (exp(1.273*log(f3.awcchronic$newhardness)-4.705)*(1.46203-log(f3.awcchronic$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawcchronic",
                          nistd = "niawcchronic", niform = (exp(0.846*log(f3.awcchronic$newhardness)+0.0584)*(0.997))/1000, niexceed = "Exceedniawcchronic",
                          agstd = "agawcchronic", agform = "none", agexceed = "agexceed",
                          znstd = "znawcchronic", znform = (exp(0.8473*log(f3.awcchronic$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawcchronic")

# Format 
f3.awcchroniccd <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awcchroniccr3 <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awcchroniccu <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awcchronicni <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awcchronicpb <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awcchroniczn <- hardformat(harddf = f3.awcchronic, "AWC_CHRONIC_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine Awcchronic
f3.awcchronicgather <- rbind(f3.awcchroniccd, f3.awcchroniccr3, f3.awcchroniccu, f3.awcchronicpb, f3.awcchronicni, f3.awcchroniczn)

### AWWacute

# Filter
f3.awwacute <- filter(f3.stdhardgather, NewUse == "AWWAcute")

# Determine standards/exceedances
f3.awwacute <- hardness(data = f3.awwacute, 
                        # cdstd = "cdawwacute", cdform = (exp(0.9789*log(f3.awwacute$newhardness)-2.208)*(1.136672-log(f3.awwacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawwacute",
                        cdstd = "cdawwacute", cdform = (exp(1.128*log(f3.awwacute$newhardness)-3.6867)*(1.136672-log(f3.awwacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawwacute",
                        cr3std = "cr3awwacute", cr3form = (exp(0.819*log(f3.awwacute$newhardness)+3.7256)*(0.316))/1000, cr3exceed = "Exceedcr3acute",
                        custd = "cuawwacute", cuform = (exp(0.9422*log(f3.awwacute$newhardness)-1.7)*(0.96))/1000, cuexceed = "Exceedcuawwacute",
                        pbstd = "pbawwacute", pbform = (exp(1.273*log(f3.awwacute$newhardness)-1.46)*(1.46203-log(f3.awwacute$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawwacute",
                        nistd = "niawwacute", niform = (exp(0.846*log(f3.awwacute$newhardness)+2.255)*(0.998))/1000, niexceed = "Exceedniawwacute",
                        agstd = "agawwacute", agform = (exp(1.72*log(f3.awwacute$newhardness)-6.59)*(0.85))/1000, agexceed = "Exceedagawwacute",
                        znstd = "znawwacute", znform = (exp(0.8473*log(f3.awwacute$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawwacute")

# Format 
f3.awwacutecd <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awwacutecr3 <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awwacutecu <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awwacuteag <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzSILVER", "agstd", "agexceed")
f3.awwacuteni <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awwacutepb <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awwacutezn <- hardformat(harddf = f3.awwacute, "AWW_ACUTE_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine awwacute
f3.awwacutegather <- rbind(f3.awwacutecd, f3.awwacutecr3, f3.awwacutecu, f3.awwacutepb, f3.awwacuteni, f3.awwacuteag, f3.awwacutezn)

### AWWChronic

# Filter
f3.awwchronic <- filter(f3.stdhardgather, NewUse == "AWWChronic")

# Determine standards/exceedances
f3.awwchronic <- hardness(data = f3.awwchronic, 
                          # cdstd = "cdawwchronic", cdform = (exp(0.7977*log(f3.awwchronic$newhardness)-3.909)*(1.101672-log(f3.awwchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawwchronic",
                          cdstd = "cdawwchronic", cdform = (exp(0.7852*log(f3.awwchronic$newhardness)-2.715)*(1.101672-log(f3.awwchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawwchronic",
                          cr3std = "cr3awwchronic", cr3form = (exp(0.819*log(f3.awwchronic$newhardness)+0.6848)*(0.86))/1000, cr3exceed = "Exceedcr3chronic",
                          custd = "cuawwchronic", cuform = (exp(0.8545*log(f3.awwchronic$newhardness)-1.702)*(0.96))/1000, cuexceed = "Exceedcuawwchronic",
                          pbstd = "pbawwchronic", pbform = (exp(1.273*log(f3.awwchronic$newhardness)-4.705)*(1.46203-log(f3.awwchronic$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawwchronic",
                          nistd = "niawwchronic", niform = (exp(0.846*log(f3.awwchronic$newhardness)+0.0584)*(0.997))/1000, niexceed = "Exceedniawwchronic",
                          agstd = "agawwchronic", agform = "none", agexceed = "agexceed",
                          znstd = "znawwchronic", znform = (exp(0.8473*log(f3.awwchronic$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawwchronic")

# Format 
f3.awwchroniccd <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awwchroniccr3 <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awwchroniccu <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awwchronicni <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awwchronicpb <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awwchroniczn <- hardformat(harddf = f3.awwchronic, "AWW_CHRONIC_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine awwchronic
f3.awwchronicgather <- rbind(f3.awwchroniccd, f3.awwchroniccr3, f3.awwchroniccu, f3.awwchronicpb, f3.awwchronicni, f3.awwchroniczn)

### AWEDWacute

# Filter
f3.awedwacute <- f3.stdhardgather %>% filter(NewUse == "AWEDWAcute") 

# Determine standards/exceedances
f3.awedwacute <- hardness(data = f3.awedwacute, 
                          # cdstd = "cdawedwacute", cdform = (exp(0.9789*log(f3.awedwacute$newhardness)-2.208)*(1.136672-log(f3.awedwacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawedwacute",
                          cdstd = "cdawedwacute", cdform = (exp(1.128*log(f3.awedwacute$newhardness)-3.6867)*(1.136672-log(f3.awedwacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawedwacute",
                          cr3std = "cr3awedwacute", cr3form = (exp(0.819*log(f3.awedwacute$newhardness)+3.7256)*(0.316))/1000, cr3exceed = "Exceedcr3acute",
                          custd = "cuawedwacute", cuform = (exp(0.9422*log(f3.awedwacute$newhardness)-1.7)*(0.96))/1000, cuexceed = "Exceedcuawedwacute",
                          pbstd = "pbawedwacute", pbform = (exp(1.273*log(f3.awedwacute$newhardness)-1.46)*(1.46203-log(f3.awedwacute$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawedwacute",
                          nistd = "niawedwacute", niform = (exp(0.846*log(f3.awedwacute$newhardness)+2.255)*(0.998))/1000, niexceed = "Exceedniawedwacute",
                          agstd = "agawedwacute", agform = (exp(1.72*log(f3.awedwacute$newhardness)-6.59)*(0.85))/1000, agexceed = "Exceedagawedwacute",
                          znstd = "znawedwacute", znform = (exp(0.8473*log(f3.awedwacute$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawedwacute")

# Format 
f3.awedwacutecd <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awedwacutecr3 <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awedwacutecu <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awedwacuteag <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzSILVER", "agstd", "agexceed")
f3.awedwacuteni <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awedwacutepb <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awedwacutezn <- hardformat(harddf = f3.awedwacute, "AWEDW_ACUTE_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine awedwacute
f3.awedwacutegather <- rbind(f3.awedwacutecd, f3.awedwacutecr3, f3.awedwacutecu, f3.awedwacutepb, f3.awedwacuteni, f3.awedwacuteag, f3.awedwacutezn)

### AWEDWChronic

# Filter
f3.awedwchronic <- filter(f3.stdhardgather, NewUse == "AWEDWChronic")

# Determine standards/exceedances
f3.awedwchronic <- hardness(data = f3.awedwchronic, 
                            # cdstd = "cdawedwchronic", cdform = (exp(0.7977*log(f3.awedwchronic$newhardness)-3.909)*(1.101672-log(f3.awedwchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawedwchronic",
                            cdstd = "cdawedwchronic", cdform = (exp(0.7852*log(f3.awedwchronic$newhardness)-2.715)*(1.101672-log(f3.awedwchronic$newhardness)*0.041838))/1000, cdexceed = "Exceedcdawedwchronic",
                            cr3std = "cr3awedwchronic", cr3form = (exp(0.819*log(f3.awedwchronic$newhardness)+0.6848)*(0.86))/1000, cr3exceed = "Exceedcr3chronic",
                            custd = "cuawedwchronic", cuform = (exp(0.8545*log(f3.awedwchronic$newhardness)-1.702)*(0.96))/1000, cuexceed = "Exceedcuawedwchronic",
                            pbstd = "pbawedwchronic", pbform = (exp(1.273*log(f3.awedwchronic$newhardness)-4.705)*(1.46203-log(f3.awedwchronic$newhardness)*0.145712))/1000, pbexceed = "Exceedpbawedwchronic",
                            nistd = "niawedwchronic", niform = (exp(0.846*log(f3.awedwchronic$newhardness)+0.0584)*(0.997))/1000, niexceed = "Exceedniawedwchronic",
                            agstd = "agawedwchronic", agform = "none", agexceed = "agexceed",
                            znstd = "znawedwchronic", znform = (exp(0.8473*log(f3.awedwchronic$newhardness)+0.884)*(0.978))/1000, znexceed = "Exceedznawedwchronic")

# Format 
f3.awedwchroniccd <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.awedwchroniccr3 <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.awedwchroniccu <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.awedwchronicni <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.awedwchronicpb <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.awedwchroniczn <- hardformat(harddf = f3.awedwchronic, "AWEDW_CHRONIC_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine awedwchronic
f3.awedwchronicgather <- rbind(f3.awedwchroniccd, f3.awedwchroniccr3, f3.awedwchroniccu, f3.awedwchronicpb, f3.awedwchronicni, f3.awedwchroniczn)

### AWEAcute

# Filter
f3.aweacute <- filter(f3.stdhardgather, NewUse == "AWEAcute")


# Determine standards/exceedances
f3.aweacute <- hardness(data = f3.aweacute, 
                        # cdstd = "cdaweacute", cdform = (exp(0.9789*log(f3.aweacute$newhardness)-1.363)*(1.136672-log(f3.aweacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdaweacute",
                        cdstd = "cdaweacute", cdform = (exp(1.128*log(f3.aweacute$newhardness)-0.9691)*(1.136672-log(f3.aweacute$newhardness)*0.041838))/1000, cdexceed = "Exceedcdaweacute",
                        cr3std = "cr3aweacute", cr3form = (exp(0.819*log(f3.aweacute$newhardness)+4.9361)*(0.316))/1000, cr3exceed = "Exceedcr3acute",
                        custd = "cuaweacute", cuform = (exp(0.9422*log(f3.aweacute$newhardness)-1.1514)*(0.96))/1000, cuexceed = "Exceedcuaweacute",
                        pbstd = "pbaweacute", pbform = (exp(1.273*log(f3.aweacute$newhardness)-0.7131)*(1.46203-log(f3.aweacute$newhardness)*0.145712))/1000, pbexceed = "Exceedpbaweacute",
                        nistd = "niaweacute", niform = (exp(0.846*log(f3.aweacute$newhardness)+4.4389)*(0.998))/1000, niexceed = "Exceedniaweacute",
                        agstd = "agaweacute", agform = (exp(1.72*log(f3.aweacute$newhardness)-6.59)*(0.85))/1000, agexceed = "Exceedagaweacute",
                        znstd = "znaweacute", znform = (exp(0.8473*log(f3.aweacute$newhardness)+3.1342)*(0.978))/1000, znexceed = "Exceedznaweacute")

# Format 
f3.aweacutecd <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzCADMIUM", "cdstd", "cdexceed")
f3.aweacutecr3 <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzCHROMIUM", "cr3std", "cr3exceed")
f3.aweacutecu <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzCOPPER", "custd", "cuexceed")
f3.aweacuteag <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzSILVER", "agstd", "agexceed")
f3.aweacuteni <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzNICKEL", "nistd", "niexceed")
f3.aweacutepb <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzLEAD", "pbstd", "pbexceed")
f3.aweacutezn <- hardformat(harddf = f3.aweacute, "AWE_ACUTE_MAX", "DissolvedzzzZINC", "znstd", "znexceed")

# Combine aweacute
f3.aweacutegather <- rbind(f3.aweacutecd, f3.aweacutecr3, f3.aweacutecu, f3.aweacutepb, f3.aweacuteni, f3.aweacuteag, f3.aweacutezn)

# 4 Suspended Sediment Concentration - Note: Uses acute dataset with full 5 years
f4.stdssc <- f.stdtypejoin %>% 
  ungroup() %>% 
  filter(CharacteristicName == "NEWSSC") %>% 
  filter(is.na(ResultSampleFractionText)) %>% 
  select(-CharacteristicName) %>% 
  mutate(CharacteristicName = "SUSPENDED SEDIMENT CONCENTRATION (SSC)")

# Define applicable uses
f4.uses <- c("AWWAcute", "AWCAcute")
f4.stdssc <- filter(f4.stdssc, NewUse %in% f4.uses)

# Calculate median of last 4 samples.  Rep creates a grouping
f4.stdssc <- f4.stdssc %>%
  group_by(WBID) %>%
  mutate(sscmedian = rep(1:55, each = 4, length.out = length(WBID))) #1:55 because assumed there aren't more than 55 aggregated samples for one waterbody

# Allows check to make sure there are at least 4 samples in each group to take a median of 
f4.stdssc <- f4.stdssc %>%
  group_by(WBID, sscmedian) %>%
  mutate(ssccount = 1:n())

# Determiens if a minimum of 4 samples taken
f4.stdssc <- f4.stdssc %>%
  group_by(WBID, sscmedian) %>%
  mutate(ssclast = last(ssccount))

# Exclude data without a minimum of 4 samples
f4.stdssc <- filter(f4.stdssc, ssclast == 4)

# Take median of last 4 samples
f4.stdssc <- f4.stdssc %>%
  group_by(WBID, CharacteristicName, ResultSampleFractionText, NewUse, STDTYPE, sscmedian) %>%
  summarise(aggtimespace = median(aggtimespace), aggdate = min(aggdate))

# Insert STDS
f4.stdssc <- mutate(f4.stdssc, STD = ifelse(NewUse == "AWCAcute", 25, 80))

# Determine if standard met
f4.stdssc <- mutate(f4.stdssc, Exceed = ifelse(aggtimespace > STD, "Yes", "No"))

# Get SSC Ready for rbind
f4.stdssc <- f4.stdssc %>% 
  ungroup() %>% 
  mutate(ResultSampleFractionText = "Suspended")

# Blank out unneeded fields
f4.stdssc <- mutate(f4.stdssc, STDUSE = "none")
f4.stdssc <- mutate(f4.stdssc, SUBSTANCE_CAS_NO = "none")
f4.stdssc$STDNEW <- f4.stdssc$STD

# Select the same fields to prep for rbind
f4.stdssc <- select(f4.stdssc, 
                    WBID,
                    aggdate, 
                    CharacteristicName,
                    ResultSampleFractionText,
                    NewUse,
                    aggtimespace,
                    STDTYPE,
                    STDUSE,
                    SUBSTANCE_CAS_NO,
                    STD,
                    STDNEW,
                    Exceed)

# Ammonia 
f5.ammonia <- filter(f.stdtypejoin, CharacteristicName == "AMMONIA-NITROGEN" | CharacteristicName == "PH" | CharacteristicName == "TEMPERATURE, WATER")

# Add in min and max values for ph and temp
f5.ammonia <- within(f5.ammonia, aggtimespace[CharacteristicName == 'PH' & aggtimespace >= 9] <- 9)
f5.ammonia <- within(f5.ammonia, aggtimespace[CharacteristicName == 'PH' & aggtimespace <= 6.5] <- 6.5)
f5.ammonia <- within(f5.ammonia, aggtimespace[CharacteristicName == 'TEMPERATURE, WATER' & aggtimespace >= 30] <- 30)
f5.ammonia <- within(f5.ammonia, aggtimespace[CharacteristicName == 'TEMPERATURE, WATER' & aggtimespace <= 0] <- 0)

# Just need aquatic life
f5.ammonia <- filter(f5.ammonia, grepl("AW", NewUse, fixed = TRUE))

# Get rid of total dissolved
f5.ammonia <- f5.ammonia %>%
  ungroup() %>% 
  select(-ResultSampleFractionText, -STDTYPE) 

# Make sure no duplicate data
f5.ammonia <- f5.ammonia %>% 
  select(WBID, aggdate, CharacteristicName, NewUse, aggtimespace) %>% 
  distinct(WBID, aggdate, CharacteristicName, NewUse, .keep_all = TRUE) %>% 
  filter(!is.na(aggtimespace))

# Spread so calculations can happen
f5.ammoniaspread <- f5.ammonia %>%
  group_by(WBID, aggdate, NewUse) %>%
  spread(CharacteristicName, aggtimespace)

f5.ammoniaspread <- rename(f5.ammoniaspread, TEMP = `TEMPERATURE, WATER`) #Rename mg/L to join

# Ammonia Chronic

# Add standards for each dependent parameter
f5.ammoniachronic <- drop_na(f5.ammoniaspread, `AMMONIA-NITROGEN`, PH, TEMP)
f5.ammoniachronic <- filter(f5.ammoniachronic, grepl("Chronic", NewUse, fixed = TRUE))

# Old chronic ammonia standard
f5.ammoniachronic <- mutate(f5.ammoniachronic, p1 = ((0.0577/(1+10^(7.688-PH)))+(2.487/(1+(10^(PH-7.688))))))
f5.ammoniachronic <- mutate(f5.ammoniachronic, p2 = 25-TEMP)
f5.ammoniachronic <- mutate(f5.ammoniachronic, p3 = 1.45*(10^(0.028*(p2))))
f5.ammoniachronic <- mutate(f5.ammoniachronic, p4 = ifelse(p3 <= 2.85, 1.45*(10^(0.028*(p2))), 2.85))
f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awcchronic = p1*p4)
f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awwchronic = p1*p4)
f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awedwchronic = p1*p4)

# New AW warm/cold chronic ammonia standard.  Uses uniodids present assumed.
# f5.ammoniachronic <- mutate(f5.ammoniachronic, p1 = ((0.0278/(1+10^(7.688-PH)))+(1.1994/(1+(10^(PH-7.688))))))
# f5.ammoniachronic <- mutate(f5.ammoniachronic, p2 = (20-max(TEMP,7))) 
# f5.ammoniachronic <- mutate(f5.ammoniachronic, p3 = 2.126*(10^(0.028*(p2))))
# f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awcchronic = 0.8876*p1*p3)
# f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awwchronic = 0.8876*p1*p3)
# 
# # New AW EDW Chronic ammonia standard.  Uses the uniodids not present formula.
# f5.ammoniachronic <- mutate(f5.ammoniachronic, e1 = ((0.0278/(1+10^(7.688-PH)))+(1.1994/(1+(10^(PH-7.688))))))
# f5.ammoniachronic <- mutate(f5.ammoniachronic, e2 = (20-max(TEMP,7))) 
# f5.ammoniachronic <- mutate(f5.ammoniachronic, e3 = 7.547*(10^(0.028*(e2))))
# f5.ammoniachronic <- mutate(f5.ammoniachronic, nh3awedwchronic = 0.9405*e1*e3)

# Determine if chronic standards met
f5.ammoniachronic <- f5.ammoniachronic %>%
  mutate(Exceednh3awcchronic = ifelse(`AMMONIA-NITROGEN` > nh3awcchronic, "Yes", "No")) %>%
  mutate(Exceednh3awwchronic = ifelse(`AMMONIA-NITROGEN` > nh3awwchronic, "Yes", "No")) %>%
  mutate(Exceednh3awedwchronic = ifelse(`AMMONIA-NITROGEN` > nh3awedwchronic, "Yes", "No"))

# ammonia awcchronic
f5.ammoniachronicawc <- f5.ammoniachronic %>%
  filter(NewUse == "AWCChronic") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awcchronic, Exceednh3awcchronic) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWC_CHRONIC_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awcchronic) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceednh3awcchronic) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# ammonia awwchronic
f5.ammoniachronicaww <- f5.ammoniachronic %>%
  filter(NewUse == "AWWChronic") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awwchronic, Exceednh3awwchronic) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWW_CHRONIC_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awwchronic) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceednh3awwchronic) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# ammonia awedwchronic
f5.ammoniachronicawedw <- f5.ammoniachronic %>%
  filter(NewUse == "AWEDWChronic") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awedwchronic, Exceednh3awedwchronic) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWEDW_CHRONIC_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awedwchronic) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceednh3awedwchronic) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# Ammonia Acute
f5.ammoniaacute <- select(f5.ammoniaspread, -TEMP)
f5.ammoniaacute <- drop_na(f5.ammoniaspread, `AMMONIA-NITROGEN`, PH)

# Old AW acute standards
f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awcacute = (0.275/(1+(10^(7.204-PH))))+(39/(1+(10^(PH-7.204)))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awwacute = (0.411/(1+(10^(7.204-PH))))+(58.4/(1+(10^(PH-7.204)))))
f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awedwacute = (0.411/(1+(10^(7.204-PH))))+(58.4/(1+(10^(PH-7.204)))))

# # New AW cold ammonia acute standard.  Uniodids present.  Now has temp
# f5.ammoniaacute <- mutate(f5.ammoniaacute, p1 = ((0.275/(1+10^(7.204-PH)))+(39.0/(1+(10^(PH-7.204))))))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, p2 = 20-TEMP)
# f5.ammoniaacute <- mutate(f5.ammoniaacute, p3 = 23.12*(10^(0.036*(p2))))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, p4 = (0.7249*((0.0114/(1+10^(7.204-PH)))+(1.6181/(1+(10^(PH-7.204)))))))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, p5 = p4*p3)
# f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awcacute = ifelse(p1 <= p5, p1, p5))
# 
# # New AW warm ammonia acute standard.  Uniodids present.  Now has temp
# f5.ammoniaacute <- mutate(f5.ammoniaacute, w1 = (0.7249*((0.0114/(1+10^(7.204-PH)))+(1.6181/(1+(10^(PH-7.204)))))))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, w2 = 20-TEMP)
# f5.ammoniaacute <- mutate(f5.ammoniaacute, wt = 23.12*(10^(0.036*(w2))))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, w3 = ifelse(51.93 <= wt, 51.93, wt))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awwacute = w1*w3)
# 
# # New AW edw ammonia acute.  Uniodids absent.  Now has temp
# f5.ammoniaacute <- mutate(f5.ammoniaacute, e1 = (0.7249*((0.0114/(1+10^(7.204-PH)))+(1.6181/(1+(10^(PH-7.204)))))))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, e2 = 20-TEMP)
# f5.ammoniaacute <- mutate(f5.ammoniaacute, et = 62.15*(10^(0.036*(e2))))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, e3 = ifelse(51.93 <= et, 51.93, et))
# f5.ammoniaacute <- mutate(f5.ammoniaacute, nh3awedwacute = e1*e3)

# Determine if standards met
f5.ammoniaacute <- f5.ammoniaacute %>%
  mutate(Exceedawcacute = ifelse(`AMMONIA-NITROGEN` > nh3awcacute, "Yes", "No")) %>%
  mutate(Exceedawwacute = ifelse(`AMMONIA-NITROGEN` > nh3awwacute, "Yes", "No")) %>%
  mutate(Exceedawedwacute = ifelse(`AMMONIA-NITROGEN` > nh3awedwacute, "Yes", "No"))

# ammonia awcacute
f5.ammoniaacuteawc <- f5.ammoniaacute %>%
  filter(NewUse == "AWCAcute") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awcacute, Exceedawcacute) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWC_ACUTE_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awcacute) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceedawcacute) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# ammonia awwacute
f5.ammoniaacuteaww <- f5.ammoniaacute %>%
  filter(NewUse == "AWWAcute") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awwacute, Exceedawwacute) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWW_ACUTE_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awwacute) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceedawwacute) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# ammonia awedwacute
f5.ammoniaacuteawedw <- f5.ammoniaacute %>%
  filter(NewUse == "AWEDWAcute") %>%
  select(WBID, aggdate, NewUse, `AMMONIA-NITROGEN`, PH, TEMP, nh3awedwacute, Exceedawedwacute) %>% 
  mutate(aggtimespace = `AMMONIA-NITROGEN`) %>% 
  mutate(STDTYPE = "AMMONIA") %>% 
  mutate(CharacteristicName = "AMMONIA-NITROGEN") %>%
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(STDUSE = "AWEDW_ACUTE_MAX") %>% 
  mutate(SUBSTANCE_CAS_NO = paste("pH = ", PH, ":", "Temperature = ", TEMP)) %>% 
  mutate(STD = nh3awedwacute) %>% 
  mutate(STDNEW = STD) %>% 
  mutate(Exceed = Exceedawedwacute) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>% 
  drop_na(Exceed)

# 6 Site Specific Nutrients
f6.nutrient <- filter(f.stdtypejoin, CharacteristicName == "INORGANIC NITROGEN (NITRATE AND NITRITE)" | CharacteristicName == "KJELDAHL NITROGEN" | CharacteristicName == "NITROGEN" | CharacteristicName == "PHOSPHORUS")

# Nutrient site specific standards apply to recreational uses and aquatic life uses. Filter out all the extra uses.  AW and Rec have same values so at the end duplicate and change use.
f6.nutrient <- f6.nutrient %>% 
  filter(grepl("AW", NewUse)) %>% 
  filter(grepl("Chronic", NewUse)) # just chronic for full 5 years and to get rid of duplicates.  Not a chronic standard just using for proxy.

# Filter for total
f6.nutrient <- f6.nutrient %>% 
  filter(ResultSampleFractionText == "Total") 

# Determine if Nitrogen is either already calculated (preferred) or needs to be calculated by adding Kjeldahl nitrogen and nitrate/ite

# Get rid of total dissolved and characteristicname.  Ungrouping important here.
f6.nutrient <- f6.nutrient %>%
  ungroup() %>% 
  select(WBID, aggdate, CharacteristicName, aggtimespace, -ResultSampleFractionText, NewUse, -STDTYPE) 

# Pair/spread all nutrient data 
f6.nutrientspread <- spread(f6.nutrient, CharacteristicName, aggtimespace)

# Add the TKN and Nitrate/ite
f6.nutrientspread <- mutate(f6.nutrientspread, TN = `INORGANIC NITROGEN (NITRATE AND NITRITE)` + `KJELDAHL NITROGEN`)

# Combine nitrogen based on priority
f6.nutrientspread$newTN <- ifelse(!is.na(f6.nutrientspread$NITROGEN), f6.nutrientspread$NITROGEN, 
                                  ifelse(!is.na(f6.nutrientspread$TN), f6.nutrientspread$TN, NA))

# Open nutrient standards

# Find out which site specific standard applied to which WBID
f6.nutrientspread <- left_join(f6.nutrientspread, YWBHUCREACH, by = "WBID")

# Just need TN and TP and site specific
f6.nutrientspread <- f6.nutrientspread %>% 
  select(WBID, NewUse, aggdate, PHOSPHORUS, newTN, NUTRIENT) %>% 
  rename(NITROGEN = newTN) %>% 
  left_join(ZUSECROSSWALK2, by = "NewUse")

# Total Nitrogen 

# Filter and select for just nitrogen data to get ready for standards comparision
f6.tn <- f6.nutrientspread %>%
  select(-PHOSPHORUS) %>%
  filter(!is.na(NUTRIENT)) %>%
  filter(!is.na(NITROGEN)) %>%
  rename(aggtimespace = NITROGEN) %>%
  mutate(CharacteristicName = "NITROGEN")

# Join data with standards
f6.tn <- left_join(f6.tn, ZNUTRIENTSTDS, by = c("NUTRIENT", "CharacteristicName"))

# Single Sample Maximum
f6.tnssm <- f6.tn %>%
  select(-annualmean, -`90thpercentile`) %>%
  mutate(Exceed = ifelse(aggtimespace > ssm, "Yes", "No")) %>%
  mutate(ResultSampleFractionText = "Total") %>%
  mutate(STDTYPE = "Nutrient") %>%
  mutate(SUBSTANCE_CAS_NO = "None") %>%
  mutate(STD = 99999.9) %>%
  rename(STDNEW = ssm) %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# Annual Mean

# Step 1 get monthly means
f6.tnannualmean <- f6.tn %>%
  mutate(month = format(aggdate, "%m"), year = format(aggdate, "%Y")) %>%
  group_by(WBID, NUTRIENT, month, year) %>%
  summarise(monthmean = mean(aggtimespace))


# Need at least 2 monthly means
f6.tnannualmean <- f6.tnannualmean %>%
  group_by(WBID, NUTRIENT, month, year) %>%
  summarise(countmonthmean = n(), monthmean = mean(monthmean)) %>% 
  filter(countmonthmean > 1)

# Step 2 calculate annual mean then exclude any year with less or equal to 3
f6.tnannualmean <- f6.tnannualmean %>%
  group_by(WBID, NUTRIENT, year) %>%
  summarise(annmean = mean(monthmean), count = n()) %>%
  filter(count >= 3)

# Format fields
f6.tnannualmean <- f6.tnannualmean %>%
  mutate(CharacteristicName = "NITROGEN") %>%
  left_join(ZNUTRIENTSTDS, by = c("NUTRIENT", "CharacteristicName")) %>%
  mutate(CharacteristicName = "TNANNUALMEAN") %>%
  mutate(Exceed = ifelse(annmean > annualmean, "Yes", "No")) %>%
  mutate(ResultSampleFractionText = "Total") %>%
  mutate(aggdate = as.Date("1900-01-01")) %>%
  mutate(NewUse = "Site Specific Nutrient") %>%
  mutate(STDTYPE = "Nutrient") %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  mutate(STD = 99999.9) %>%
  rename(STDNEW = ssm) %>%
  rename(aggtimespace = annmean) %>%
  mutate(STDUSE = "none") %>%
  ungroup() %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed)) 

# Total Phosphorus 

# Filter and select for just nitrogen data to get ready for standards comparision
f6.tp <- f6.nutrientspread %>%
  select(-NITROGEN) %>%
  filter(!is.na(NUTRIENT)) %>%
  filter(!is.na(PHOSPHORUS)) %>%
  rename(aggtimespace = PHOSPHORUS) %>%
  mutate(CharacteristicName = "PHOSPHORUS")


# Join data with standards
f6.tp <- left_join(f6.tp, ZNUTRIENTSTDS, by = c("NUTRIENT", "CharacteristicName"))

# Single Sample Maximum
f6.tpssm <- f6.tp %>%
  select(-annualmean, -`90thpercentile`) %>%
  mutate(Exceed = ifelse(aggtimespace > ssm, "Yes", "No")) %>%
  mutate(ResultSampleFractionText = "Total") %>%
  mutate(STDTYPE = "Nutrient") %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  mutate(STD = 99999.9) %>%
  rename(STDNEW = ssm) %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# Annual Mean

# Step 1 get monthly means
f6.tpannualmean <- f6.tp %>%
  mutate(month = format(aggdate, "%m"), year = format(aggdate, "%Y")) %>%
  group_by(WBID, NUTRIENT, month, year) %>%
  summarise(monthmean = mean(aggtimespace))

# Need at least 2 monthly means
f6.tpannualmean <- f6.tpannualmean %>%
  group_by(WBID, NUTRIENT, month, year) %>%
  summarise(countmonthmean = n(), monthmean = mean(monthmean)) %>% 
  filter(countmonthmean > 1)

# Step 2 calculate annual mean then exclude any year with less or equal to 3
f6.tpannualmean <- f6.tpannualmean %>%
  group_by(WBID, NUTRIENT, year) %>%
  summarise(annmean = mean(monthmean), count = n()) %>%
  filter(count >= 3)

# Format fields
f6.tpannualmean <- f6.tpannualmean %>%
  mutate(CharacteristicName = "PHOSPHORUS") %>%
  left_join(ZNUTRIENTSTDS, by = c("NUTRIENT", "CharacteristicName")) %>%
  mutate(CharacteristicName = "TPANNUALMEAN") %>%
  mutate(Exceed = ifelse(annmean > annualmean, "Yes", "No")) %>%
  mutate(ResultSampleFractionText = "Total") %>%
  mutate(aggdate = as.Date("1900-01-01")) %>%
  mutate(NewUse = "Site Specific Nutrient") %>%
  mutate(STDTYPE = "Nutrient") %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  mutate(STD = 99999.9) %>%
  rename(STDNEW = ssm) %>%
  rename(aggtimespace = annmean) %>%
  mutate(STDUSE = "none") %>%
  ungroup() %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# 7 PH

# pH is the only parameter that functions using a range so can have both a min exceedance and max exceedance.

# Open PH STDS.  Has hydrogen ion concentrations for comparison.


# PH Min Exceedances

# Filter for hydrogen ion
# Compare to standards.  Note: Hion is opposite from pH.  So an hion of 0.0001 (ph 4) is below the standard of 0.00001 (pH 5) for DWS
f7.phmin <- f.stdtypejoin %>%
  filter(CharacteristicName == "HIONMIN") %>%
  left_join(ZPHSTDS, by = "NewUse") %>% # Join stds to data
  mutate(Exceedmin = ifelse(HIONMINSTD < aggtimespace, "Yes", "No")) %>% 
  rename(Exceed = Exceedmin) %>% 
  filter(!is.na(Exceed)) %>% 
  mutate(SUBSTANCE_CAS_NO = paste0("Standard between ", phmin, " and ", phmax, "su"))

f7.phmax <- f.stdtypejoin %>%
  filter(CharacteristicName == "HIONMAX") %>%
  left_join(ZPHSTDS, by = "NewUse") %>% # Join stds to data
  mutate(Exceedmin = ifelse(HIONMAXSTD > aggtimespace, "Yes", "No")) %>% 
  rename(Exceed = Exceedmin) %>% 
  filter(!is.na(Exceed)) %>% 
  mutate(SUBSTANCE_CAS_NO = paste0("Standard between ", phmin, " and ", phmax, "su"))

# Combine ph min and max taking the worst case scenario
f7.phall <- bind_rows(f7.phmin, f7.phmax)

# Remove duplicates (the no exceedances are repeated for min and max)
f7.phall <- f7.phall %>% 
  ungroup() %>% 
  select(WBID, aggdate, NewUse, aggtimespace, Exceed, SUBSTANCE_CAS_NO) %>% 
  distinct()

# Standard format
f7.phall <- f7.phall %>% 
  ungroup() %>% 
  mutate(CharacteristicName = "PH") %>% 
  mutate(ResultSampleFractionText = "Total") %>% 
  mutate(agg2 = -log10(aggtimespace)) %>% #use log 10 for ph...log give natural log
  select(-aggtimespace) %>% 
  rename(aggtimespace = agg2) %>% 
  mutate(STDTYPE = "none") %>% 
  mutate(STDUSE = "none") %>% 
  mutate(STD = 999999.9) %>% 
  mutate(STDNEW = 999999.9) %>% 
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) 

# Prioritize exceedances / remove duplicates.  
f7.phall <- f7.phall %>% 
  arrange(WBID, NewUse, aggdate, desc(Exceed)) %>% 
  mutate(concate = paste(WBID, NewUse, aggdate, sep = "-")) %>% # the sort in previous step prioritizes exceedances
  mutate(duplicate = duplicated(concate)) %>% 
  filter(duplicate == "FALSE") %>% 
  select(-duplicate, -concate)

# 8 E coli 

# E coli regular

# E coli regular samples just look at last 3 years of data
f8.ecolireg <- f.stdtypejoin %>%
  filter(CharacteristicName == "ESCHERICHIA COLI" & aggdate > acuteDate) %>%
  filter(NewUse == "FBC" | NewUse == "PBC")

# Bring in the e coli standards
XECOLISTD <- setNames(data.frame(c("FBC", "PBC"), c(235, 575)), c("NewUse","EcoliSTD"))

# Join data to standards
f8.ecolireg <- left_join(f8.ecolireg, XECOLISTD, by = "NewUse")

# Compare to standards
f8.ecolireg <- mutate(f8.ecolireg, Exceedreg = ifelse(aggtimespace > EcoliSTD, "Yes", "No")) 

# Prepare for bind_row
f8.ecolireg <- f8.ecolireg %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  rename(STDNEW = EcoliSTD) %>%
  mutate(STD = 99999.9) %>%
  mutate(STDUSE = "none") %>%
  rename(Exceed = "Exceedreg") %>%
  ungroup() %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# E coli geomean

# Geomean exceedance = at least 4 samples taken in 30 day period. No time/space aggregation so full 5 year window.

# Step one see if any meet criteria
f8.ecoligeo <- filter(d.usejoin, CharacteristicName == "ESCHERICHIA COLI") # no aggregation
f8.ecoligeo <- filter(f8.ecoligeo, Use == "FBC" | Use == "PBC")

# Use median of duplicate.  Basically aggregation by day with time so that duplicate samples don't overweight samples.
f8.ecoligeo <- f8.ecoligeo %>% 
  group_by(WBID, MonitoringLocationIdentifier, ActivityStartTime.Time, aggdate = floor_date(ActivityStartDate, "1 day"), CharacteristicName, ResultSampleFractionText, NewUse) %>%
  summarize(ecoli = median(STDResult))

# Roll up to Waterbody based on Worst Case
f8.ecoligeo <- f8.ecoligeo %>% 
  group_by(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse) %>% 
  summarise(ecoli = max(ecoli))

# Create geomean function...this is from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
geomean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}

# Calculate Geomean
f8.ecoligeo <- f8.ecoligeo %>%
  mutate(month = format(aggdate, "%m"), year = format(aggdate, "%Y")) %>%
  group_by(WBID, NewUse, month, year) %>%
  summarise(monthgeo = geomean(ecoli), count = n()) %>%
  filter(count >= 4) %>%
  mutate(Exceedgeo = ifelse(monthgeo > 126, "Yes", "No")) # Standard is 126 for both PBC and FBC

# Prepare for bind_row
f8.ecoligeo <- f8.ecoligeo %>%
  mutate(CharacteristicName = "ECOLIGEO")   %>% # to distinguish from regular ecoli.  allows combining if impairment determination made  
  mutate(ResultSampleFractionText = "Total") %>%
  rename(aggtimespace = monthgeo) %>%
  mutate(SUBSTANCE_CAS_NO = "none") %>%
  mutate(STDTYPE = "none") %>%
  mutate(STDNEW = 126) %>%
  mutate(STD = 126) %>%
  mutate(STDUSE = "none") %>%
  rename(Exceed = "Exceedgeo") 

# Format
f8.ecoligeo <- f8.ecoligeo %>%
  mutate(aggdate = as.Date("1900-01-01")) %>% 
  ungroup() %>%
  select(WBID, aggdate, CharacteristicName, ResultSampleFractionText, NewUse, aggtimespace, STDTYPE, STDUSE, SUBSTANCE_CAS_NO, STD, STDNEW, Exceed) %>%
  filter(!is.na(Exceed))

# gsh run to here

# JOIN ALL EXCEEDANCE RESULTS
f.joinstdall1 <- bind_rows(f1.stdregular, f2.joinstdoxygen, f3.awcacutegather, f3.awcchronicgather, f3.awwacutegather, f3.awwchronicgather, f3.awedwacutegather, f3.awedwchronicgather, f3.aweacutegather)
f.joinstdall2 <- bind_rows(f4.stdssc, f5.ammoniachronicawc, f5.ammoniachronicaww, f5.ammoniachronicawedw, f5.ammoniaacuteawc, f5.ammoniaacuteaww, f5.ammoniaacuteawedw)

# gsh error
# Error: Can't combine `..1$Exceed` <character> and `..2$Exceed` <logical>.
# mutate exceed to char, - f6.tnannualmean, f6.tpannualmean, 
f6.tnannualmean <- mutate(f6.tnannualmean,
                          Exceed=as.character(Exceed))
f6.tpannualmean <- mutate(f6.tpannualmean,
                          Exceed=as.character(Exceed))


f.joinstdall3 <- bind_rows(f6.tnssm, f6.tnannualmean, f6.tpssm, f6.tpannualmean, f7.phall, f8.ecoligeo, f8.ecolireg)

f.joinstdall <- bind_rows(f.joinstdall1, f.joinstdall2, f.joinstdall3)

f.joinstdall <- left_join(f.joinstdall, XUSECROSSWALK, by = "NewUse")

# Identify all exceedances
f.exceed <- left_join(f.joinstdall, d.dlagg, by = c("WBID", "aggdate", "CharacteristicName", "ResultSampleFractionText", "NewUse"))

# Remove any exceedances that have detection limit issues
f.dlissues <- f.exceed %>% 
  filter(Exceed == "Yes" & countdl > 0) %>% 
  filter(CharacteristicName != "ESCHERICHIA COLI")

# now just exclude values where exceed = Yes and countdl not na.  Just exceedances with no detection limit issues.  
f.exceed <- filter(f.exceed, !Exceed == "Yes" | is.na(countdl))

# Shiny Dashboard
save(f.exceed, file = "shinyDashboardFiles/inputs/ZEXCEED.Rdata")

# F9 TDS.  TDS standards flow weighted annual mean for the Colorado River
# No need to code unless there are actual exceedances.  As on 5/22/2020 there were none.  Would need to be able to pull in flows from NWIS for full automation. 
# Also need to confirm that TDS standards apply to point locations not reaches.
