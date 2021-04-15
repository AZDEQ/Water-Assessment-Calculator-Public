#### G - SUMMARIZE EXCEEDANCES ####



# Summarize exceedances
g.excsum <- f.exceed %>%
  group_by(WBID, CharacteristicName, ResultSampleFractionText, NewUse, Exceed) %>%
  summarize(samplecount = n())

# Spread so can count samples
g.excsum <- spread(g.excsum, Exceed, samplecount)

# Calculate number of samples (aggregated)
g.excsum <- g.excsum %>%
  rowwise() %>%
  mutate(sampcount = sum(No,Yes, na.rm = TRUE)) %>% #na.rm handles any NA's
  select(WBID, CharacteristicName, ResultSampleFractionText, NewUse, No, Yes, sampcount)

