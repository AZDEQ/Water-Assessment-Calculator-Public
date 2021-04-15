# Load Historic Data for the 2018 and 2020 cycle (7/1/2012 to 6/30/2016)
load(file = "inputs/Z2014TO2016_AZDATA.Rdata") # note that this opens as a.azdata
a.azdata2014to2016 <- a.azdata


load(file = "inputs/Z2014TO2016_AZSITES.Rdata") # note that this opens as a.azsites
a.azsites2014to2016 <- a.azsites


load(file = "inputs/Z2012TO2014_AZDATA.Rdata") # note that this opens as a.azdata
a.azdata2012to2014 <- a.azdata


load(file = "inputs/Z2012TO2014_AZSITES.Rdata") # note that this opens as a.azsites
a.azsites2012to2014 <- a.azsites

# bind 2022 to 2014-2016 and 2012-2014
a.azdata <- rbind(a.azdata_current, a.azdata2014to2016,a.azdata2012to2014)
a.azsites <- rbind(a.azsites_current,a.azsites2014to2016,a.azsites2012to2014)


#testing Filter
a.azdata <- a.azdata %>%
  filter(ActivityStartDate >= as.Date("2016-07-01"))

a.azsites <- a.azsites %>%
  filter(ActivityStartDate >= as.Date("2016-07-01"))

