# Development Status
# 9/30/2020 - gh Calculator code and Dashboard code running to complete
# Need to incorporate mySQL for Google Sheet in 'l extras'


#### ARIZONA's Real Time Assessment Tool - "Ratatoole" ####
### Code by Jason Jones, Arizona Department of Environmental Quality, October 2018 to now() ###
### Code is free to use and distribute.  Proper credit is requested.  
### Contact.  Jason Jones, Arizona Assessment Coordinator, jdj@azdeq.gov, 602-771-2235.

### Purpose: To pull data from WQX, compare it to standards and 
### determine assessment decisions by parameter, use and waterbody

### Setup and configuration

# Clock how long it takes code to run...end time is at the end of the code
start_time <- Sys.time()

# Load Libraries and packages - install package if not already installed
library(dataRetrieval) 
library(tidyverse) 
library(lubridate)
library(RODBC)
library(googlesheets4)

# Connect to AWS RDS 
library(RMariaDB)
library(tm)
# Dialog with user for input
library(svDialogs)
# Connect to AWS s3
# Install - install.packages("aws.s3", repos = "https://cloud.R-project.org")
library(aws.s3)

# Set working directory

# AWS working directory
setwd("~/projects/WaterCalculator/ADEQ")

#Desktop working directory
#setwd("C:/Users/191103/Documents/projects/WaterCalculator/ADEQ")


# Load functions
source("functions/get_csvFrom_s3.R")
source("functions/writeCSV_s3.R")
source("functions/f_makeOracleConnection.R")

# Load configuration
source("config/config.R")
source("config/loadConfigData_CSV.R")



# make a backup copy - originally around line 2263
# gsh? - not sure where to put this?
csvTo_s3(human,s3Bucket,paste("humancopy", format(Sys.Date(), "%Y-%m-%d"), "csv", sep = "."),"humancopies")
# add to write to local directory
# Make a backup
file.copy("human.csv", paste("humancopies/humancopy", format(Sys.Date(), "%Y-%m-%d"), "csv", sep = "."))

# Run modules - A through N

#### A -Load EPA data  ####

if(user.useLocalEPA == "n"){
  if(user.s3EPA == "y"){
    source("modules/a_loadData_s3.R")
  } else {
    source("modules/a_loadData_epa.R")
  }
} else {
  load(file = "inputs/ZAZDATA.Rdata")
  load(file = "inputs/ZAZSITES.Rdata")
}

#### B -  DATA PREPARATION ####
source("modules/b_dataPreparation.R")

#### C - STANDARDIZE DATA ####
source("modules/c_standardizeData.R")

#### D - AGGREGATION BY TIME / ITERATION BY DESIGNATED USE ####
source("modules/d_aggDataTime_designatedUse.R")

#### E - AGGREGATE TO ASSESSMENT UNIT / WBID ####
source("modules/e_agg_assessUnit_WBID.R")

#### F - COMPARE RESULTS TO STANDARDS ####
source("modules/f_results_toStandards.R")

#### G - SUMMARIZE EXCEEDANCES ####
source("modules/g_summarizeExceedances.R")

#### H - ASSESS BY DESIGNATED USE PARAMETER AND WATERBDOY ####
source("modules/h_assessDesig_use_param_wbid.R")

#### I - ASSESS BY USE AND WATERBODY or PARAMETER AND WATERBODY ####
source("modules/i_assess_use_wbid_or_param_wbid.R")

#### J - METRICS ####
source("modules/j_metrics.R")

#### K - QUALITY ASSURANCE ####
source("modules/k_qualityAssurance.R")

#### L Charts ####
source("modules/l_charts.R")

#### L Extras ####
source("modules/l_extras.R")

# #### TEMP FISH ####
#source("modules/m_Fish.R")

## CLEAN UP - Close Connection to DB ##
source("modules/n_closeDB.R")
