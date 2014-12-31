# --------------------------- Description ---------------------------

#  Project:          equality  
#  File-Name:        3-data-preparation-final.R                                           
#  Date:             11.12.2014                                   
#  Author:           Mikael Andersson                                      
#  Purpose:          Combine all data and make final preparations
#  Data Output:      Merged WVS and WDI data, as well as test data
#  Input File:       [wvs-data.csv], [wvs-data-inequality-measures.csv], [wdi-data.csv]
#  Output File:      [wvs-data-full.csv], [wvs-country-data.csv], [wvs-data-random-sample.csv], [wvs-test-data.csv], [wvs-test-data-random-sample.csv]
#  Previous file:    [1-data-preparation-wvs.R], [2-data-preparation-wdi.R] 


# --------------------------- Start Matter ---------------------------

# Clear workspace
rm(list = ls())

# Set WD
setwd("~/Dropbox/projects/equality/R")

# Set seed
set.seed(123)

# Load packages
library(reshape)                       # for rename variables more efficiently


# --------------------------- Import Data ---------------------------

# World Value Survey data
wvsData <- read.csv("output/wvs-data.csv")
wvsMeasures <- read.csv("output/wvs-data-inequality-measures.csv")

# World Development Indicators data
wdiData <- read.csv("output/wdi-data.csv")


# --------------------------- Combine WVS and WDI Data ---------------------------

# Combine everything in "wvsData"
wvsData <- merge(wvsData, wdiData, by = "CountryYear", all.x = TRUE)

# Rename variables
wvsData  <- rename(wvsData, c(Country.x="Country"))
wvsData  <- rename(wvsData, c(Year.x="Year"))

# Remove excess variables
wvsData$X.x <- NULL
wvsData$X.y <- NULL
wvsData$X.1 <- NULL
wvsData$Country.y <- NULL
wvsData$Year.y <- NULL

# Add variable with current date
wvsData$DateRun3 <- Sys.Date()


# --------------------------- Create A Data Frame For Country-level Data ---------------------------

# Separate data frame for country-level data
wvsCountryData <- merge(wvsMeasures, wdiData, by = "CountryYear", all.x = TRUE)

# Rename variables
wvsCountryData  <- rename(wvsCountryData, c(Country.x="Country"))
wvsCountryData  <- rename(wvsCountryData, c(Year.x="Year"))

# Remove excess variables
wvsCountryData$X.x <- NULL
wvsCountryData$X.y <- NULL
wvsCountryData$X.1 <- NULL
wvsCountryData$Country.y <- NULL
wvsCountryData$Year.y <- NULL

# --------------------------- Create Test Data ---------------------------

# Only keep wave 5 and 6 to use as test data
wvsTestData <- wvsData[wvsData$Wave == 5 | wvsData$Wave == 6, ]

# Draw a random sample of 10000
wvsRandomSample <- wvsData[sample(1:nrow(wvsData), 10000, replace=FALSE), ]

# Draw a random sample of 10000 from the test data (wave 5 and 6)
wvsTestDataRandomSample <- wvsTestData[sample(1:nrow(wvsTestData), 10000, replace=FALSE), ]


# --------------------------- Write data to files ---------------------------

# Write full data set and country-level data set to csv files
write.csv(wvsData, "output/wvs-data-full", na = "")
write.csv(wvsCountryData, "output/wvs-country-data", na = "")

# Write test data to csv files
write.csv(wvsTestData, "output/wvs-test-data.csv", na = "")
write.csv(wvsRandomSample, "output/wvs-data-random-sample.csv", na = "")
write.csv(wvsTestDataRandomSample, "output/wvs-test-data-random-sample.csv", na = "")

