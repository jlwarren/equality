# --------------------------- Description ---------------------------

#  Project:          equality  
#  File-Name:        2-data-preparation-wdi.R                                           
#  Date:             11.12.2014                                   
#  Author:           Mikael Andersson                                      
#  Purpose:          Download and prepare World Development Indicator (WDI) Data from the World Bank         
#  Data Output:      WDI data ready to be combined with the WVS data and used for analysis
#  Input File:       [wvs-country-year.csv]                             
#  Output File:      [wdi-data.csv]
#  Previous file:    [1-data-preparation-wvs.R]


# --------------------------- Start Matter ---------------------------

# Clear workspace
rm(list = ls())

# Set WD
setwd("~/Dropbox/projects/equality/R")

# Set seed
set.seed(123)

# Load packages
library(WDI)                       # package for downloading Word Development Index (WDI) data from the World Bank
library(countrycode)               # package for converting country names into iso-2 characters used by the WDI package

# Update WDI list 
#WDIcache()                        # the WDI package only ships with information on series available mid-2012 so update is need (once)


# --------------------------- Import WVS Meta Data ---------------------------

# WVS Country/Year Data frame
wvsCountryYear <- read.csv("output/wvs-country-year.csv")

# Vector for WVS data Countries
countries <- levels(wvsCountryYear$Country)
countriesIso <- countrycode(countries, "country.name", "iso2c") # convert names to iso-2 abbr.

# Vector for WVS data time span
firstYear <- min(wvsCountryYear$Year)
lastYear <- max(wvsCountryYear$Year)


# --------------------------- Download WDI Data ---------------------------

# Define World Development Indicators to be downloaded
Indicators <- c("NY.GDP.MKTP.PP.KD", # GDP, PPP (constant 2011 international $)
                "NY.GDP.PCAP.PP.CD", # GDP per capita, PPP (constant 2011 international $)
                "NY.GNP.MKTP.PP.KD", # GNI, PPP (constant 2011 international $) 
                "NY.GNP.PCAP.PP.KD", # GNI per capita, PPP (constant 2011 international $)
                "NY.GDP.MKTP.KD.ZG", # GDP growth (annual %)
                "NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                "IT.NET.BBND.P2", # Fixed broadband Internet subscribers (per 100 people)
                "IT.NET.USER.P2", # Internet users (per 100 people)
                "SL.EMP.VULN.ZS", # Vulnerable employment, total (% of total employment)
                "SL.EMP.TOTL.SP.ZS", # Employment to population ratio, 15+, total (%) (modeled ILO estimate)
                "SL.TLF.ACTI.ZS", # Labor force participation rate, total (% of total population ages 15-64) (modeled ILO estimate)
                "SM.POP.REFG.OR", # Refugee population by country or territory of origin
                "lm_ub.gen_pop", # Generosity of unemployment benefits and ALMP (% of total welfare of beneficiary households)
                "lm_ub.cov_pop", # Coverage of unemployment benefits and ALMP (% of population)
                "allsp.gen_pop", # Generosity of social protection and labor programs (% of total welfare of beneficiary households)
                "allsp.cov_pop", # Coverage of social protection and labor programs (% of population)
                "IQ.CPA.PROT.XQ", # CPIA social protection rating (1=low to 6=high)
                "SL.UEM.LTRM.ZS", # Long-term unemployment (% of total unemployment)
                "SL.UEM.TOTL.ZS", # Unemployment, total (% of total labor force) (modeled ILO estimate)
                "SI.POV.GINI", # GINI index
                "SI.POV.2DAY", # Poverty headcount ratio at $2 a day (PPP) (% of population)
                "SI.POV.GAP2", # Poverty gap at $2 a day (PPP) (%)
                "SI.POV.RUHC", # Rural poverty headcount ratio at national poverty lines (% of rural population)
                "SI.SPR.PCAP", # Survey mean consumption or income per capita, total population (2005 PPP $ per day)
                "SI.SPR.PC40", # Survey mean consumption or income per capita, bottom 40% of population (2005 PPP $ per day)
                "IC.TAX.TOTL.CP.ZS", # Total tax rate (% of commercial profits)
                "IC.FRM.CRIM.ZS", # Losses due to theft, robbery, vandalism, and arson (% sales)
                "VC.IDP.TOTL.HE", # Internally displaced persons (number, high estimate)
                "VC.IDP.TOTL.LE", # Internally displaced persons (number, low estimate)
                "VC.PKP.TOTL.UN", # Presence of peace keepers (number of troops, police, and military observers in mandate)
                "GC.DOD.TOTL.GD.ZS", # Central government debt, total (% of GDP)
                "GC.REV.SOCL.ZS", # Social contributions (% of revenue)
                "SE.PRM.UNER", # Children out of school, primary
                "SE.TER.ENRR", # School enrollment, tertiary (% gross)
                "SE.PRM.ENRR", # School enrollment, primary (% gross)
                "SE.PRM.DURS", # Primary education, duration (years)
                "SE.ADT.LITR.ZS", # Literacy rate, adult total (% of people ages 15 and above)
                "EN.POP.DNST", # Population density (people per sq. km of land area)
                "SP.RUR.TOTL.ZS", # Rural population (% of total population)
                "EG.ELC.ACCS.ZS", # Access to electricity (% of population)
                "DT.ODA.OATL.CD", # Net official aid received (current US$)
                "DT.ODA.ODAT.CD", # Net official development assistance received (current US$)
                "FR.INR.RINR", # Real interest rate (%)
                "FR.INR.LEND", # Lending interest rate (%)
                "SH.XPD.EXTR.ZS", # External resources for health (% of total expenditure on health)
                "SH.XPD.OOPC.TO.ZS", # Out-of-pocket health expenditure (% of total expenditure on health)
                "SP.DYN.LE00.IN", # Life expectancy at birth, total (years)
                "SN.ITK.DEFC.ZS", # Prevalence of undernourishment (% of population)
                "SP.POP.TOTL", # Population, total
                "SH.DTH.COMM.ZS", # Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)
                "UNDP.HDI.HY.XD", # Inequality-adjusted Human Development Index (IHDI)
                "allsp.inc_gini") # Gini inequality reduction (%) -  All Social Protection



# Download WDI data from the World Bank via the WDI package
wdiData <- WDI(country = countriesIso, indicator = Indicators, start = firstYear, end = lastYear, extra = FALSE, cache = NULL)

# Rename variables
wdiDataNames <- c("CountryIso2", "Country", "Year", "GDP", "GDPpc", "GNI", "GNIpc", "GDPGrowth", "GDPpcGrowth", "BroadbandSubscribers", "InternetUsers", "VulnerableEmployment", "EmploymentRatio", "LaborForceParticipation", "Refugees", 
                  # "BenefitsGenerosity", "BenefitsCoverage", "SocialProtectionGenerosity", "CoverageSocialProtection", 
                  "CPIARating", "LongTermUnemployment", "Unemployement", "Gini", "PovertyRatio", "PovertyGap", "RuralPoverty", "MeanConsumption", "MeanConsumptionBottom40", "CommercialTaxRate", "MaliciousLosses", "DisplacedPersonsHigh", "DisplacesPersonsLow", "PeaceKeepers", "GovernmentDebt", "SocialContribution", "ChildrenOutOfSchool", "EnrollmentTertiary", "EnrollmentPrimary", "EducationDuration", "LiteracyRate", "PopulationDensity", "RuralPopulation", "ElectricityAccess", "AidRecieved", "AssistanceRecieved", "InterestRate", "LendingInterestRate", "ExternalHealthResources", "PrivateHealthExpenditure", "LifeExpentency", "Undernourishment", "Population", "DeathByDiseases"
                  # , "IHDIIndex", "GiniReducationSocial"
                  )
names(wdiData) <- wdiDataNames
  
       
# --------------------------- Prepare WDI Data ---------------------------

# Only keep data for the countries and years in the WVS data
wdiData <- merge(wdiData, wvsCountryYear, by = c("Country", "Year"))

# Remove excess variables
wdiData$CountryIso2 <- NULL
wdiData$X.x <- NULL
wdiData$X.y <- NULL
wdiData$freq.x <- NULL
wdiData$freq.y <- NULL


# ---------------------------  Write Data To File ---------------------------

# Write WDI data to csv file
write.csv(wdiData, "output/wdi-data.csv", na = "")

