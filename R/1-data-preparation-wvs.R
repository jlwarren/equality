# --------------------------- Description ---------------------------

#  Project:          equality  
#  File-Name:        1-data-preparation-wvs.R                                           
#  Date:             11.12.2014                                   
#  Author:           Mikael Andersson                                      
#  Purpose:          Prepare World Value Survey ("WVS") data (downloadable from the WVS website) for analysis  
#  Data Output:      WVS data ready for analysis, including "subjective based" measures of inequality
#  Input File:       [WVS_Longitudinal_1981-2014_rdata_v_2014_11_25.rdata]                               
#  Output File:      [wvs-data.csv], [wvs-data-inequality-measures.csv], [wvs-country-year.csv]
#  Previous file:    [none]     


# --------------------------- Start Matter ---------------------------

# Clear workspace
rm(list = ls())

# Set WD
setwd("~/Dropbox/projects/equality/R")

# Set seed
set.seed(123)

# Load packages
library(plyr)                       # for revaluing more efficiently
library(ineq)                       # for computing subjective aggregate inequality measures

# Define parsing functions
reverse <- function(x) {
  # Reverse a variable so that the first value is last, and so on.
  #
  # Args:
  #   x: list of vectors to be reversed.
  #
  # Returns:
  #   The reversed values of x.
  xTemp <- min(x, na.rm = TRUE) + max(x, na.rm = TRUE) - x
  return(xTemp)
}


# --------------------------- Import Data ---------------------------

# Load WVS data from the raw longitudinal rdata file downloaded from the WVS website
load("input/WVS_Longitudinal_1981-2014_rdata_v_2014_11_25.rdata")

# Rename data frame
wvsData <- `WVS_Longitudinal_1981-2014_rdata_v_2014_11_25`
rm(`WVS_Longitudinal_1981-2014_rdata_v_2014_11_25`)


# --------------------------- Keep Relevant Data ---------------------------

# The first wave does not include the relevant questions on equality and redistribution
wvsData <- wvsData[wvsData$S002 != 1, ]

# Keep variables of interest
wvsDataVariables <- c("S002", "S003", "S007", "S020", "S025", "X001", "X002", "X003", "X007", "X011", "X025", "X028", "X036", "X045", "X045B", "X047", "x047b", "X047R", "X049", "X051", "Y001", "X052", "C006", "A005", "A008", "A009", "A165", "A170", "A173", "F034", "E023", "E033", "E025", "E026", "E027", "E035", "E036", "E037", "E039", "E040", "E001", "X047CS")
wvsData <- wvsData[, wvsDataVariables]

# Rename variables
wvsDataNames <- c("Wave", "Country", "ID", "Year", "CountryYear", "Female" ,"YearBorn", "Age", "MaritalStatus", "Children", "HighestEducation", "EmploymentStatus", "Profession", "SubjectiveSocialClass", "SubjectiveSocialClassWAVE2", "IncomeScale", "MonthlyIncome", "IncomeLevel", "TownSize", "EthnicGroup", "PostMaterialistIndex", "EmploymentInstitution", "FinancialSatisfaction", "WorkImportant", "FeelingHappy", "HealthStatus", "Trust", "LifeSatisfaction", "FreedomOfChoice", "Religious", "PoliticalInterest", "SubjectiveLeftRight", "ActSignedPetition", "ActBoycotted", "ActDemonstrated", "MoreEconomicEquality", "MoreGovernmentOwnership", "MoreGovernmentResponsibility", "CompetitionGood", "WorkBringsSuccess", "CountryShouldAimFor", "Income")
names(wvsData) <- wvsDataNames


# --------------------------- Recode and Create Variables ---------------------------

# Recode missing values as NA
missingValues <- c(-99, -1, -2, -3, -4, -5)
for (missingValue in missingValues) {
  wvsData[wvsData == missingValue] <- NA
  print(missingValue)                             # as overview of loop-progress
}

# Country
wvsData$Country <- as.factor(wvsData$Country)
wvsData$Country <- revalue(wvsData$Country, c("8"="Albania", "12"="Algeria", "16"="American Samoa", "20"="Andorra", "24"="Angola", "28"="Antigua", "31"="Azerbaijan", "32"="Argentina", "36"="Australia", "40"="Austria", "48"="Bahrain", "50"="Bangladesh", "51"="Armenia", "52"="Barbados", "56"="Belgium", "60"="Bermuda", "64"="Bhutan", "68"="Bolivia", "70"="Bosnia", "72"="Botswana", "76"="Brazil", "84"="Belize", "100"="Bulgaria", "104"="Myanmar", "108"="Burundi", "112"="Belarus", "116"="Cambodia", "120"="Cameroon", "124"="Canada", "144"="Sri Lanka", "148"="Chad", "152"="Chile", "156"="China", "158"="Taiwan", "170"="Colombia", "180"="DR Congo", "184"="Cook Islands", "188"="Costa Rica", "191"="Croatia", "192"="Cuba", "196"="Cyprus (G)", "197"="Cyprus (T)", "203"="Czech Rep", "208"="Denmark", "214"="Dominican Rep", "218"="Ecuador", "222"="El Salvador", "226"="EqGuinea", "231"="Ethiopia", "232"="Eritrea", "233"="Estonia", "246"="Finland", "250"="France", "268"="Georgia", "270"="Gambia", "275"="Palestine", "276"="Germany", "288"="Ghana", "292"="Gibraltar", "300"="Greece", "320"="Guatemala", "324"="Guinea", "328"="Guyana", "332"="Haiti", "340"="Honduras", "344"="Hong Kong", "348"="Hungary", "352"="Iceland", "356"="India", "360"="Indonesia", "364"="Iran", "368"="Iraq", "372"="Ireland", "376"="Israel", "380"="Italy", "384"="Côte dIvoire", "388"="Jamaica", "392"="Japan", "398"="Kazakhstan", "400"="Jordan", "404"="Kenya", "408"="North Korea", "410"="South Korea", "414"="Kuwait", "417"="Kyrgyzstan", "418"="Laos", "422"="Lebanon", "426"="Lesotho", "428"="Latvia", "430"="Liberia", "434"="Libya", "438"="Liechtenstein", "440"="Lithuania", "442"="Luxembourg", "450"="Madagascar", "454"="Malawi", "458"="Malaysia", "466"="Mali", "470"="Malta", "474"="Martinique", "478"="Mauritania", "480"="Mauritius", "484"="Mexico", "492"="Monaco", "496"="Mongolia", "498"="Moldova", "504"="Morocco", "508"="Mozambique", "512"="Oman", "516"="Namibia", "524"="Nepal", "528"="Netherlands", "554"="New Zealand", "558"="Nicaragua", "562"="Niger", "566"="Nigeria", "578"="Norway", "586"="Pakistan", "591"="Panama", "598"="Papua New Guinea", "600"="Paraguay", "604"="Peru", "608"="Philippines", "616"="Poland", "620"="Portugal", "624"="Guinea-Bissau", "626"="Timor-Leste", "630"="Puerto Rico", "634"="Qatar", "642"="Romania", "643"="Russia", "646"="Rwanda", "682"="Saudi Arabia", "686"="Senegal", "690"="Seychelles", "694"="Sierra Leone", "702"="Singapore", "703"="Slovakia", "704"="Viet Nam", "705"="Slovenia", "706"="Somalia", "710"="South Africa", "716"="Zimbabwe", "724"="Spain", "736"="Sudan", "740"="Suriname", "752"="Sweden", "756"="Switzerland", "760"="Syria", "762"="Tajikistan", "764"="Thailand", "768"="Togo", "780"="Trinidad and Tobago", "784"="United Arab Emirates", "788"="Tunisia", "792"="Turkey", "795"="Turkmenistan", "800"="Uganda", "804"="Ukraine", "807"="Macedonia", "818"="Egypt", "826"="Great Britain", "834"="Tanzania", "840"="United States", "850"="US Virgin Islands", "854"="Burkina Faso", "858"="Uruguay", "860"="Uzbekistan", "862"="Venezuela", "887"="Yemen", "891"="Serbia and Montenegro", "894"="Zambia", "900"="West Germany", "901"="East Germany", "902"="Tambov", "903"="Moscow", "904"="Basque Country", "906"="Andalusia", "907"="Galicia", "909"="North Ireland", "910"="Valencia", "911"="Serbia", "912"="Montenegro", "913"="Srpska Republic", "914"="Bosnian Federation", "915"="Kosovo"))
wvsData$Country <- as.character(wvsData$Country) # in order to remove "factor levels" without any observations
wvsData$Country <- as.factor(wvsData$Country)
Countries <- levels(wvsData$Country)
table(wvsData$Country)

# CountryYear
wvsData$CountryYear <- as.factor(wvsData$CountryYear)
countryYearNumbers <- seq(from = 1, to = 231)
levels(wvsData$CountryYear) <- countryYearNumbers
countryYearNumbers <- as.character(countryYearNumbers)

# Female
wvsData$Female[wvsData$Female == 1] <- 0
wvsData$Female[wvsData$Female == 2] <- 1
#wvsData$Female <- as.factor(wvsData$Female)
#wvsData$Female <- revalue(wvsData$Female, c("0" = "Male", "1" = "Female"))

# Married
wvsData$Married <- wvsData$MaritalStatus
wvsData$MaritalStatus <- NULL
wvsData$Married[wvsData$Married == 10] <- 1
wvsData$Married[wvsData$Married == 2] <- 0
wvsData$Married[wvsData$Married == 3] <- 0
wvsData$Married[wvsData$Married == 4] <- 0
wvsData$Married[wvsData$Married == 5] <- 0
wvsData$Married[wvsData$Married == 6] <- 0
wvsData$Married[wvsData$Married == 7] <- 0
wvsData$Married[wvsData$Married == 8] <- 0
#wvsData$Married <- as.factor(wvsData$Married)
#wvsData$Married <- revalue(wvsData$Married, c("0" = "Not Married", "1" = "Married"))

# Children
wvsData$Children[wvsData$Children == 2] <- 1
wvsData$Children[wvsData$Children == 3] <- 1
wvsData$Children[wvsData$Children == 4] <- 1
wvsData$Children[wvsData$Children == 5] <- 1
wvsData$Children[wvsData$Children == 6] <- 1
wvsData$Children[wvsData$Children == 7] <- 1
wvsData$Children[wvsData$Children == 8] <- 1
#wvsData$Children <- as.factor(wvsData$Children)
#wvsData$Children <- revalue(wvsData$Children, c("0" = "Dont have children", "1" = "Have children"))

# HigherEducation
wvsData$HigherEducation <- wvsData$HighestEducation
wvsData$HighestEducation <- NULL
wvsData$HigherEducation[wvsData$HigherEducation == 1] <- 0
wvsData$HigherEducation[wvsData$HigherEducation == 2] <- 0
wvsData$HigherEducation[wvsData$HigherEducation == 3] <- 0
wvsData$HigherEducation[wvsData$HigherEducation == 4] <- 0
wvsData$HigherEducation[wvsData$HigherEducation == 5] <- 0
wvsData$HigherEducation[wvsData$HigherEducation == 6] <- 0
wvsData$HigherEducation[wvsData$HigherEducation == 7] <- 1
wvsData$HigherEducation[wvsData$HigherEducation == 8] <- 1
#wvsData$HigherEducation <- as.factor(wvsData$HigherEducation)
#wvsData$HigherEducation <- revalue(wvsData$HigherEducation, c("0" = "Not higher education", "1" = "Higher education"))

# Employed
wvsData$Employed <- wvsData$EmploymentStatus
wvsData$EmploymentStatus <- NULL
wvsData$Employed[wvsData$Employed == 1] <- 1
wvsData$Employed[wvsData$Employed == 2] <- 1
wvsData$Employed[wvsData$Employed == 3] <- 1
wvsData$Employed[wvsData$Employed == 4] <- 0
wvsData$Employed[wvsData$Employed == 5] <- 0
wvsData$Employed[wvsData$Employed == 6] <- 0
wvsData$Employed[wvsData$Employed == 7] <- 0
wvsData$Employed[wvsData$Employed == 8] <- 0
#wvsData$Employed <- as.factor(wvsData$Employed)
#wvsData$Employed <- revalue(wvsData$Employed, c("0" = "Not employed", "1" = "Employed"))

# Employer
wvsData$Employer <- wvsData$Profession
wvsData$Profession <- NULL
wvsData$Employer[wvsData$Employer < 20] <- 1
wvsData$Employer[wvsData$Employer > 20] <- 0
#wvsData$Employer <- as.factors(wvsData$Employer)
#wvsData$Employer <- revalues(wvsData$Employer, c("0" = "Not employer", "1" = "Employer"))

# SubjectiveSocialClass
wvsData$SubjectiveSocialClassWAVE2[wvsData$SubjectiveSocialClassWAVE2 == 3] <- 2 # wave 2 have one more category, "middle middle class", which is merged with "Upper middle class"
wvsData$SubjectiveSocialClass <- ifelse(wvsData$Wave == 2, wvsData$SubjectiveSocialClassWAVE2, wvsData$SubjectiveSocialClass)
wvsData$SubjectiveSocialClassWAVE2 <- NULL

# EmployedPublicInstitution
wvsData$EmployedPublicInstitution <- wvsData$EmploymentInstitution
wvsData$EmploymentInstitution <- NULL
wvsData$EmployedPublicInstitution[wvsData$EmployedPublicInstitution == "2"] <- 0
wvsData$EmployedPublicInstitution[wvsData$EmployedPublicInstitution == "3"] <- 0
wvsData$EmployedPublicInstitution[wvsData$EmployedPublicInstitution == "4"] <- 0
#wvsData$EmployedPublicInstitution <- as.factor(wvsData$EmployedPublicInstitution)
#wvsData$EmployedPublicInstitution <- revalues(wvsData$EmployedPublicInstitution, c("0" = "Not employed at public institution", "1" = "Employed at public institution"))

# Trust
wvsData$Trust[wvsData$Trust == "2"] <- 0
#wvsData$Trust <- as.factor(wvsData$Trust)
#wvsData$Trust <- revalues(wvsData$Trust, c("0" = "Cant be too careful", "1" = "Most people can be trusted"))

# Religious
wvsData$Religious[wvsData$Religious == "2"] <- 0
wvsData$Religious[wvsData$Religious == "3"] <- 0
wvsData$Religious[wvsData$Religious == "4"] <- 0
#wvsData$Religious <- as.factor(wvsData$Religious)
#wvsData$Religious <- revalues(wvsData$Religious, c("0" = "Not religious", "1" = "Religious"))

# CountryShouldFocusOnGrowth
wvsData$CountryShouldFocusOnGrowth <- wvsData$CountryShouldAimFor
wvsData$CountryShouldAimFor <- NULL
wvsData$CountryShouldFocusOnGrowth[wvsData$CountryShouldFocusOnGrowth == "2"] <- 0
wvsData$CountryShouldFocusOnGrowth[wvsData$CountryShouldFocusOnGrowth == "3"] <- 0
wvsData$CountryShouldFocusOnGrowth[wvsData$CountryShouldFocusOnGrowth == "4"] <- 0
#wvsData$CountryShouldFocusOnGrowth <- as.factor[wvsData$CountryShouldFocusOnGrowth]
#wvsData$CountryShouldFocusOnGrowth <- revalues(wvsData$CountryShouldFocusOnGrowth, c("0" = "Growth is not the most important aim", "1" = "Growth is the most important aim"))

# ActedPolitical
wvsData$ActSignedPetition[wvsData$ActSignedPetition == "2"] <- 0 # recode variables to 0/1 indicating whether the resp did the act
wvsData$ActSignedPetition[wvsData$ActSignedPetition == "3"] <- 0
wvsData$ActBoycotted[wvsData$ActBoycotted == "2"] <- 0
wvsData$ActBoycotted[wvsData$ActBoycotted == "3"] <- 0
wvsData$ActDemonstrated[wvsData$ActDemonstrated == "2"] <- 0
wvsData$ActDemonstrated[wvsData$ActDemonstrated == "3"] <- 0
wvsData$ActedPolitical <- wvsData$ActSignedPetition + wvsData$ActBoycotted + wvsData$ActDemonstrated # index from 0-3 measuring how much politcally--as in political act--active the respondent is
wvsData$ActSignedPetition <- NULL
wvsData$ActBoycotted <- NULL
wvsData$ActDemonstrated <- NULL

# Reverse variables where the highest value is non-intuitive
wvsData$WorkImportant <- reverse(wvsData$WorkImportant)                 # higher value = more important
wvsData$FeelingHappy <- reverse(wvsData$FeelingHappy)                   # higher value = more happy
wvsData$HealthStatus <- reverse(wvsData$HealthStatus)                   # higher value = better health
wvsData$PoliticalInterest <- reverse(wvsData$PoliticalInterest)         # higher value = more interest
wvsData$MoreEconomicEquality <- reverse(wvsData$MoreEconomicEquality)   # higher value = more equality
wvsData$CompetitionGood <- reverse(wvsData$CompetitionGood)             # higher value = competition is better

# Add variable with current date
wvsData$DateRun1 <- Sys.Date()


# --------------------------- Compute Subjective Inequality Measures ---------------------------

# Create a data frame to contain measures based on each country-year
wvsData$CountryYear <- as.numeric(wvsData$CountryYear)
wvsMeasures <- unique(wvsData[, c(2,4,5)])
wvsMeasures$SubjectiveGini <- NA
wvsMeasures$SubjectiveRS <- NA
wvsMeasures$SubjectiveAtkinson <- NA
wvsMeasures$SubjectiveKolm <- NA
wvsMeasures$SubjectiveVariation <- NA
wvsMeasures$SubjectiveEntropy <- NA

# Compute measures
for (i in 1:nrow(wvsMeasures)) {
  # Extract income data from specific country/year
  wvsTemp <- wvsData[-c(which(wvsData$CountryYear != i)), ]
  incomeMeasure <- wvsTemp$IncomeScale
  
  # Compute each measure in turn
  gini <- Gini(incomeMeasure, corr = FALSE, na.rm = TRUE)
  rs <- RS(incomeMeasure, na.rm = TRUE)
  atkinson <- Atkinson(incomeMeasure, parameter = NULL, na.rm = TRUE)
  theil <- Theil(incomeMeasure, parameter = NULL, na.rm = TRUE)
  kolm <- Kolm(incomeMeasure, parameter = NULL, na.rm = TRUE)
  var <- var(incomeMeasure, na.rm = TRUE)
  entropy <- entropy(incomeMeasure, parameter = NULL, na.rm = TRUE)
  
  # Input measures into the data frame
  wvsMeasures$SubjectiveGini <- ifelse(wvsMeasures$CountryYear == i, as.character(gini), as.character(wvsMeasures$SubjectiveGini))
  wvsMeasures$SubjectiveRS <- ifelse(wvsMeasures$CountryYear == i, as.character(rs), as.character(wvsMeasures$SubjectiveRS))
  wvsMeasures$SubjectiveAtkinson <- ifelse(wvsMeasures$CountryYear == i, as.character(atkinson), as.character(wvsMeasures$SubjectiveAtkinson))
  wvsMeasures$SubjectiveTheil <- ifelse(wvsMeasures$CountryYear == i, as.character(theil), as.character(wvsMeasures$SubjectiveTheil))
  wvsMeasures$SubjectiveKolm <- ifelse(wvsMeasures$CountryYear == i, as.character(kolm), as.character(wvsMeasures$SubjectiveKolm))
  wvsMeasures$SubjectiveVariation <- ifelse(wvsMeasures$CountryYear == i, as.character(var), as.character(wvsMeasures$SubjectiveVariation))
  wvsMeasures$SubjectiveEntropy <- ifelse(wvsMeasures$CountryYear == i, as.character(entropy), as.character(wvsMeasures$SubjectiveEntropy))
                                       
  # Print country-year as overview of loop-progress
  print(i)
}

# Merge measures with WVS data
wvsData <- merge(wvsData, wvsMeasures, by = "CountryYear")
wvsData <- rename(wvsData, c(Country.x="Country"))
wvsData <- rename(wvsData, c(Year.x="Year"))
wvsData$Year.y <- NULL
wvsData$Country.y <- NULL


# --------------------------- Compute Satisfaction Inequality Measures ---------------------------

# Create a data frame to contain measures based on each country-year
wvsMeasures2 <- unique(wvsData[, c(3,5,1)])
wvsMeasures2$SatisfactionGini <- NA
wvsMeasures2$SatisfactionRS <- NA
wvsMeasures2$SatisfactionAtkinson <- NA
wvsMeasures2$SatisfactionKolm <- NA
wvsMeasures2$SatisfactionVariation <- NA
wvsMeasures2$SatisfactionEntropy <- NA

# Compute measures
for (i in 1:nrow(wvsMeasures2)) {
  # Extract income data from specific country/year
  wvsTemp <- wvsData[-c(which(wvsData$CountryYear != i)), ]
  incomeMeasure <- wvsTemp$FinancialSatisfaction
  
  # Compute each measure in turn
  gini <- Gini(incomeMeasure, corr = FALSE, na.rm = TRUE)
  rs <- RS(incomeMeasure, na.rm = TRUE)
  atkinson <- Atkinson(incomeMeasure, parameter = NULL, na.rm = TRUE)
  theil <- Theil(incomeMeasure, parameter = NULL, na.rm = TRUE)
  kolm <- Kolm(incomeMeasure, parameter = NULL, na.rm = TRUE)
  var <- var(incomeMeasure, na.rm = TRUE)
  entropy <- entropy(incomeMeasure, parameter = NULL, na.rm = TRUE)
  
  # Input measures into the data frame
  wvsMeasures2$SatisfactionGini <- ifelse(wvsMeasures2$CountryYear == i, as.character(gini), as.character(wvsMeasures2$SatisfactionGini))
  wvsMeasures2$SatisfactionRS <- ifelse(wvsMeasures2$CountryYear == i, as.character(rs), as.character(wvsMeasures2$SatisfactionRS))
  wvsMeasures2$SatisfactionAtkinson <- ifelse(wvsMeasures2$CountryYear == i, as.character(atkinson), as.character(wvsMeasures2$SatisfactionAtkinson))
  wvsMeasures2$SatisfactionTheil <- ifelse(wvsMeasures2$CountryYear == i, as.character(theil), as.character(wvsMeasures2$SatisfactionTheil))
  wvsMeasures2$SatisfactionKolm <- ifelse(wvsMeasures2$CountryYear == i, as.character(kolm), as.character(wvsMeasures2$SatisfactionKolm))
  wvsMeasures2$SatisfactionVariation <- ifelse(wvsMeasures2$CountryYear == i, as.character(var), as.character(wvsMeasures2$SatisfactionVariation))
  wvsMeasures2$SatisfactionEntropy <- ifelse(wvsMeasures2$CountryYear == i, as.character(entropy), as.character(wvsMeasures2$SatisfactionEntropy))
                                       
  # Print country-year as overview of loop-progress
  print(i)
}

# Merge measures with WVS data and WVS subjective income inequality
wvsData <- merge(wvsData, wvsMeasures2, by = "CountryYear")
wvsData <- rename(wvsData, c(Country.x="Country"))
wvsData <- rename(wvsData, c(Year.x="Year"))
wvsData$Year.y <- NULL
wvsData$Country.y <- NULL

# Merge measures with subjective income inequality measures
wvsMeasures <- merge(wvsMeasures, wvsMeasures2, by = "CountryYear")
wvsMeasures <- rename(wvsMeasures, c(Country.x = "Country"))
wvsMeasures <- rename(wvsMeasures, c(Year.x = "Year"))
wvsMeasures$Year.y <- NULL
wvsMeasures$Country.y <- NULL


# --------------------------- Create Data Frame Including Countries and Years ---------------------------

# Extract meta data about countries and years
wvsCountryYear <- unique(wvsData[, c(1,3,5)])


# --------------------------- Write Data to Files ---------------------------

# Write wvs data to csv file
write.csv(wvsData, "output/wvs-data.csv", na = "")

# Write subjective-based inequality measures to separate file
write.csv(wvsMeasures, "output/wvs-data-inequality-measures.csv", na = "")

# Write country-year data to R file
write.csv(wvsCountryYear, file = "output/wvs-country-year.csv", na = "")
