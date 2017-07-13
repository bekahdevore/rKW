## Variables
## GET BLS QCEW DATA VARIABLES
qcewDataLouisville <- getQcewData(quarter, currentYear)

## GET ACS DATA VARIABLES
##Info about ACS 1 yr API https://www.census.gov/data/developers/data-sets/acs-1year.html
## variables https://api.census.gov/data/2015/acs1/subject/variables.json
medianHouseholdWage <- "DP03_0062E" # profile
population <- "S0101_C01_001E" # subject
medianHomeValue <- "DP04_0089E" #  profile
medianMonthlyRent <- "DP04_0126PE" # profile
laborForceParticipationRate <- "S2301_C02_001E" #subject
POPULATION_2011 

# ACS API Key
apiKey <- "00b1974d78394a0f553ab06d7d20f58d9fee6e51"

# ACS AREAS
acsMetros <- "&for=metropolitan+statistical+area/micropolitan+statistical+area:*"
acsKentucky <- "&for=state:21"
acsUS <- "&for=us:*"


currentYear <- as.numeric(format(Sys.Date(), "%Y"))
quarter <-  4





