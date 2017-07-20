############# VARIABLES

# ACS API Key
apiKey <- "00b1974d78394a0f553ab06d7d20f58d9fee6e51"

##  ACS DATA VARIABLES
##Info about ACS 1 yr API https://www.census.gov/data/developers/data-sets/acs-1year.html
##  subject variables: https://api.census.gov/data/2015/acs1/subject/variables.json 
## profile variables:   https://api.census.gov/data/2015/acs1/profile/variables.json 
medianHouseholdWage <- "DP03_0062E" # profile
population <- "S0101_C01_001E" # subject
medianHomeValue <- "DP04_0089E" #  profile
medianMonthlyRent <- "DP04_0134E" # profile
laborForceParticipationRate <- "S2301_C02_001E" #subject

# ACS AREAS
acsMetros <- "metropolitan+statistical+area/micropolitan+statistical+area:*"
acsKentucky <- "state:21"
acsUS <- "us:*"

## LAUS COLUMN NAMES
unemploymentRateName <- "Unemployment Rate"
laborForceSizeName <- "Labor Force Size"

# BLS AREAS 
blsUS <- "US000"
blsKY <- "21000"
blsBirmingham <- "C1382"
blsCharlotte <- "C1674"
blsCincinnati <- "C1714"
blsColumbus <- "C1814"
blsDayton <- "C1938"
blsGreensboro <- "C2466"
blsIndianapolis <- "C2690"
blsJacksonville <- "C2726"
blsKansasCity <- "C2814"
blsLouisville <- "C3114"
blsMemphis <- "C3282"
blsNashville <- "C3498"
blsOmaha <- "C3654"
blsRaleigh <- "C3958"
blsRichmond <- "C4006"

currentYear <- as.numeric(format(Sys.Date(), "%Y"))
quarter <-  4