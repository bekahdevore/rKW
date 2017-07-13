
## ACS DATA 
medianHomeValue <- cleanAcsData(getDataMetros(medianHomeValue, acsMetros, currentYear, "profile"), "Median Home Value")
medianHouseholdWage <- cleanAcsData(getDataMetros(medianHouseholdWage, acsMetros, currentYear, "profile"), "Median Household Wage")
population <- cleanAcsData(getDataMetros(population, acsMetros, currentYear, "subject"), "Population")
medianMonthlyRent <-cleanAcsData(getDataMetros(medianMonthlyRent, acsMetros, currentYear, "profile"), "Median Monthly Rent")
laborForceParticipationRate <- cleanAcsData(getDataMetros(laborForceParticipationRate, acsMetros, currentYear, "subject"), "Labor Force Participation Rate")

## BLS  QCEW DATA 
qcewDataLouisville <- getQcewData(quarter, currentYear)
## BLS OES imports 
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current", "oesMainData")
## BLS LAUS imports
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.60.Metro", "lausMetros") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/la/la.period", "lausPeriod") # BLS LAUS METROPOLITAN DATA


## Grab  MSA area codes from google sheets
areaCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
peerAreaCodes <- read.csv(textConnection(areaCodeConnection))

## Scrape MIT Living Wage Data
mitLivingWageLouisvilleMSA <- read_html("http://livingwage.mit.edu/metros/31140")
mitLivingWageLouisvilleMSA <- mitLivingWageLouisvilleMSA %>% html_nodes("table") %>% 
  .[[1]] %>% 
  html_table()