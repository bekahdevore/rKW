## BLS LAUS imports
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.60.Metro", "lausMetros") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/la/la.period", "lausPeriod") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", "lausUS") # US laus data
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.24.Kentucky", "lausKY") # state laus data
 

## CLEAN LAUS DATA
#lausMetros <- lausMetros %>% filter(((area_code %in% peerAreaCodes$area_code)))
lausKY <- lausDataManipulation(lausKY, "KY")
lausMetros <- lausDataManipulation(lausMetros, "Metros")

## GET LATEST MONTH TO USE ON ALL LAUS AND LFS CPS DATA
latestMonth <- as.numeric(max(lausMetros$month))
latestYearBlsLaus <- as.numeric(max(lausMetros$year))

lausKY <- lausKY %>% filter(year == latestYearBlsLaus)
lausMetros <- lausMetros %>% filter(year == latestYearBlsLaus)

## Labor Force Statistics from the Current Population Survey (NAICS)
lausUS[,"month"] <- substr(lausUS[,"period"], 2,3)
lausUS[,"month"] <- as.numeric(as.character(lausUS[,"month"]))
lausUS <- lausUS %>% filter(month != 13)
lausUS <- lausUS %>% filter(year == latestYearBlsLaus & month == latestMonth)
lausUS$area_code <- 1


### US 
## US UNEMPLOYMENT
unemploymentRateUS <- lausUS %>% filter(series_id == "LNS14000000")
unemploymentRateUS <- unemploymentRateUS %>% select(4, 6)
colnames(unemploymentRateUS)[1] <- unemploymentRateName


## US LABOR FORCE SIZE
laborForceSizeUS <- lausUS %>% filter(series_id == "LNS11000000")
laborForceSizeUS$value <- as.numeric(as.character(laborForceSizeUS$value))
laborForceSizeUS$value <- laborForceSizeUS$value * 1000
laborForceSizeUS <- laborForceSizeUS %>% select(4, 6)
colnames(laborForceSizeUS)[1] <- laborForceSizeName


### PEER CITIES
# PEER CITY UNEMPLOYMENT RATE
unemploymentRateMetros <- lausMetros %>% filter(datapoint == "03") %>% select(4, 5)
colnames(unemploymentRateMetros)[1] <- unemploymentRateName
# PEER CITY LABOR FORCE SIZE
laborForceSizeMetros <- lausMetros %>% filter(datapoint == "06") %>% select(4, 5)
colnames(laborForceSizeMetros)[1] <- laborForceSizeName


### KENTUCKY 
### CLEAN LAUS DATA FOR KENTUCKY
unemploymentRateKY <- lausKY %>% filter(datapoint == "03") %>% select(4, 8)
colnames(unemploymentRateKY)[1] <- unemploymentRateName

laborForceSizeKY <- lausKY %>% filter(datapoint == "06") %>% select(4, 8)
colnames(laborForceSizeKY)[1] <- laborForceSizeName

## Select LAUS Date
lausDate <- lausMetros %>% select(5, 1)
lausDate <- lausDate[1,]
lausDate <- paste(lausDate[,1], lausDate[,2])
