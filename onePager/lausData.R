## BLS LAUS imports
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.60.Metro", "lausMetros") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/la/la.period", "lausPeriod") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", "lausUS") # US laus data
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.24.Kentucky", "lausKY") # state laus data

###### BLS LAUS  MANIPULATION ############
## GET LATEST YEAR TO USE IN DATA 
latestYearBlsLaus <- as.numeric(max(lausMetros$year))

lausDataManipulation <- function(lausMetros) {
  lausMetros <- lausMetros %>% filter(year == latestYearBlsLaus)
  lausMetros[,"area_code"] <- substr(lausMetros[,"series_id"], 8,12)
  lausMetros[,"datapoint"] <- substr(lausMetros[,"series_id"], 19,20)
  lausMetros[,"type"] <- substr(lausMetros[,"series_id"], 1,5)
  lausMetros[,"month"] <- substr(lausMetros[,"period"], 2,3)
  
  lausMetros[,"area_code"] <- as.numeric(as.character(lausMetros[,"area_code"]))
  lausMetros[,"month"] <- as.numeric(as.character(lausMetros[,"month"]))
  
  ## LATEST MONTH
  latestMonth <- as.numeric(max(lausMetros$month))
  
  lausMetros <- lausMetros %>% filter((datapoint == "03" | datapoint == "06") & month == latestMonth )
  lausMetros <- left_join(lausMetros, lausPeriod, by = "period") 
}

## KENTUCKY LAUS DATA
lausKY <- lausDataManipulation(lausKY)
## METROS LAUS DATA
lausMetros <- lausDataManipulation(lausMetros)

## GET LATEST MONTH TO USE ON ALL LAUS AND LFS CPS DATA
latestMonth <- as.numeric(max(lausMetros$month))

## Labor Force Statistics from the Current Population Survey (NAICS)
lausUS[,"month"] <- substr(lausUS[,"period"], 2,3)
lausUS[,"month"] <- as.numeric(as.character(lausUS[,"month"]))
lausUS <- lausUS %>% filter(year == latestYearBlsLaus & month == latestMonth) 

## US UNEMPLOYMENT
usUnemployment <- lausUS %>% filter(series_id == "LNS14000000")

## US LABOR FORCE SIZE
usLaborForceSize <- lausUS %>% filter(series_id == "LNS12000000")
usLaborForceSize$value <- as.numeric(as.character(usLaborForceSize$value))
usLaborForceSize$value <- usLaborForceSize$value * 1000
usLaborForceSize <- usLabor


## CLEAN LAUS METROS DATA TO PREPARE FOR MERGE WITH MAIN DATA
lausMetros <- lausMetros %>% filter(((area_code %in% peerAreaCodes$area_code) & type == "LAUMT"))  
lausMetros <- lausMetros %>% select(2, 4:6, 9)

## PEER CITY UNEMPLOYMENT RATE
unemploymentRateMetros <- lausMetros %>% filter(datapoint == "03") %>% select(2, 3)
colnames(unemploymentRateMetros)[1] <- "Unemployment Rate"
## PEER CITY LABOR FORCE SIZE
laborForceSizeMetros <- lausMetros %>% filter(datapoint == "06") %>% select(2, 3)
colnames(laborForceSizeMetros)[1] <- "Labor Force Size"

### CLEAN LAUS DATA FOR KENTUCKY
lausKY <- lausKY %>% filter(area_code == 2100000)

## Select LAUS Date
lausDate <- lausMetros %>% select(5, 1)
lausDate <- lausDate[1,]
lausData <- paste(lausDate[,1], lausDate[,2])
