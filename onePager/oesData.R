## BLS OES imports 
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current", "oesMainData")
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.release", "oesReleaseDate")
####### NEED TO PULL STATE AND NATION

##### BLS OES DATA MANIPULATION ####
## Create new variables (from series_id) that contains the area code and datapoint
oesMainData[,"area_code"] <- substr(oesMainData[,"series_id"], 7,11)
oesMainData[,"industry"] <- substr(oesMainData[,"series_id"], 15,23)
oesMainData[,"datapoint"] <- substr(oesMainData[,"series_id"], 24,25)
oesMainData[,"middle"] <- substr(oesMainData[,"series_id"], 5,23)

## Change vairables to numeric
oesMainData[,"area_code"] <- as.numeric(as.character(oesMainData[,"area_code"]))
#oesMainData[,"datapoint"] <- as.numeric(as.character(oesMainData[,"datapoint"]))
oesNationData <- oesMainData %>% 
  filter((datapoint == 13 & (area_code == 00) & middle == "0000000000000000000"))

oesStateData <- oesMainData %>% 
  filter((datapoint == 13 & middle == "2100000000000000000"))

oesMetrosData <- oesMainData %>% 
  filter((datapoint == "13" & (area_code %in% peerAreaCodes$area_code) & industry == "000000000")) %>% 
  select(4:5)
oesMetrosData[,1] <- as.numeric(as.character(oesMetrosData[,1]))
colnames(oesMetrosData)[1] <- "Annual Median Wage (USD)"

## OES RELEASE DATA  
oesReleaseDate <- paste(oesReleaseDate$description)

## CLEAN OES NATION AND STATE DATA
oesNationData <- oesNationData %>% select(value) 
oesNationData$area_code <- 1
colnames(oesNationData)[1] <- "Annual Median Wage (USD)"

oesStateData <- oesStateData %>% select(value)
oesStateData$area_code <- 21
colnames(oesStateData)[1] <- "Annual Median Wage (USD)"
