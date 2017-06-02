library(RCurl)
library(stringr)
library(dplyr)
library(googlesheets)

##Info about ACS 1 yr API https://www.census.gov/data/developers/data-sets/acs-1year.html
## variables https://api.census.gov/data/2015/acs1/cprofile/variables.html
laborForce16plus <- "S2301_C02_001E"
laborForce20to64 <- "S2301_C02_021E"
apiKey <- "00b1974d78394a0f553ab06d7d20f58d9fee6e51"

getDataMetros <- function(tableCode) {
  dataHere <- getURL(paste("http://api.census.gov/data/2015/acs1/subject?get=NAME,", tableCode, "&for=metropolitan+statistical+area/micropolitan+statistical+area:*&key=", apiKey))
}

getDataUS <- function(tableCode){
  dataHere <- getURL(paste("http://api.census.gov/data/2015/acs1/subject?get=NAME,", tableCode,"&for=us:*&key=", apiKey))
}

getDataKY <- function(tableCode){
  dataHere <- getURL(paste("http://api.census.gov/data/2015/acs1/subject?get=NAME,", tableCode, "&for=state:21&key=", apiKey))  
}

getDataJefferson <- function(tableCode){
  dataHere <- getURL(paste("http://api.census.gov/data/2015/acs1/subject?get=NAME,", tableCode, "&for=county:111&in=state:21&key=", apiKey))
}

msaCodesConnection <- getURL("https://docs.google.com/spreadsheets/d/1TpKFAJ43Npz-2SyVxM5_YKJ0dsNHNj6bTMC8wQPfVes/pub?gid=0&single=true&output=csv")
peerCityConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")

msaCodes <- unique(read.csv(textConnection(msaCodesConnection)))
peerCityCodes <- read.csv(textConnection(peerCityConnection))

ky16plus <- read.csv(textConnection(getDataKY(laborForce16plus)))
us16plus <- read.csv(textConnection(getDataUS(laborForce16plus)))
jeff16plus <- read.csv(textConnection(getDataJefferson(laborForce16plus)))
metros16plus <- read.csv(textConnection(getDataMetros(laborForce16plus)))

ky20to64 <- read.csv(textConnection(getDataKY(laborForce20to64)))
us20to64 <- read.csv(textConnection(getDataUS(laborForce20to64)))
jeff20to64 <- read.csv(textConnection(getDataJefferson(laborForce20to64)))
metros20to64 <- read.csv(textConnection(getDataMetros(laborForce20to64)))

dataHere <- metros20to64
dataHere$X..NAME <- str_replace_all(dataHere$X..NAME, "\\[", "")
dataHere$metropolitan.statistical.area.micropolitan.statistical.area. <- str_replace_all(dataHere$metropolitan.statistical.area.micropolitan.statistical.area., "\\]", "")
colnames(dataHere)[3] <- "msaCode"
dataHere$msaCode <- as.numeric(dataHere$msaCode)

colnames(peerCityCodes)[1] <- "msaCode"
allData <- left_join(msaCodes, dataHere, by = "msaCode") %>% select(1:3)
peerCities <- left_join(peerCityCodes, dataHere, by = "msaCode") %>% select(1:2,4)

allData <- allData %>% mutate(ranking = rank(-allData$S2301_C02_021E))
peerCities <- peerCities %>% mutate(ranking = rank(-peerCities$`Labor Force Participation Rate`))
colnames(peerCities)[3] <- "Labor Force Participation Rate"

allData_gs <- gs_new("Labor Force Participation Rate 20 - 64", input = allData)
