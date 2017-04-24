library(dplyr)
library(RCurl)

zipCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/14CzxVoRs7D_MeomNLdndAMmp5vSUv69re1OQt_nah-k/pub?gid=0&single=true&output=csv")
crimeTypeConnection <- getURL("https://docs.google.com/spreadsheets/d/1HbVtxLCaRdn_CIMW-OdDWExgxNOWROD0gOQZiLURRp4/pub?gid=0&single=true&output=csv")

zipCodes <- read.csv(textConnection(zipCodeConnection))
crimeType <- read.csv(textConnection(crimeTypeConnection))
crimeData2016 <- read.csv("Crime_Data_2016_29.csv")

violentCrimesJeffesonCounty <- crimeData2016 %>% filter(ZIP_CODE %in% zipCodes$zip) %>% 
                              filter(CRIME_TYPE %in% crimeType$VIOLENT_CRIME)

violentCrimeJeffersonCountyRevised <- crimeData2016 %>% filter(ZIP_CODE %in% zipCodes$zip) %>% 
                                          filter(UOR_DESC %in% crimeType$SPECIFIC_CRIME_TYPE)

violentCrimesLouisville <- crimeData2016 %>% filter(UOR_DESC %in% crimeType$SPECIFIC_CRIME_TYPE)
test <- as.data.frame(unique(sort(violentCrimesJeffesonCounty$UOR_DESC)))
