library(RCurl)
library(dplyr)


## BLS MEDIAN WAGE 
areaCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
peerAreaCodes <- read.csv(textConnection(areaCodeConnection))
 
load("allData.RData")

medianWage <- allData %>% 
  filter((area_code %in% peerAreaCodes$area_code & datatype_code == 13 & occupation_code == 0)) %>% 
  select(20, 4, 13)

medianWage$rank <- rank(desc(medianWage$value))

## BLS LAUS



