library(RCurl)
library(dplyr)
library(plotly)
library(ggplot2)
library(scales)


## Load and read data
load("allData.RData") ## BLS data for all metros

msaCodesConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
msaListConnection <- getURL("https://docs.google.com/spreadsheets/d/1TpKFAJ43Npz-2SyVxM5_YKJ0dsNHNj6bTMC8wQPfVes/pub?gid=0&single=true&output=csv")
rppConnection <- getURL("https://docs.google.com/spreadsheets/d/101D20GoMgT50II1U_fzS0Cc4uwrjxDvdoovbbi74gu0/pub?gid=0&single=true&output=csv")
statesRppConnection <- getURL("https://docs.google.com/spreadsheets/d/101D20GoMgT50II1U_fzS0Cc4uwrjxDvdoovbbi74gu0/pub?gid=1515091341&single=true&output=csv")

msaCodes <- read.csv(textConnection(msaCodesConnection))
msaList <- read.csv(textConnection(msaListConnection))
rpp <- read.csv(textConnection(rppConnection))
statesRpp <- read.csv(textConnection(statesRppConnection))

allOccupationsMedianAverageWage <- allData %>% filter(datatype_code == 4 | datatype_code == 13) %>% 
  filter(industry_code == "000000" & occupation_code == 0)

allOccupationsMedianAverageWage <- left_join(allOccupationsMedianAverageWage, rpp, by = "area_code")
allOccupationsMedianAverageWage <- left_join(allOccupationsMedianAverageWage, statesRpp, by = "area_code")

allOccupationsMedianAverageWage$value <- as.numeric(allOccupationsMedianAverageWage$value)
allOccupationsMedianAverageWage$X2014 <- as.numeric(as.character(allOccupationsMedianAverageWage$X2014))

allOccupationsMedianAverageWage <- allOccupationsMedianAverageWage %>% mutate(stateCol = value/(All/100))
allOccupationsMedianAverageWage <- allOccupationsMedianAverageWage %>% mutate(col = value/(X2014/100)) %>% 
  filter(LineCode == 1)

national <- allOccupationsMedianAverageWage %>% filter(areatype_code.x == "N")
kentucky <- allOccupationsMedianAverageWage %>% filter(area_code == 2100000) 
louisville <- allOccupationsMedianAverageWage %>% filter(area_code == 31140)

allMSAs <- allOccupationsMedianAverageWage %>% filter(area_code %in% msaList$msaCode)
msaMean <- allMSAs %>% filter(datatype_code == 4) %>% select(21, 4, 31) %>% mutate(rank = rank(-col))
msaMedian <- allMSAs %>% filter(datatype_code == 13) %>% select(21, 4, 31) %>% mutate(rank = rank(-col))


write.csv(msaMean, file = "msaMean.csv")
write.csv(msaMedian, file = "msaMedian.csv")
