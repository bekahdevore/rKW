library(RCurl)
library(dplyr)
library(tidyr)

dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1EA3LE1IrInIDYnBeXesV3-WYlmnw0V4ORsSTxT16o0s/pub?gid=1267790205&single=true&output=csv")

allData <- read.csv(textConnection(dataConnection)) %>% select(1, 2, 9)

allData <- allData %>% spread(Setting, X2014) %>% select(1, 3, 4)


allData[is.na(allData)] <- 0
allData$All <- (allData$ED + allData$IP)




