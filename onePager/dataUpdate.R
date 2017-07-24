

library(rvest)
library(RCurl) # used to get URLs
library(stringr) # used to replace specific values with other values 
library(dplyr) # used to pipe data manipultion
library(tidyr)
library(googlesheets)

## Grab  MSA area codes from google sheets
areaCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
peerAreaCodes <- read.csv(textConnection(areaCodeConnection))

## Run scripts to gather and oganize data for one-pager
source("variables.R")
source("functions.R")
source("acsData.R")
source("qcewData.R")
source("oesData.R")
source("lausData.R")
source("mainDataMerge.R")
source("rankings.R")
source("stateUsData.R")


## Add commas to numbers

#allData <- format(allData, big.mark = ',')
#qcewGrowthLouisville <- format(qcewGrowthLouisville, big.mark = ',')
#stateUsData <- format(stateUsData, big.mark = ',')
#stateUsData <- stateUsData %>% select(-1)

## Export datatables as .csv documents

# write.csv(allData, file = "mainDataForOnePager.csv")
# write.csv(mitLivingWageLouisvilleMSA, file = "mitLivingWageData.csv")
# write.csv(louisvilleRankings, file = "louisvilleRankings.csv")
# write.csv(qcewGrowthLouisville, file = "growthLouisville.csv")
# write.csv(stateUsData, file = "stateUsData.csv")



## push to google sheet
peerCitiesGoogleSheet <- gs_title("Peer Cities One-Pager")
allRankingsGoogleSheet <- gs_title("All Rankings One-Pager")
louisvilleRankingsGoogleSheet <- gs_title("Louisville Rankings One-Pager")
growthLouisvilleGoogleSheet <- gs_title("Growth Louisville One-Pager")
stateAndUsDataGoogleSheet <- gs_title("State and US One-Pager")


peerCitiesGoogleSheet <- peerCitiesGoogleSheet %>% gs_edit_cells(input = allData)
allRankingsGoogleSheet <- allRankingsGoogleSheet %>% gs_edit_cells(input = allRankings)
louisvilleRankingsGoogleSheet <- louisvilleRankingsGoogleSheet %>% gs_edit_cells(input = louisvilleRankings)
growthLouisvilleGoogleSheet <- growthLouisvilleGoogleSheet %>% gs_edit_cells(input = qcewGrowthLouisville)
stateAndUsDataGoogleSheet <- stateAndUsDataGoogleSheet %>% gs_edit_cells(input = stateUsData)

