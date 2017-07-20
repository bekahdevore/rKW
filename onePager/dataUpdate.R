library(rvest) # used to scrape data from html on websites
library(RCurl) # used to get URLs
library(stringr) # used to replace specific values with other values 
library(dplyr) # used to pipe data manipultion
library(tidyr)


## Grab  MSA area codes from google sheets
areaCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
peerAreaCodes <- read.csv(textConnection(areaCodeConnection))

## Scrape MIT Living Wage Data
mitLivingWageLouisvilleMSA <- read_html("http://livingwage.mit.edu/metros/31140")
mitLivingWageLouisvilleMSA <- mitLivingWageLouisvilleMSA %>% html_nodes("table") %>% 
  .[[2]] %>% 
  html_table()
mitLivingWageLouisvilleMSA <- mitLivingWageLouisvilleMSA %>% select(-10)

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
allData <- format(allData, big.mark = ',')
qcewGrowthLouisville <- format(qcewGrowthLouisville, big.mark = ',')
stateUsData <- format(stateUsData, big.mark = ',')
#stateUsData <- stateUsData %>% select(-1)

## Export datatables as .csv documents
write.csv(allData, file = "mainDataForOnePager.csv")
write.csv(mitLivingWageLouisvilleMSA, file = "mitLivingWageData.csv")
write.csv(louisvilleRankings, file = "louisvilleRankings.csv")
write.csv(qcewGrowthLouisville, file = "growthLouisville.csv")
write.csv(stateUsData, file = "stateUsData.csv")
## state and us datatable here

## push to google sheet

