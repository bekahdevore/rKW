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
  .[[1]] %>% 
  html_table()


source("variables.R")
source("functions.R")
source("acsData.R")
source("qcewData.R")
source("oesData.R")
source("lausData.R")

#rm(acsKentucky, acsMetros, acsUS, apiKey, quarter)

source("mainDataMerge.R")
source("rankings.R")
source("stateUsData.R")

## Add commas to numbers
allData <- format(allData, big.mark = ',')

## Export datatables as .csv documents
write.csv(allData, file = "mainDataForOnePager.csv")
write.csv(mitLivingWageLouisvilleMSA, file = "mitLivingWageData.csv")
write.csv(louisvilleRankings, file = "louisvilleRankings.csv")
## state and us datatable here

