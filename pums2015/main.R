library(RCurl)
library(dplyr)
library(plotrix)
library(scales)
library(plotly)
library(ggplot2)

## Read pums data
indianaPopulation  <- read.csv("ss15pin.csv")
kentuckyPopulation <- read.csv("ss15pky.csv")

# add PUMA list for filtering
dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv")
pumas          <- read.csv(textConnection(dataConnection))
rm(dataConnection)


## FUNCTIONS
pumaFilter <- function(enterData, enterPUMASList) {
  dataSave <- enterData %>% filter(PUMA %in% pumas[, enterPUMASList])
}

## Filter data
indianaPopulation  <- pumaFilter(indianaPopulation, "inPUMA")
kentuckyPopulation <- pumaFilter(kentuckyPopulation, "kyPUMA")

## Merge data
allData <- rbind(indianaPopulation, kentuckyPopulation)
rm(indianaPopulation, kentuckyPopulation, pumas, pumaFilter)

## Look at weighted totals for specific populations
totalPopulation <- sum(allData$PWGTP)
withDisability <- sum(subset(allData, DIS == 1, select=PWGTP))

percentWithDisability <- percent(withDisability/totalPopulation)

