library(rvest) # used to scrape data from html on websites
library(RCurl) # used to get URLs
library(stringr) # used to replace specific values with other values 
library(dplyr) # used to pipe data manipultion

## Variables

## GET ACS DATA VARIABLES
##Info about ACS 1 yr API https://www.census.gov/data/developers/data-sets/acs-1year.html
## variables https://api.census.gov/data/2015/acs1/subject/variables.json
medianHouseholdWage <- "DP03_0062E" # profile
population <- "S0101_C01_001E" # subject
medianHomeValue <- "DP04_0089E" #  profile
medianMonthlyRent <- "DP04_0134E" # profile
laborForceParticipationRate <- "S2301_C02_001E" #subject
POPULATION_2011 

# ACS API Key
apiKey <- "00b1974d78394a0f553ab06d7d20f58d9fee6e51"

# ACS AREAS
acsMetros <- "&for=metropolitan+statistical+area/micropolitan+statistical+area:*"
acsKentucky <- "&for=state:21"
acsUS <- "&for=us:*"

# BLS AREAS 
blsUS <- "US000"
blsKY <- "21000"
blsBirmingham <- "C1382"
blsCharlotte <- "C1674"
blsCincinnati <- "C1714"
blsColumbus <- "C1814"
blsDayton <- "C1938"
blsGreensboro <- "C2466"
blsIndianapolis <- "C2690"
blsJacksonville <- "C2726"
blsKansasCity <- "C2814"
blsLouisville <- "C3114"
blsMemphis <- "C3282"
blsNashville <- "C3498"
blsOmaha <- "C3654"
blsRaleigh <- "C3958"
blsRichmond <- "C4006"


currentYear <- as.numeric(format(Sys.Date(), "%Y"))
quarter <-  4

## FUNCTIONS

# GET CURRENT BLS QCEW DATA
getQcewData <- function(quarter, currentYear, blsAreaCode) {
  dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", quarter, "/area/", blsAreaCode, ".csv")
  dataHere <- read.csv(textConnection(getURL(dataURL)))
  if (ncol(dataHere) > 2) return (dataHere)
  else { 
    for (i in 4:1) {
      dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", i, "/area/", blsAreaCode, ".csv")
      dataHere <- read.csv(textConnection(getURL(dataURL)))
      if (ncol(dataHere) > 2) return (dataHere)
    }
    currentYear = currentYear - 1 
    for (i in 4:1) {
      dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", i, "/area/", blsAreaCode, ".csv")
      dataHere <- read.csv(textConnection(getURL(dataURL)))
      if (ncol(dataHere) > 2) return (dataHere)
    }
  }
}

## BLS IMPORT
####  IMPORT FROM BLS FUNCTION ##
# https://download.bls.gov/pub/time.series/overview.txt

import.from.bls <- function(web.address, filenameInput) {
  
  # Import data
  temp <- tempfile()
  download.file(web.address, temp)
  data <- read.table(temp,
                     header=FALSE,
                     sep="\t",
                     skip=1,
                     stringsAsFactors=FALSE,
                     strip.white=TRUE,
                     quote = NULL)
  
  # Add column headers
  topline <- readLines(web.address)
  topline <- topline[1] # Select column headers
  topline <- as.list(strsplit(x = topline, split = "\t")[[1]]) # Split the string into a list
  colnames(data) <- topline
  
  unlink(temp)
  
  # Drop extra, unused column
  data <- data[,1:(ncol(data) - 1)]
  
  # This command plucks the text that appears after the pattern below,
  # and uses it to name the file.
  #filename <- gsub(pattern = "https://download.bls.gov/pub/time.series/","", x = web.address, ignore.case=T)
  
  # save the file to the global environment
  assign(filenameInput, data, envir = .GlobalEnv) 
}


## ACS DATA
getDataMetros <- function(tableCode, area, currentYear, dataPath) {
  dataHere <- getURL(paste("http://api.census.gov/data/", currentYear, "/acs1/subject?get=NAME,", tableCode, area, "&key=", apiKey))
  dataHere <- read.csv(textConnection(dataHere))
  if (nrow(dataHere) > 2) return (dataHere)
  for (i in currentYear:2015) {
    dataHere <- getURL(paste("http://api.census.gov/data/", i, "/acs1/", dataPath, "?get=NAME,", tableCode, area, "&key=", apiKey))
    dataHere <- read.csv(textConnection(dataHere))
    if (nrow(dataHere) > 2) return (dataHere)
  }
}

cleanAcsData <- function(dataHere, dataPointName){
  dataHere[,"X..NAME"] <- str_replace_all(dataHere[,"X..NAME"], "\\[", "")
  dataHere[,"metropolitan.statistical.area.micropolitan.statistical.area."] <- str_replace_all(dataHere[,"metropolitan.statistical.area.micropolitan.statistical.area."], "\\]", "")
  colnames(dataHere)[3] <- "area_code"
  colnames(dataHere)[2] <- dataPointName
  dataHere[,dataPointName] <- as.numeric(as.character(dataHere[,dataPointName]))
  dataHere[,"area_code"] <- as.numeric(as.character(dataHere[,"area_code"])) 
  dataHere <- dataHere %>% select(2:3)
  dataHere <- dataHere %>% filter(area_code %in% peerAreaCodes$area_code)
  return (dataHere)
}

qcewDataFilter2011 <- function(dataHere) {
  dataHere %>% 
    filter(own_code == 0 & industry_code == 10) %>% 
    select(1, 9, 10, 11, 14, 15)
}

qcewDataFilter <- function(dataHere, enterMSA) {
  dataHere %>% 
    filter(industry_code == 10) %>% 
    filter(own_code == 5 | own_code == 1) %>%
    select(1, 2, 9, 12) %>% 
    mutate(MSA = enterMSA)
}


## Grab  MSA area codes from google sheets
areaCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
peerAreaCodes <- read.csv(textConnection(areaCodeConnection))

## ACS DATA 
medianHomeValue <- cleanAcsData(getDataMetros(medianHomeValue, acsMetros, currentYear, "profile"), "Median Home Value")
medianHouseholdWage <- cleanAcsData(getDataMetros(medianHouseholdWage, acsMetros, currentYear, "profile"), "Median Household Wage")
population <- cleanAcsData(getDataMetros(population, acsMetros, currentYear, "subject"), "Population") 
medianMonthlyRent <-cleanAcsData(getDataMetros(medianMonthlyRent, acsMetros, currentYear, "profile"), "Median Monthly Rent") ### NEED TO FIX
laborForceParticipationRate <- cleanAcsData(getDataMetros(laborForceParticipationRate, acsMetros, currentYear, "subject"), "Labor Force Participation Rate")

## BLS  QCEW DATA 
qcewDataLouisville_2011 <- read.csv("2011_LouisvilleMSA.csv")
qcewDataKentucky_2011 <- read.csv("2011_Kentucky.csv") 

qcewDataKY_2011 <- qcewDataFilter2011(qcewDataKentucky_2011)
qcewDataLouisville_2011 <- qcewDataFilter2011(qcewDataLouisville_2011)

qcewDataUS <- qcewDataFilter(getQcewData(quarter, currentYear, blsUS), "United States")
qcewDataKY <-  qcewDataFilter(getQcewData(quarter, currentYear, blsKY), "Kentucky")
qcewDataBirmingham <- qcewDataFilter(getQcewData(quarter, currentYear, blsBirmingham), "Birmingham")
qcewDataCharlotte <- qcewDataFilter(getQcewData(quarter, currentYear, blsCharlotte), "Charlotte")
qcewDataCincinnati <- qcewDataFilter(getQcewData(quarter, currentYear, blsCincinnati), "Cincinnati")
qcewDataColumbus <- qcewDataFilter(getQcewData(quarter, currentYear, blsColumbus), "Columbus")
qcewDataDayton <- qcewDataFilter(getQcewData(quarter, currentYear, blsDayton), "Dayton")
qcewDataGreensboro <- qcewDataFilter(getQcewData(quarter, currentYear, blsGreensboro), "Greensboro")
qcewDataIndianapolis <- qcewDataFilter(getQcewData(quarter, currentYear, blsIndianapolis), "Indianapolis")
qcewDataJacksonville <- qcewDataFilter(getQcewData(quarter, currentYear, blsJacksonville), "Jacksonville")
qcewDataKansasCity <- qcewDataFilter(getQcewData(quarter, currentYear, blsKansasCity), "Kansas City")
qcewDataLouisville <- qcewDataFilter(getQcewData(quarter, currentYear, blsLouisville), "Louisville")
qcewDataMemphis <- qcewDataFilter(getQcewData(quarter, currentYear, blsMemphis), "Memphis")
qcewDataNashville <- qcewDataFilter(getQcewData(quarter, currentYear, blsNashville), "Nashville")
qcewDataOmaha <- qcewDataFilter(getQcewData(quarter, currentYear, blsOmaha), "Omaha")
qcewDataRaleigh <- qcewDataFilter(getQcewData(quarter, currentYear, blsRaleigh), "Raleigh")
qcewDataRichmond <- qcewDataFilter(getQcewData(quarter, currentYear, blsRichmond), "Richmond")

allCurrentQcewData <- rbind(qcewDataUS, qcewDataKY, qcewDataBirmingham, 
                            qcewDataBirmingham, qcewDataCharlotte, qcewDataCincinnati, 
                            qcewDataColumbus, qcewDataDayton, qcewDataGreensboro, 
                            qcewDataIndianapolis, qcewDataJacksonville, qcewDataKansasCity, 
                            qcewDataLouisville, qcewDataMemphis, qcewDataNashville, 
                            qcewDataOmaha, qcewDataRaleigh, qcewDataRichmond)

## BLS OES imports 
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current", "oesMainData")
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.release", "oesReleaseDate")

## BLS LAUS imports
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.60.Metro", "lausMetros") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/la/la.period", "lausPeriod") # BLS LAUS METROPOLITAN DATA

## Scrape MIT Living Wage Data
mitLivingWageLouisvilleMSA <- read_html("http://livingwage.mit.edu/metros/31140")
mitLivingWageLouisvilleMSA <- mitLivingWageLouisvilleMSA %>% html_nodes("table") %>% 
  .[[1]] %>% 
  html_table()

rm(acsKentucky, acsMetros, acsUS, apiKey, quarter)



##### BLS OES DATA MANIPULATION ####
## Create new variables (from series_id) that contains the area code and datapoint
oesMainData[,"area_code"] <- substr(oesMainData[,"series_id"], 7,11)
oesMainData[,"datapoint"] <- substr(oesMainData[,"series_id"], 24,25)
oesMainData[,"industry"] <- substr(oesMainData[,"series_id"], 15,23)

## Change vairables to numeric
oesMainData[,"area_code"] <- as.numeric(as.character(oesMainData[,"area_code"]))
#oesMainData[,"datapoint"] <- as.numeric(as.character(oesMainData[,"datapoint"]))

oesMainData <- oesMainData %>% 
  filter((datapoint == "13" & (area_code %in% peerAreaCodes$area_code) & industry == "000000000")) %>% 
  select(4:5)
oesMainData[,1] <- as.numeric(as.character(oesMainData[,1]))
colnames(oesMainData)[1] <- "Annual Median Wage (USD)"

## OES RELEASE DATA  
oesReleaseDate <- paste(oesReleaseDate$description)



###### BLS LAUS  MANIPULATION ############
latestYearBlsLaus <- as.numeric(max(lausMetros$year))
lausMetros <- lausMetros %>% filter(year == latestYearBlsLaus)

lausMetros[,"area_code"] <- substr(lausMetros[,"series_id"], 8,12)
lausMetros[,"datapoint"] <- substr(lausMetros[,"series_id"], 19,20)
lausMetros[,"type"] <- substr(lausMetros[,"series_id"], 1,5)
lausMetros[,"month"] <- substr(lausMetros[,"period"], 2,3)

lausMetros[,"area_code"] <- as.numeric(as.character(lausMetros[,"area_code"]))
lausMetros[,"month"] <- as.numeric(as.character(lausMetros[,"month"]))

## LATEST MONTH
latestMonth <- as.numeric(max(lausMetros$month))

lausMetros <- lausMetros %>% filter(((area_code %in% peerAreaCodes$area_code) & (datapoint == "03" | datapoint == "06") & 
                                       month == latestMonth & 
                                       type == "LAUMT"))  
lausMetros <- left_join(lausMetros, lausPeriod, by = "period") %>% select(2, 4:6, 9)
rm(lausPeriod)

unemploymentRate <- lausMetros %>% filter(datapoint == "03") %>% select(2, 3)
colnames(unemploymentRate)[1] <- "Unemployment Rate"

laborForceSize <- lausMetros %>% filter(datapoint == "06") %>% select(2, 3)
colnames(laborForceSize)[1] <- "Labor Force Size"

## Select LAUS Date
lausDate <- lausMetros %>% select(5, 1)
lausDate <- lausDate[1,]
lausData <- paste(lausDate[,1], lausDate[,2])

joinAllDataByThisVariable <- "area_code"
allData <- full_join(laborForceParticipationRate, laborForceSize, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianHomeValue, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianHouseholdWage, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianMonthlyRent, by = joinAllDataByThisVariable)
allData <- full_join(allData, oesMainData, by = joinAllDataByThisVariable)
allData <- full_join(allData, population, by = joinAllDataByThisVariable)
allData <- full_join(allData, unemploymentRate, by = joinAllDataByThisVariable)
allData <- full_join(allData, peerAreaCodes, by = joinAllDataByThisVariable)

allData <- allData %>% select(10, 1, 3:9)


###  RANKINGS, WOULD BE CLEANER IF UPDATED WITH A FUNCTION
lfprRanking <- allData %>%
  mutate("Rank" = rank(-`Labor Force Participation Rate`))
lfprRanking <- lfprRanking %>% select(1, 2, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Labor Force Participation")
colnames(lfprRanking)[2] <- "Value"

lfSizeRanking <- allData %>% 
    mutate("Rank" = rank(-`Labor Force Size`)) %>% 
    select(1, 3, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Labor Force Size")
colnames(lfSizeRanking)[2] <- "Value"

medianHomeValueRanking <- allData %>% 
    mutate("Rank" = rank(-`Median Home Value`)) %>% 
    select(1, 4, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Home Value")
colnames(medianHomeValueRanking)[2] <- "Value"

medianHouseholdWageRanking <- allData %>% 
    mutate("Rank" = rank(-`Median Household Wage`)) %>% 
    select(1, 5, 10)  %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Household Wage")
colnames(medianHouseholdWageRanking)[2] <- "Value"

medianMonthlyRentRank <- allData %>% 
  mutate("Rank" = rank(-`Median Monthly Rent`)) %>% 
  select(1, 6, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Monthly Rent")
colnames(medianMonthlyRentRank)[2] <- "Value"

annualMedianWageRank <- allData %>% 
  mutate("Rank" = rank(-`Annual Median Wage (USD)`)) %>% 
  select(1, 7, 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Annual Median Wage")
colnames(annualMedianWageRank)[2] <- "Value"

populationRanking <- allData %>%
  mutate("Rank" = rank(-Population)) %>%
  select(1, 8, 10)  %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Population")
colnames(populationRanking)[2] <- "Value"

unemploymentRateRanking <- allData %>% 
  mutate("Rank" = rank(`Unemployment Rate`)) %>%
  select(1, 9 , 10) %>% 
  filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Unemployment Rate")
colnames(unemploymentRateRanking)[2] <- "Value"


louisvilleRankings <- rbind(lfprRanking, lfSizeRanking,
              medianHomeValueRanking, medianHouseholdWageRanking,
              medianMonthlyRentRank, annualMedianWageRank, 
              populationRanking, unemploymentRateRanking)

louisvilleRankings <- louisvilleRankings %>% select(4, 3, 2)
louisvilleRankings$Value <- round(louisvilleRankings$Value)
louisvilleRankings <- format(louisvilleRankings, big.mark = ',')


## Add commas to numbers
allData <- format(allData, big.mark = ',')

## Export as .csv document
write.csv(allData, file = "mainDataForOnePager.csv")
write.csv(mitLivingWageLouisvilleMSA, file = "mitLivingWageData.csv")
write.csv(louisvilleRankings, file = "louisvilleRankings.csv")


