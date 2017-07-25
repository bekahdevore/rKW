library(rvest)
library(RCurl) # used to get URLs
library(stringr) # used to replace specific values with other values 
library(dplyr) # used to pipe data manipultion
library(tidyr)
library(googlesheets)

## Grab  MSA area codes from google sheets
areaCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
peerAreaCodes <- read.csv(textConnection(areaCodeConnection))

############# VARIABLES

# ACS API Key
apiKey <- "00b1974d78394a0f553ab06d7d20f58d9fee6e51"

##  ACS DATA VARIABLES
##Info about ACS 1 yr API https://www.census.gov/data/developers/data-sets/acs-1year.html
##  subject variables: https://api.census.gov/data/2015/acs1/subject/variables.json 
## profile variables:   https://api.census.gov/data/2015/acs1/profile/variables.json 
medianHouseholdWage <- "DP03_0062E" # profile
population <- "S0101_C01_001E" # subject
medianHomeValue <- "DP04_0089E" #  profile
medianMonthlyRent <- "DP04_0134E" # profile
laborForceParticipationRate <- "S2301_C02_001E" #subject

# ACS AREAS
acsMetros <- "metropolitan+statistical+area/micropolitan+statistical+area:*"
acsKentucky <- "state:21"
acsUS <- "us:*"

## LAUS COLUMN NAMES
unemploymentRateName <- "Unemployment Rate"
laborForceSizeName <- "Labor Force Size"

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

################################## FUNCTIONS

# GET CURRENT BLS QCEW DATA
getQcewData <- function(quarter, currentYear, blsAreaCode) {
  dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", quarter, "/area/", blsAreaCode, ".csv")
  dataConnection <- textConnection(getURL(dataURL))
  dataHere <- read.csv(dataConnection)
  close(dataConnection)
  if (ncol(dataHere) > 2) return (dataHere)
  else { 
    for (i in 4:1) {
      dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", i, "/area/", blsAreaCode, ".csv")
      dataConnection <- textConnection(getURL(dataURL))
      dataHere <- read.csv(dataConnection)
      close(dataConnection)
      if (ncol(dataHere) > 2) return (dataHere)
    }
    currentYear = currentYear - 1 
    for (i in 4:1) {
      dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", i, "/area/", blsAreaCode, ".csv")
      dataConnection <- textConnection(getURL(dataURL))
      dataHere <- read.csv(dataConnection)
      close(dataConnection)
      if (ncol(dataHere) > 2) return (dataHere)
    }
  }
}

qcewDataFilter2011 <- function(dataHere) {
  dataHere %>% 
    filter((own_code == 0 | own_code == 5) & industry_code == 10) %>% 
    select(1, 2, 9, 10, 11, 14, 15)
}

qcewDataFilter <- function(dataHere, enterMSA) {
  dataHere %>% 
    filter(industry_code == 10) %>% 
    filter(own_code == 5 | own_code == 0) %>%
    select(1, 2, 9, 12) %>% 
    mutate(MSA = enterMSA)
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

## LAUS DATA CLEAN/MANIPULATE
lausDataManipulation <- function(lausData, dataArea) {
  lausData <- lausData
  lausData[,"area_code"] <- substr(lausData[,"series_id"], 8,12)
  lausData[,"datapoint"] <- substr(lausData[,"series_id"], 19,20)
  lausData[,"type"] <- substr(lausData[,"series_id"], 1,5)
  lausData[,"month"] <- substr(lausData[,"period"], 2,3)
  
  lausData[,"area_code"] <- as.numeric(as.character(lausData[,"area_code"]))
  lausData[,"month"] <- as.numeric(as.character(lausData[,"month"]))
  lausData <- lausData %>% filter(month != 13)
  
  if (dataArea == "KY") {
    lausData <- lausData %>% filter(area_code == 0 & type == "LASST") %>% select(-5)
    lausData$area_code = 21
    lausData
  }
  if (dataArea == "Metros") {
    ## FILTER PEER CITIES WITHIN LAUS METROS AND SELECT NECESSARY VARIABLES
    lausData <- lausData %>% filter(((area_code %in% peerAreaCodes$area_code) & type == "LAUMT"))  
    #lausData <- lausData %>% select(2, 4:6, 9)
    lausData
  }
  
  ## LATEST MONTH
  latestMonth <- as.numeric(max(lausData$month))
  #latestYearBlsLaus <- as.numeric(max(lausData$year))
  
  lausData <- lausData %>% filter((datapoint == "03" | datapoint == "06") & month == latestMonth )
  lausData <- left_join(lausData, lausPeriod, by = "period") 
  #lausData <- lausData %>% filter(year == latestYearBlsLaus)
}

## ACS DATA
## Example URL
#http://api.census.gov/data/2015/acs1/profile?get=NAME,DP04_0089E&for=state:021&key=00b1974d78394a0f553ab06d7d20f58d9fee6e51
getDataAcs <- function(tableCode, areaHere, currentYear, dataPath) {
  dataURL <- getURL(paste0("https://api.census.gov/data/", currentYear, "/acs1/", dataPath, "?get=NAME,", tableCode, "&for=", areaHere, "&key=", apiKey))
  dataConnection <- textConnection(dataURL)
  dataHere <- read.csv(dataConnection)
  close(dataConnection)
  if (nrow(dataHere) >= 1) return (dataHere)
  for (i in currentYear:2015) {
    dataURL <- getURL(paste0("https://api.census.gov/data/", i, "/acs1/", dataPath, "?get=NAME,", tableCode, "&for=", areaHere, "&key=", apiKey))
    dataConnection <- textConnection(dataURL)
    dataHere <- read.csv(dataConnection)
    close(dataConnection)
    if (nrow(dataHere) >= 1) return (dataHere)
  }
}
#test <- getDataAcs(medianHomeValue, acsMetros, currentYear, "profile")


cleanAcsDataMetros <- function(dataHere, dataPointName){
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

cleanAcsDataStateUs <- function(dataHere, dataPointName) {
  dataHere[,3] <- str_replace_all(dataHere[,3], "\\]", "")
  colnames(dataHere)[3] <- "area_code"
  colnames(dataHere)[2] <- dataPointName
  dataHere[,dataPointName] <- as.numeric(as.character(dataHere[,dataPointName]))
  dataHere[,"area_code"] <- as.numeric(as.character(dataHere[,"area_code"])) 
  dataHere <- dataHere %>% select(3:2)
  return (dataHere)
}

### ACS DATA 
#METRO DATA  ACS
medianHomeValueMetros <- cleanAcsDataMetros(getDataAcs(medianHomeValue, acsMetros, currentYear, "profile"), "Median Home Value")
medianHouseholdWageMetros <- cleanAcsDataMetros(getDataAcs(medianHouseholdWage, acsMetros, currentYear, "profile"), "Median Household Wage")
populationMetros <- cleanAcsDataMetros(getDataAcs(population, acsMetros, currentYear, "subject"), "Population") 
medianMonthlyRentMetros <-cleanAcsDataMetros(getDataAcs(medianMonthlyRent, acsMetros, currentYear, "profile"), "Median Monthly Rent")
laborForceParticipationRateMetros <- cleanAcsDataMetros(getDataAcs(laborForceParticipationRate, acsMetros, currentYear, "subject"), "Labor Force Participation Rate")

#STATE DATA ACS
medianHomeValueKY <- cleanAcsDataStateUs(getDataAcs(medianHomeValue, acsKentucky, currentYear, "profile"), "Median Home Value")
medianHouseholdWageKY <- cleanAcsDataStateUs(getDataAcs(medianHouseholdWage, acsKentucky, currentYear, "profile"), "Median Household Wage")
populationKY <- cleanAcsDataStateUs(getDataAcs(population, acsKentucky, currentYear, "subject"), "Population") 
medianMonthlyRentKY <-cleanAcsDataStateUs(getDataAcs(medianMonthlyRent, acsKentucky, currentYear, "profile"), "Median Monthly Rent")
laborForceParticipationRateKY <- cleanAcsDataStateUs(getDataAcs(laborForceParticipationRate, acsKentucky, currentYear, "subject"), "Labor Force Participation Rate")

# UNITED STATES DATA ACS
medianHomeValueUS <- cleanAcsDataStateUs(getDataAcs(medianHomeValue, acsUS, currentYear, "profile"), "Median Home Value")
medianHouseholdWageUS <- cleanAcsDataStateUs(getDataAcs(medianHouseholdWage, acsUS, currentYear, "profile"), "Median Household Wage")
populationUS <- cleanAcsDataStateUs(getDataAcs(population, acsUS, currentYear, "subject"), "Population") 
medianMonthlyRentUS <-cleanAcsDataStateUs(getDataAcs(medianMonthlyRent, acsUS, currentYear, "profile"), "Median Monthly Rent")
laborForceParticipationRateUS <- cleanAcsDataStateUs(getDataAcs(laborForceParticipationRate, acsUS, currentYear, "subject"), "Labor Force Participation Rate")

### BLS  QCEW DATA 
qcewDataKentucky_2011 <- read.csv("2011_Kentucky.csv") 
qcewDataLouisville_2011 <- read.csv("2011_LouisvilleMSA.csv")

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
                            qcewDataCharlotte, qcewDataCincinnati, 
                            qcewDataColumbus, qcewDataDayton, qcewDataGreensboro, 
                            qcewDataIndianapolis, qcewDataJacksonville, qcewDataKansasCity, 
                            qcewDataLouisville, qcewDataMemphis, qcewDataNashville, 
                            qcewDataOmaha, qcewDataRaleigh, qcewDataRichmond)

allCurrentQcewData <- allCurrentQcewData %>% select(-1)

privateQcewData <- allCurrentQcewData %>% filter(own_code == 5) %>% select(2:4)
allQcewData <- allCurrentQcewData %>% filter(own_code == 0) %>% select(2:4)

establishmentsPrivate <- "Establishments (private)"
establishmentsAll <- "Establishments (all)"

employmentPrivate <- "Employment (private)"
employmentAll <- "Employment (all)"

colnames(privateQcewData)[1] <- establishmentsPrivate
colnames(allQcewData)[1] <- establishmentsAll

colnames(privateQcewData)[2] <- employmentPrivate
colnames(allQcewData)[2] <- employmentAll

allCurrentQcewData <- left_join(privateQcewData, allQcewData, by = "MSA") %>% select(3, 1, 4, 2, 5)

## GET GROWTH SINCE 2011 IN LOUISVILLE

# Prepare data for merge
qcewDataLouisville <- qcewDataLouisville %>% select(-1)
colnames(qcewDataLouisville)[2] <- "Current Establishments"
colnames(qcewDataLouisville)[3] <- "Current Employment"

qcewGrowthLouisville <- qcewDataLouisville_2011 %>% select(2, 6:7) 
colnames(qcewGrowthLouisville)[2] <- "2011 Establishments" 
colnames(qcewGrowthLouisville)[3] <- "2011 Employment"

qcewGrowthLouisville <- full_join(qcewGrowthLouisville, qcewDataLouisville, by = "own_code")

qcewGrowthLouisville$`Establishment Growth since 2011` <- qcewGrowthLouisville$`Current Establishments`- qcewGrowthLouisville$`2011 Establishments`
qcewGrowthLouisville$`Employment Growth since 2011` <- qcewGrowthLouisville$`Current Employment` - qcewGrowthLouisville$`2011 Employment`

qcewGrowthLouisville <- qcewGrowthLouisville %>% mutate("Ownership type" = ifelse(own_code == 0, "All", "Private"))
qcewGrowthLouisville <- qcewGrowthLouisville %>% select(9, 4:5, 7:8)

## BLS OES imports 
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current", "oesMainData")
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.release", "oesReleaseDate")
####### NEED TO PULL STATE AND NATION

##### BLS OES DATA MANIPULATION ####
## Create new variables (from series_id) that contains the area code and datapoint
oesMainData[,"area_code"] <- substr(oesMainData[,"series_id"], 7,11)
oesMainData[,"industry"] <- substr(oesMainData[,"series_id"], 15,23)
oesMainData[,"datapoint"] <- substr(oesMainData[,"series_id"], 24,25)
oesMainData[,"middle"] <- substr(oesMainData[,"series_id"], 5,23)

## Change vairables to numeric
oesMainData[,"area_code"] <- as.numeric(as.character(oesMainData[,"area_code"]))
#oesMainData[,"datapoint"] <- as.numeric(as.character(oesMainData[,"datapoint"]))
oesNationData <- oesMainData %>% 
  filter((datapoint == 13 & (area_code == 00) & middle == "0000000000000000000"))

oesStateData <- oesMainData %>% 
  filter((datapoint == 13 & middle == "2100000000000000000"))

oesMetrosData <- oesMainData %>% 
  filter((datapoint == "13" & (area_code %in% peerAreaCodes$area_code) & industry == "000000000")) %>% 
  select(4:5)
oesMetrosData[,1] <- as.numeric(as.character(oesMetrosData[,1]))
colnames(oesMetrosData)[1] <- "Annual Median Wage (USD)"

## OES RELEASE DATA  
oesReleaseDate <- paste(oesReleaseDate$description)

## CLEAN OES NATION AND STATE DATA
oesNationData <- oesNationData %>% select(value) 
oesNationData$area_code <- 1
colnames(oesNationData)[1] <- "Annual Median Wage (USD)"

oesStateData <- oesStateData %>% select(value)
oesStateData$area_code <- 21
colnames(oesStateData)[1] <- "Annual Median Wage (USD)"

## BLS LAUS imports
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.60.Metro", "lausMetros") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/la/la.period", "lausPeriod") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", "lausUS") # US laus data
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.24.Kentucky", "lausKY") # state laus data


## CLEAN LAUS DATA
#lausMetros <- lausMetros %>% filter(((area_code %in% peerAreaCodes$area_code)))
lausKY <- lausDataManipulation(lausKY, "KY")
lausMetros <- lausDataManipulation(lausMetros, "Metros")

## GET LATEST MONTH TO USE ON ALL LAUS AND LFS CPS DATA
latestMonth <- as.numeric(max(lausMetros$month))
latestYearBlsLaus <- as.numeric(max(lausMetros$year))

lausKY <- lausKY %>% filter(year == latestYearBlsLaus)
lausMetros <- lausMetros %>% filter(year == latestYearBlsLaus)

## Labor Force Statistics from the Current Population Survey (NAICS)
lausUS[,"month"] <- substr(lausUS[,"period"], 2,3)
lausUS[,"month"] <- as.numeric(as.character(lausUS[,"month"]))
lausUS <- lausUS %>% filter(month != 13)
lausUS <- lausUS %>% filter(year == latestYearBlsLaus & month == latestMonth)
lausUS$area_code <- 1


### US 
## US UNEMPLOYMENT
unemploymentRateUS <- lausUS %>% filter(series_id == "LNS14000000")
unemploymentRateUS <- unemploymentRateUS %>% select(4, 6)
colnames(unemploymentRateUS)[1] <- unemploymentRateName


## US LABOR FORCE SIZE
laborForceSizeUS <- lausUS %>% filter(series_id == "LNS11000000")
laborForceSizeUS$value <- as.numeric(as.character(laborForceSizeUS$value))
laborForceSizeUS$value <- laborForceSizeUS$value * 1000
laborForceSizeUS <- laborForceSizeUS %>% select(4, 6)
colnames(laborForceSizeUS)[1] <- laborForceSizeName


### PEER CITIES
# PEER CITY UNEMPLOYMENT RATE
unemploymentRateMetros <- lausMetros %>% filter(datapoint == "03") %>% select(4, 5)
colnames(unemploymentRateMetros)[1] <- unemploymentRateName
# PEER CITY LABOR FORCE SIZE
laborForceSizeMetros <- lausMetros %>% filter(datapoint == "06") %>% select(4, 5)
colnames(laborForceSizeMetros)[1] <- laborForceSizeName


### KENTUCKY 
### CLEAN LAUS DATA FOR KENTUCKY
unemploymentRateKY <- lausKY %>% filter(datapoint == "03") %>% select(4, 8)
colnames(unemploymentRateKY)[1] <- unemploymentRateName

laborForceSizeKY <- lausKY %>% filter(datapoint == "06") %>% select(4, 8)
colnames(laborForceSizeKY)[1] <- laborForceSizeName

## Select LAUS Date
lausDate <- lausMetros %>% select(5, 1)
lausDate <- lausDate[1,]
lausDate <- paste(lausDate[,1], lausDate[,2])

#### JOIN DATA TO MAKE MAIN DATA TABLE ########

# populationMetros$Population <- as.numeric(populationMetros$Population) 
# laborForceSizeMetros$`Labor Force Size` <- as.numeric(laborForceSizeMetros$`Labor Force Size`)
oesMetrosData$`Annual Median Wage (USD)` <- as.numeric(oesMetrosData$`Annual Median Wage (USD)`)

joinAllDataByThisVariable <- "area_code"
allData <- full_join(laborForceParticipationRateMetros, laborForceSizeMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianHomeValueMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianHouseholdWageMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianMonthlyRentMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, populationMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, unemploymentRateMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, oesMetrosData, by = joinAllDataByThisVariable)
allData <- full_join(allData, peerAreaCodes, by = joinAllDataByThisVariable)
allData <- left_join(allData, allCurrentQcewData, by = "MSA")
allData <- allData %>% select(10, 1, 3:14)

###  RANKINGS, WOULD BE CLEANER IF UPDATED WITH A FUNCTION
lfprRanking <- allData %>%
  mutate("Rank" = rank(-`Labor Force Participation Rate`))
lfprRanking <- lfprRanking %>% select(1, 2, 14) %>% 
  #filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Labor Force Participation Rate")
colnames(lfprRanking)[2] <- "Value"
#lfprRanking$Datapoint <- paste0("$", lfprRanking$Datapoint)

lfSizeRanking <- allData %>% 
  mutate("Rank" = rank(-`Labor Force Size`)) %>% 
  select(1, 3, 14) %>% 
  #filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Labor Force Size")
colnames(lfSizeRanking)[2] <- "Value"

medianHomeValueRanking <- allData %>% 
  mutate("Rank" = rank(-`Median Home Value`)) %>% 
  select(1, 4, 14) %>% 
  #filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Home Value")
colnames(medianHomeValueRanking)[2] <- "Value"

medianHouseholdWageRanking <- allData %>% 
  mutate("Rank" = rank(-`Median Household Wage`)) %>% 
  select(1, 5, 14)  %>% 
  #filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Household Wage")
colnames(medianHouseholdWageRanking)[2] <- "Value"

medianMonthlyRentRank <- allData %>% 
  mutate("Rank" = rank(-`Median Monthly Rent`)) %>% 
  select(1, 6, 14) %>% 
  #filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Median Monthly Rent")
colnames(medianMonthlyRentRank)[2] <- "Value"

annualMedianWageRank <- allData %>% 
  mutate("Rank" = rank(-`Annual Median Wage (USD)`)) %>% 
  select(1, 9, 14) %>% 
  #filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Annual Median Wage (USD)")
colnames(annualMedianWageRank)[2] <- "Value"

populationRanking <- allData %>%
  mutate("Rank" = rank(-Population)) %>%
  select(1, 7, 14)  %>% 
  #filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Population")
colnames(populationRanking)[2] <- "Value"

unemploymentRateRanking <- allData %>% 
  mutate("Rank" = rank(`Unemployment Rate`)) %>%
  select(1, 8 , 14) %>% 
  #filter(MSA == "Louisville") %>%
  mutate(Datapoint = "Unemployment Rate")
colnames(unemploymentRateRanking)[2] <- "Value"
unemploymentRateRanking$Rank <- round(unemploymentRateRanking$Rank)

allRankings <- rbind(lfprRanking, lfSizeRanking,
                     medianHomeValueRanking, medianHouseholdWageRanking,
                     medianMonthlyRentRank, annualMedianWageRank, 
                     populationRanking, unemploymentRateRanking)

louisvilleRankings <- allRankings %>% filter(MSA == "Louisville")
louisvilleRankings <- louisvilleRankings %>% select(4, 3, 2)
louisvilleRankings <- format(louisvilleRankings, big.mark = ',')

allRankings <- allRankings %>% select(-Value)
allRankings <- spread(allRankings, Datapoint, Rank)

#stateData <- full_join(laborForceParticipationRateKY, laborForceSizeKY, by = joinAllDataByThisVariable)
stateData <- full_join(laborForceParticipationRateKY, laborForceSizeKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, medianHomeValueKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, medianHouseholdWageKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, medianMonthlyRentKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, populationKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, unemploymentRateKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, oesStateData, by = joinAllDataByThisVariable)

# US DATA
usData <- full_join(laborForceParticipationRateUS , laborForceSizeUS, by = joinAllDataByThisVariable)
usData <- full_join(usData, medianHomeValueUS, by = joinAllDataByThisVariable)
usData <- full_join(usData, medianHouseholdWageUS, by = joinAllDataByThisVariable)
usData <- full_join(usData, medianMonthlyRentUS, by = joinAllDataByThisVariable)
usData <- full_join(usData, populationUS, by = joinAllDataByThisVariable)
usData <- full_join(usData, unemploymentRateUS, by = joinAllDataByThisVariable)
usData <- full_join(usData, oesNationData, by = joinAllDataByThisVariable)


## MERGE STATE AND US DATA
stateUsData <- rbind(usData, stateData)
stateUsData <- stateUsData %>% mutate(Area = ifelse(area_code == 1, "United States", "Kentucky"))
stateUsData <- stateUsData %>% select(10, 2:9)

# Add Louisville data
louisvilleAllData <- allData %>% filter(MSA == "Louisville") %>% select(2:9)
louisvilleAllData$Area <- "Louisville"

stateUsData <- rbind(stateUsData, louisvilleAllData)

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