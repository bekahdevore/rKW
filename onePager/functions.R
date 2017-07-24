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
  dataURL <- getURL(paste0("http://api.census.gov/data/", currentYear, "/acs1/", dataPath, "?get=NAME,", tableCode, "&for=", areaHere, "&key=", apiKey))
  dataConnection <- textConnection(dataURL)
  dataHere <- read.csv(dataConnection)
  close(dataConnection)
  if (nrow(dataHere) >= 1) return (dataHere)
  for (i in currentYear:2015) {
    dataURL <- getURL(paste0("http://api.census.gov/data/", i, "/acs1/", dataPath, "?get=NAME,", tableCode, "&for=", areaHere, "&key=", apiKey))
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

