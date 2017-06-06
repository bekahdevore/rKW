library(dplyr)
library(RCurl)
library(scales)
#BLS data from here: https://www.bls.gov/lau/#tables

#countyList <- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=468101338&single=true&output=csv")))
dataConnection2016 <- getURL("https://www.bls.gov/lau/laucnty16.txt")
dataConnection2010 <- getURL("https://www.bls.gov/lau/laucnty10.txt")
dataConnection2007 <- getURL("https://www.bls.gov/lau/laucnty07.txt")
dataConnection1999 <- getURL("https://www.bls.gov/lau/laucnty99.txt")

countyCodes <- read.csv("countyCodeList.csv")

removeCommas <- function(data) {
  data <- as.data.frame(lapply(data,function(x) {gsub(",", "", x)
  }))
}

changeDataType <- function(data) {
  names <- c("Labor_Force", "Unemployed")
  data[,names] <- lapply(data[,names] , as.character)
  data[,names] <- lapply(data[,names] , as.numeric)
  data
}

cleanData <- function(year) {
  allData <- read.table(textConnection(year), fill = TRUE)
  allData <- allData[-1:-4, -5]
  
  colnames(allData)[1] <- "LAUS_Code"
  colnames(allData)[2] <- "State_FIPS"
  colnames(allData)[3] <- "County_FIPS"
  colnames(allData)[4] <- "County_Name"
  colnames(allData)[5] <- "State"
  colnames(allData)[6] <- "Year"
  colnames(allData)[7] <- "Labor_Force"
  colnames(allData)[8] <- "Employed"
  colnames(allData)[9] <- "Unemployed"
  colnames(allData)[10] <- "Unemployment_Rate"
  
  allData$Code <- paste0(allData$State_FIPS, allData$County_FIPS)
  allData <- allData %>% filter(Code %in% countyCodes$Code)
  
  allData <- removeCommas(allData)
  allData <- changeDataType(allData)
  
  laborForce <- sum(allData$Labor_Force)
  unemployed <- sum(allData$Unemployed)
  unemploymentRate <- percent(unemployed/laborForce)

}

ur2010 <- as.data.frame(cleanData(dataConnection2010)) %>% mutate(Year = "2010")
ur2007 <- as.data.frame(cleanData(dataConnection2007)) %>% mutate(Year = "2007")
ur1999 <- as.data.frame(cleanData(dataConnection1999)) %>% mutate(Year = "1999")
ur2016 <- as.data.frame(cleanData(dataConnection2016)) %>% mutate(Year = "2016")

colnames(ur2010)[1] <- "Unemployment Rate"
colnames(ur2007)[1] <- "Unemployment Rate"
colnames(ur1999)[1] <- "Unemployment Rate"
colnames(ur2016)[1] <- "Unemployment Rate"

allData <- rbind(ur2010, 
                 ur2007, 
                 ur1999, 
                 ur2016)