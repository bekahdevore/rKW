library(RCurl)
library(dplyr)
library(stringr)
#Connect to data url's
qcewAllIndustriesDataConnection <- getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=276897935&single=true&output=csv")
specificIndustriesDataConnection <- getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=1004580518&single=true&output=csv")
countiesDataConnection <- getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=468101338&single=true&output=csv")

#Read data into R from URL's 
qcewAllIndustriesData <- read.csv(textConnection(qcewAllIndustriesDataConnection)) #Make new column with annualized wage, filter to only columns that are needed
specificIndustriesData <- read.csv(textConnection(specificIndustriesDataConnection))
counties <- read.csv(textConnection(countiesDataConnection))



#Remove data that is no longer needed
rm(qcewAllIndustriesDataConnection, manufacturingDataConnection, countiesDataConnection)

# FUNCTIONS
removeCommas <- function(data) {
  data <- as.data.frame(lapply(data,function(x) {gsub(",", "", x)
                        }))
}

removeDollarSigns <- function(data) {
  data <- as.data.frame(lapply(data,function(x) {gsub("\\$", "", x)
                        }))
}




changeDataType <- function(data) {
  names <- c("July.Employment", "August.Employment", "September.Employment", "Average.Weekly.Wage")
  data[,names] <- lapply(data[,names] , as.character)
  data[,names] <- lapply(data[,names] , as.numeric)
  data
}


annalizeAndSelect <- function(data) {
  this <- data %>% mutate(annualizedWage = (Average.Weekly.Wage * 52)) %>% 
    mutate(averageEmployment = ((July.Employment + August.Employment + September.Employment)/3))
   output <- this %>% select(County, annualizedWage, averageEmployment)
   output
}

fortyCountyFilter <- function(data) {
  data %>% filter(County %in% counties$allCounties)
}
kentuckianaWorksFilter <- function(data) {
  data %>% filter(County %in% counties$kentuckianaWorks)
}
bluegrassFilter <- function(data) {
  data %>% filter(County %in% counties$bluegrass)
}
northernKentuckyFilter <- function(data) {
  data %>% filter(County %in% counties$northernKentucky)
}
lincolnTrailFilter <- function(data) {
  data %>% filter(County %in% counties$lincolnTrail)
}

manipulateData <- function(insertData, insertFilter) {annalizeAndSelect(changeDataType(removeDollarSigns(removeCommas(insertFilter(insertData)))))}


# Run Data
fortyCountiesData <- manipulateData(qcewAllIndustriesData, fortyCountyFilter)
kentuckianaWorksData <- manipulateData(qcewAllIndustriesData, kentuckianaWorksFilter)
bluegrassData <- manipulateData(qcewAllIndustriesData, bluegrassFilter)
lincolnTrailData <- manipulateData(qcewAllIndustriesData, lincolnTrailFilter)



