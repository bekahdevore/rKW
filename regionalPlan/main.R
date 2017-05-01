library(RCurl)
library(dplyr)

#Connect to data url's
qcewAllIndustriesDataConnection <- getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=276897935&single=true&output=csv")
manufacturingDataConnection <- getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=411223081&single=true&output=csv")
countiesDataConnection <- getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=468101338&single=true&output=csv")

#Read data into R from URL's 
qcewAllIndustriesData <- read.csv(textConnection(qcewAllIndustriesDataConnection)) #Make new column with annualized wage, filter to only columns that are needed
manufacturingData <- read.csv(textConnection(manufacturingDataConnection))
counties <- read.csv(textConnection(countiesDataConnection))

#Remove data that is no longer needed
rm(qcewAllIndustriesDataConnection, manufacturingDataConnection, countiesDataConnection)

# FUNCTIONS
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



 <- fortyCountyFilter(qcewAllIndustriesData)
