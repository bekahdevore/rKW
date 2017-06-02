library(dplyr)
library(googlesheets)

allData <- read.csv("allIndustries40countiesIncluding1990.csv")
countyCodeList <- read.csv("countyCodeList.csv")
colnames(countyCodeList)[3] <- "area_fips"

countiesDataConnection <- getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=468101338&single=true&output=csv")
counties <- read.csv(textConnection(countiesDataConnection))

## FUNCTIONS
changeDataType <- function(data) {
  names <- c("month1_emplvl", "month2_emplvl", "month3_emplvl", "avg_wkly_wage")
  data[,names] <- lapply(data[,names] , as.character)
  data[,names] <- lapply(data[,names] , as.numeric)
  data
}

removeDollarSigns <- function(data) {
  data <- as.data.frame(lapply(data,function(x) {gsub("\\$", "", x)
  }))
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

allData <- allData %>% mutate(industry = ifelse(industry_code == "1013", "Manufacturing", 
                                                ifelse(industry_code == "1012", "Construction", 
                                                       ifelse(industry_code == "62", "Healthcare", 
                                                              ifelse(industry_code == "42" | industry_code == "48-49", "Logistics", 
                                                                     ifelse(industry_code == "1023", "Finance", 
                                                                            ifelse(industry_code == "44-45", "Retail", 
                                                                                   ifelse(industry_code == "722", "Food Service", 
                                                                                          ifelse(industry_code == "721", "Hospitality", 
                                                                                                 ifelse(industry_code == "11", "Agriculture", 
                                                                                                        ifelse(industry_code == "1024", "Business", "Other")))))))))))
allData <- left_join(allData, countyCodeList, by = "area_fips")
allData <- changeDataType(allData)
allData <- allData %>% mutate(annualizedWage = (avg_wkly_wage * 52)) %>% 
  mutate(avgEmployment = (month1_emplvl + month2_emplvl + month3_emplvl)/3) %>% 
  select(Area.Title, industry, annualizedWage, avgEmployment, year)










groupAndCalculate <- function(yearHere, areaGroup, areaName){
  allData <- allData %>% filter(year == yearHere)
  allData <- allData %>% filter(Area.Title %in% areaGroup)
  
  groupEmployment <- data.frame(
    aggregate(allData$avgEmployment, by=list(Category=allData$industry), FUN=sum))
  
  groupWage <- data.frame(
    aggregate(allData$annualizedWage, by=list(Category=allData$industry), FUN=mean))
  
  combinedData <- left_join(groupWage, groupEmployment, by = "Category") 
  combinedData <- combinedData %>% mutate(Area = areaName) %>% 
    mutate(Year = yearHere)
  colnames(combinedData)[1] <- "Industry"
  colnames(combinedData)[2] <- "Avg. Wage"
  colnames(combinedData)[3] <- "Employment"
  
  combinedData
}

all1990 <- groupAndCalculate(1990, counties$allCounties, "All")
kw1990 <- groupAndCalculate(1990, counties$kentuckianaWorks, "KentuckianaWorks")
blue1990 <- groupAndCalculate(1990, counties$bluegrass, "Bluegrass")
nKY1990 <- groupAndCalculate(1990, counties$northernKentucky, "Northern Kentucky")
linc1990 <- groupAndCalculate(1990, counties$lincolnTrail, "LincolnTrail")

all2000 <- groupAndCalculate(2000, counties$allCounties, "All")
kw2000 <- groupAndCalculate(2000, counties$kentuckianaWorks, "KentuckianaWorks")
blue2000 <- groupAndCalculate(2000, counties$bluegrass, "Bluegrass")
nKY2000 <- groupAndCalculate(2000, counties$northernKentucky, "Northern Kentucky")
linc2000 <- groupAndCalculate(2000, counties$lincolnTrail, "LincolnTrail")

all2010 <- groupAndCalculate(2010, counties$allCounties, "All")
kw2010 <- groupAndCalculate(2010, counties$kentuckianaWorks, "KentuckianaWorks")
blue2010 <- groupAndCalculate(2010, counties$bluegrass, "Bluegrass")
nKY2010 <- groupAndCalculate(2010, counties$northernKentucky, "Northern Kentucky")
linc2010 <- groupAndCalculate(2010, counties$lincolnTrail, "LincolnTrail")

all2016 <- groupAndCalculate(2016, counties$allCounties, "All")
kw2016 <- groupAndCalculate(2016, counties$kentuckianaWorks, "KentuckianaWorks")
blue2016 <- groupAndCalculate(2016, counties$bluegrass, "Bluegrass")
nKY2016 <- groupAndCalculate(2016, counties$northernKentucky, "Northern Kentucky")
linc2016 <- groupAndCalculate(2016, counties$lincolnTrail, "LincolnTrail")

everythingIsHere <-  rbind( 
  all1990,
  kw1990,
  blue1990,
  nKY1990,
  linc1990,
  all2000,
  kw2000,
  blue2000,
  nKY2000,
  linc2000,
  all2010,
  kw2010,
  blue2010,
  nKY2010,
  linc2010,
  all2016,
  kw2016,
  blue2016,
  nKY2016,
  linc2016
 )

iris_ss <- gs_new("Area by Industry, including 1990", input = everythingIsHere)
#write.csv(everythingIsHere, file = "industryDataByArea.csv")
