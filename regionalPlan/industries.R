library(RCurl)
library(dplyr)

# Industry Codes
manufacturing <- 1013
construction <- 1012
healthcare <- 62
logistics1 <- 42
logistics2File <- "48-49"
logistics2api <- "48_49"
finance <- 1023
retailFile <- "44-45"
retailApi <- "44_45"
foodService <- 722
hospitality <- 721
agriculture <- 11
business <- 1024

manufacturingTitle <- "Manufacturing"
constructionTitle <- "Construction"
healthcareTitle <- "Health care and social assistance"
logistics1Title <- "Wholesale trade"
logistics2Title <- "Transportation and warehousing"
financeTitle <- "Financial activities"
retailTitle <- "Retail Trade"
foodServiceTitle <- "Food services and drinking places"
hospitalityTitle <- "Accommodation"
agricultureTitle <- "Agriculture, forestry, fishing and hunting"
businessTitle <- "Professional and business services"


areaCodes <- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/1WVJh6RgaVLTtpF8GDF9ZuuXBVJ58opmzfrmFpqmUegA/pub?gid=0&single=true&output=csv")))
countyList <- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=468101338&single=true&output=csv")))

colnames(countyList)[1] <- "Area.Title"
countyCodesList <- left_join(countyList, areaCodes, by = "Area.Title") %>% select(1, 6)

getData <- function(industryCode) {
  dataURL <- paste0("https://data.bls.gov/cew/data/api/2016/3/industry/", industryCode, ".csv")
  dataHere <- read.csv(textConnection(getURL(dataURL)))
  dataHere <- dataHere %>% filter(area_fips %in% countyCodesList$Code) %>% 
    select(1, 3, 6, 10, 11, 12, 16)
}

getDataFile <- function(year, industryCode, industryTitle){
  readThisCsv <- paste0( year, ".q1-q4.by_industry/", year, ".q1-q4"," ", industryCode, " ", industryTitle, ".csv")
  dataHere <- read.csv(readThisCsv)  
  dataHere <- dataHere %>% 
    filter(area_fips %in% countyCodesList$Code) %>% 
    filter(qtr == 3) %>% select(1, 3, 6, 15, 16, 17, 21)
}

## Archived Data from files
manufacturing1990 <- getDataFile(1990, manufacturing, manufacturingTitle)
construction1990 <- getDataFile(1990, construction, constructionTitle)
healthcare1990 <- getDataFile(1990, healthcare, healthcareTitle)
logistics1_1990 <- getDataFile(1990, logistics1, logistics1Title)
logistics2_1990 <- getDataFile(1990, logistics2File, logistics2Title)
finance1990 <- getDataFile(1990, finance, financeTitle)
retail1990 <- getDataFile(1990, retailFile, retailTitle)
foodService1990 <- getDataFile(1990, foodService, foodServiceTitle)
hospitality1990 <- getDataFile(1990, hospitality, hospitalityTitle)
agriculture1990 <- getDataFile(1990, agriculture, agricultureTitle)
business1990 <- getDataFile(1990, business, businessTitle)

#all2000 <- getDataFile(2000, 10, "Total, all industries") %>% filter(own_code == 0)
manufacturing2000 <- getDataFile(2000, manufacturing, manufacturingTitle)
construction2000 <- getDataFile(2000, construction, constructionTitle)
healthcare2000 <- getDataFile(2000, healthcare, healthcareTitle)
logistics1_2000 <- getDataFile(2000, logistics1, logistics1Title)
logistics2_2000 <- getDataFile(2000, logistics2File, logistics2Title)
finance2000 <- getDataFile(2000, finance, financeTitle)
retail2000 <- getDataFile(2000, retailFile, retailTitle)
foodService2000 <- getDataFile(2000, foodService, foodServiceTitle)
hospitality2000 <- getDataFile(2000, hospitality, hospitalityTitle)
agriculture2000 <- getDataFile(2000, agriculture, agricultureTitle)
business2000 <- getDataFile(2000, business, businessTitle)

manufacturing2010 <- getDataFile(2010, manufacturing, manufacturingTitle)
construction2010 <- getDataFile(2010, construction, constructionTitle)
healthcare2010 <- getDataFile(2010, healthcare, healthcareTitle)
logistics1_2010 <- getDataFile(2010, logistics1, logistics1Title)
logistics2_2010 <- getDataFile(2010, logistics2File, logistics2Title)
finance2010 <- getDataFile(2010, finance, financeTitle)
retail2010 <- getDataFile(2010, retailFile, retailTitle)
foodService2010 <- getDataFile(2010, foodService, foodServiceTitle)
hospitality2010 <- getDataFile(2010, hospitality, hospitalityTitle)
agriculture2010 <- getDataFile(2010, agriculture, agricultureTitle)
business2010 <- getDataFile(2010, business, businessTitle)


### Data from API 2016 Q3
manufacturing2016 <- getData(manufacturing)
construction2016 <- getData(construction)
healthcare2016 <- getData(healthcare)
logistics1_2016 <- getData(logistics1)
logistics2_2016 <- getData(logistics2api)
finance2016 <- getData(finance)
retail2016 <- getData(retailApi)
foodService2016 <- getData(foodService)
hospitality2016 <- getData(hospitality)
agriculture2016 <- getData(agriculture)
business2016 <- getData(business)

allData <- rbind(manufacturing1990,
                 manufacturing2000,
                 manufacturing2010, 
                 manufacturing2016,
                 construction1990,
                 construction2000,
                 construction2010, 
                 construction2016,
                 healthcare1990,
                 healthcare2000,
                 healthcare2010, 
                 healthcare2016,
                 logistics1_1990,
                 logistics1_2000,
                 logistics1_2010, 
                 logistics1_2016,
                 logistics2_1990,
                 logistics2_2000,
                 logistics2_2010, 
                 logistics2_2016,
                 finance1990,
                 finance2000,
                 finance2010, 
                 finance2016,
                 retail1990,
                 retail2000,
                 retail2010, 
                 retail2016,
                 foodService1990,
                 foodService2000,
                 foodService2010, 
                 foodService2016,
                 hospitality1990,
                 hospitality2000,
                 hospitality2010, 
                 hospitality2016,
                 agriculture1990,
                 agriculture2000,
                 agriculture2010, 
                 agriculture2016,
                 business1990,
                 business2000,
                 business2010, 
                 business2016)

rm(manufacturing2000,
   manufacturing2010, 
   manufacturing2016,
   construction2000,
   construction2010, 
   construction2016,
   healthcare2000,
   healthcare2010, 
   healthcare2016,
   logistics1_2000,
   logistics1_2010, 
   logistics1_2016,
   logistics2_2000,
   logistics2_2010, 
   logistics2_2016,
   finance2000,
   finance2010, 
   finance2016,
   retail2000,
   retail2010, 
   retail2016,
   foodService2000,
   foodService2010, 
   foodService2016,
   hospitality2000,
   hospitality2010, 
   hospitality2016,
   agriculture2000,
   agriculture2010, 
   agriculture2016,
   business2000,
   business2010, 
   business2016)

write.csv(allData, file = "allIndustries40countiesIncluding1990.csv")
