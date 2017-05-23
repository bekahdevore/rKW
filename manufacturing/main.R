library(RCurl)
library(dplyr)
library(googlesheets)

demographics <- getURL("https://docs.google.com/spreadsheets/d/1l3GqUbuNcnjUrnoND2J-NAEoAh1G7u6mfN1OF7G2B6s/pub?gid=1144908517&single=true&output=csv")
manufacturing <- getURL("https://docs.google.com/spreadsheets/d/1l3GqUbuNcnjUrnoND2J-NAEoAh1G7u6mfN1OF7G2B6s/pub?gid=1000115821&single=true&output=csv")

demographicsData <- read.csv(textConnection(demographics), check.names = FALSE)
manufacturingData <- read.csv(textConnection(manufacturing), check.names = FALSE)

rm(demographics, manufacturing)

allData <- left_join(manufacturingData, demographicsData, by = "SOC")
allDataTrimmed <- allData %>% filter(allData$`Employed in Industry (2016)` != 0 & allData$`Employed in Industry (2026)` != 0)


write.csv(allDataTrimmed, file = "manufacturingData.csv")
allDataTrimmed_gs <- gs_new("manufacturingData", input = allDataTrimmed) 

