library(dplyr)
library(RCurl)
library(streamgraph)
library(tidyr)

countyCodeList <- read.csv("countyCodeList.csv")
colnames(countyCodeList)[3] <- "area_fips"

countiesDataConnection <- getURL("https://docs.google.com/spreadsheets/d/1D4d6pTndk6CffoEnGr6DZn95Q7PVbQyCRHwEVLp2iGg/pub?gid=468101338&single=true&output=csv")
counties <- read.csv(textConnection(countiesDataConnection))


# list of all datasets 
# https://download.bls.gov/pub/time.series/overview.txt

import.from.bls <- function(web.address) {
  
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
  filename <- gsub(pattern = "https://download.bls.gov/pub/time.series/la/la.","", x = web.address, ignore.case=T)
  
  # save the file to the global environment
  assign(filename, data, envir = .GlobalEnv) 
}


import.from.bls("https://download.bls.gov/pub/time.series/la/la.area") # area
import.from.bls("https://download.bls.gov/pub/time.series/la/la.series") 
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.24.Kentucky")



bluegrassCounties <- countyCodeList %>% filter(Area.Title %in% counties$bluegrass)
bluegrassCounties$Area.Title <- sub("Kentucky", "KY", bluegrassCounties$Area.Title)

countyData <- area %>% filter(area_text %in% bluegrassCounties$Area.Title)
countyData <- merge(countyData, series, by = "area_code") 
countyData <- merge(countyData2, data.24.Kentucky, by = "series_id")
countyData <- countyData %>% filter(period == "M13")

countyData <- countyData %>% filter(measure_code != 3)

countyData2 <- group_by(countyData, year, measure_code)
countyDataGrouped <- summarise(countyData2, values = sum(value))

countyDataGroupedSpread <- spread(countyDataGrouped, measure_code, values)
colnames(countyDataGroupedSpread)[2] <- "Unemployed"
colnames(countyDataGroupedSpread)[3] <- "Employed"
colnames(countyDataGroupedSpread)[4] <- "Labor_Force"
countyDataGroupedSpread$UR <- countyDataGroupedSpread$Unemployed/countyDataGroupedSpread$Labor_Force



countyDataForStreamgraph <- countyDataGrouped %>% mutate(data_type = ifelse(measure_code == 4, "Unemployed", 
                                                                            ifelse(measure_code == 5, "Employed",
                                                                                   ifelse(measure_code == 6, "Labor Force", "Other"))))
countyDataForStreamgraph <- countyDataForStreamgraph %>% filter(data_type != "Labor Force")
## visuzliation
streamgraph(countyDataForStreamgraph, "data_type","values","year" ) %>% sg_fill_brewer(palette = "Dark2") %>% 
  sg_axis_x(5, "year")
