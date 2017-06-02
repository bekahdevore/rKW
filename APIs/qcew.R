getData <- function(industryCode) {
  dataURL <- paste0("https://data.bls.gov/cew/data/api/2016/3/industry/", industryCode, ".csv")
  dataHere <- read.csv(textConnection(getURL(dataURL)))
  # dataHere <- dataHere %>% filter(area_fips %in% countyCodesList$Code) %>% 
  #   select(1, 3, 6, 10, 11, 12, 16)
}

## Before 2012, must manually retrieve data files and add to directory
## Files found here: https://www.bls.gov/cew/datatoc.htm
getDataFile <- function(year, industryCode, industryTitle){
  readThisCsv <- paste0( year, ".q1-q4.by_industry/", year, ".q1-q4"," ", industryCode, " ", industryTitle, ".csv")
  dataHere <- read.csv(readThisCsv)  
  # dataHere <- dataHere %>% 
  #   filter(area_fips %in% countyCodesList$Code) %>% 
  #   filter(qtr == 3) %>% select(1, 3, 6, 15, 16, 17, 21)
}