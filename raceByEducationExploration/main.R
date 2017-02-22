library(RCurl)
library(dplyr)

#Add pums household and population data
indianaHousing  <- read.csv("ss15hin.csv")
kentuckyHousing <- read.csv("ss15hky.csv")

indianaPopulation  <- read.csv("ss15pin.csv")
kentuckyPopulation <- read.csv("ss15pky.csv")

# add PUMA list for filtering
dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv")
pumas          <- read.csv(textConnection(dataConnection))
rm(dataConnection)


# filter for MSA
pumaFilter <- function(enterData, enterPUMASList) {
  dataSave <- enterData %>% filter(PUMA %in% pumas[, enterPUMASList])
}

indianaHousing    <- pumaFilter(indianaHousing, "inPUMA")
kentuckyHousing   <- pumaFilter(kentuckyHousing, "kyPUMA")

indianaPopulation  <- pumaFilter(indianaPopulation, "inPUMA")
kentuckyPopulation <- pumaFilter(kentuckyPopulation, "kyPUMA")

# merge population and housing records
indiana  <- left_join(indianaPopulation, indianaHousing, by = "SERIALNO")
kentucky <- left_join(kentuckyPopulation, kentuckyHousing, by = "SERIALNO")

# merge ky and in puma files 
allData <- rbind(indiana, kentucky)
rm(indiana, indianaHousing, indianaPopulation, kentucky, kentuckyHousing, kentuckyPopulation, pumas, pumaFilter)

#seperate into groups by race and education 
bachelors <- allData %>% filter(SCHL == 21)
masters   <- allData %>% filter(SCHL == 22)

blackBachelors <- bachelors %>% filter(RAC1P == 2)
whiteBachelors <- bachelors %>% filter(RAC1P == 1)

blackMasters   <- masters %>% filter(RAC1P == 2)
whiteMasters   <- masters %>% filter(RAC1P == 1)

genderBreakdownBA <- blackBachelors %>% count(SEX, wt = PWGTP)
genderBreakdownMA <- blackMasters %>% count(SEX, wt = PWGTP)
#calculate percent
#pie graph

ageBA <- blackBachelors %>% count(AGEP, wt = PWGTP)
ageMA <- blackMasters   %>% count(AGEP, wt = PWGTP)

disabilityBA <- blackBachelors %>% count(DIS, wt = PWGTP)
disabilityMA <- blackMasters   %>% count(DIS, wt = PWGTP)
disabilityBA$percent <- (disabilityBA$n)/sum(disabilityBA$n)
disabilityMA$percent <- (disabilityMA$n)/sum(disabilityMA$n)
 