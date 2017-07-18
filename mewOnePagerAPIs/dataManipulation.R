##### BLS OES DATA MANIPULATION ####
## Create new variables (from series_id) that contains the area code and datapoint
oesMainData[,"area_code"] <- substr(oesMainData[,"series_id"], 7,11)
oesMainData[,"datapoint"] <- substr(oesMainData[,"series_id"], 24,25)
oesMainData[,"industry"] <- substr(oesMainData[,"series_id"], 15,23)

## Change vairables to numeric
oesMainData[,"area_code"] <- as.numeric(as.character(oesMainData[,"area_code"]))
#oesMainData[,"datapoint"] <- as.numeric(as.character(oesMainData[,"datapoint"]))

oesMainData <- oesMainData %>% 
  filter((datapoint == "13" & (area_code %in% peerAreaCodes$area_code) & industry == "000000000")) %>% 
  select(4:5)
oesMainData[,1] <- as.numeric(as.character(oesMainData[,1]))
colnames(oesMainData)[1] <- "Annual Median Wage (USD)"


oesReleaseDate <- paste(oesReleaseDate$description)

###### BLS LAUS  MANIPULTION ############
latestYearBlsLaus <- as.numeric(max(lausMetros$year))
lausMetros <- lausMetros %>% filter(year == latestYearBlsLaus)

lausMetros[,"area_code"] <- substr(lausMetros[,"series_id"], 8,12)
lausMetros[,"datapoint"] <- substr(lausMetros[,"series_id"], 19,20)
lausMetros[,"type"] <- substr(lausMetros[,"series_id"], 1,5)
lausMetros[,"month"] <- substr(lausMetros[,"period"], 2,3)

lausMetros[,"area_code"] <- as.numeric(as.character(lausMetros[,"area_code"]))
lausMetros[,"month"] <- as.numeric(as.character(lausMetros[,"month"]))


latestMonth <- as.numeric(max(lausMetros$month))

lausMetros <- lausMetros %>% filter(((area_code %in% peerAreaCodes$area_code) & (datapoint == "03" | datapoint == "06") & 
                                       month == latestMonth & 
                                       type == "LAUMT"))  
lausMetros <- left_join(lausMetros, lausPeriod, by = "period") %>% select(2, 4:6, 9)
rm(lausPeriod)

unemploymentRate <- lausMetros %>% filter(datapoint == "03") %>% select(2, 3)
colnames(unemploymentRate)[1] <- "Unemployment Rate"

laborForceSize <- lausMetros %>% filter(datapoint == "06") %>% select(2, 3)
colnames(laborForceSize)[1] <- "Labor Force Size"

## Select LAUS Date
lausDate <- lausMetros %>% select(5, 1)
lausDate <- lausDate[1,]
lausData <- paste(lausDate[,1], lausDate[,2])

joinAllDataByThisVariable <- "area_code"
allData <- full_join(laborForceParticipationRate, laborForceSize, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianHomeValue, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianHouseholdWage, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianMonthlyRent, by = joinAllDataByThisVariable)
allData <- full_join(allData, oesMainData, by = joinAllDataByThisVariable)
allData <- full_join(allData, population, by = joinAllDataByThisVariable)
allData <- full_join(allData, unemploymentRate, by = joinAllDataByThisVariable)
allData <- full_join(allData, peerAreaCodes, by = joinAllDataByThisVariable)

allData <- allData %>% select(10, 1, 3:9)
allData <- format(allData, big.mark = ',')
