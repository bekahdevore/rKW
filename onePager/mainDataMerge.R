#### JOIN DATA TO MAKE MAIN DATA TABLE ########

# populationMetros$Population <- as.numeric(populationMetros$Population) 
# laborForceSizeMetros$`Labor Force Size` <- as.numeric(laborForceSizeMetros$`Labor Force Size`)
oesMetrosData$`Annual Median Wage (USD)` <- as.numeric(oesMetrosData$`Annual Median Wage (USD)`)

joinAllDataByThisVariable <- "area_code"
allData <- full_join(laborForceParticipationRateMetros, laborForceSizeMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianHomeValueMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianHouseholdWageMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, medianMonthlyRentMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, populationMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, unemploymentRateMetros, by = joinAllDataByThisVariable)
allData <- full_join(allData, oesMetrosData, by = joinAllDataByThisVariable)
allData <- full_join(allData, peerAreaCodes, by = joinAllDataByThisVariable)
allData <- left_join(allData, allCurrentQcewData, by = "MSA")
allData <- allData %>% select(10, 1, 3:14)
