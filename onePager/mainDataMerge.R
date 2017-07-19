#### JOIN DATA TO MAKE MAIN DATA TABLE ########
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