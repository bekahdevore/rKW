#stateData <- full_join(laborForceParticipationRateKY, laborForceSizeKY, by = joinAllDataByThisVariable)
stateData <- laborForceParticipationRateKY
stateData <- full_join(stateData, medianHomeValueKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, medianHouseholdWageKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, medianMonthlyRentKY, by = joinAllDataByThisVariable)
stateData <- full_join(stateData, populationKY, by = joinAllDataByThisVariable)

# US DATA
## MERGE STATE AND US DATA