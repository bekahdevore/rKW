### ACS DATA 
#METRO DATA  ACS
medianHomeValueMetros <- cleanAcsDataMetros(getDataAcs(medianHomeValue, acsMetros, currentYear, "profile"), "Median Home Value")
medianHouseholdWageMetros <- cleanAcsDataMetros(getDataAcs(medianHouseholdWage, acsMetros, currentYear, "profile"), "Median Household Wage")
populationMetros <- cleanAcsDataMetros(getDataAcs(population, acsMetros, currentYear, "subject"), "Population") 
medianMonthlyRentMetros <-cleanAcsDataMetros(getDataAcs(medianMonthlyRent, acsMetros, currentYear, "profile"), "Median Monthly Rent")
laborForceParticipationRateMetros <- cleanAcsDataMetros(getDataAcs(laborForceParticipationRate, acsMetros, currentYear, "subject"), "Labor Force Participation Rate")

#STATE DATA ACS
medianHomeValueKY <- cleanAcsDataStateUs(getDataAcs(medianHomeValue, acsKentucky, currentYear, "profile"), "Median Home Value")
medianHouseholdWageKY <- cleanAcsDataStateUs(getDataAcs(medianHouseholdWage, acsKentucky, currentYear, "profile"), "Median Household Wage")
populationKY <- cleanAcsDataStateUs(getDataAcs(population, acsKentucky, currentYear, "subject"), "Population") 
medianMonthlyRentKY <-cleanAcsDataStateUs(getDataAcs(medianMonthlyRent, acsKentucky, currentYear, "profile"), "Median Monthly Rent")
laborForceParticipationRateKY <- cleanAcsDataStateUs(getDataAcs(laborForceParticipationRate, acsKentucky, currentYear, "subject"), "Labor Force Participation Rate")

# UNITED STATES DATA ACS
medianHomeValueUS <- cleanAcsDataStateUs(getDataAcs(medianHomeValue, acsUS, currentYear, "profile"), "Median Home Value")
medianHouseholdWageUS <- cleanAcsDataStateUs(getDataAcs(medianHouseholdWage, acsUS, currentYear, "profile"), "Median Household Wage")
populationUS <- cleanAcsDataStateUs(getDataAcs(population, acsUS, currentYear, "subject"), "Population") 
medianMonthlyRentUS <-cleanAcsDataStateUs(getDataAcs(medianMonthlyRent, acsUS, currentYear, "profile"), "Median Monthly Rent")
laborForceParticipationRateUS <- cleanAcsDataStateUs(getDataAcs(laborForceParticipationRate, acsUS, currentYear, "subject"), "Labor Force Participation Rate")