library(dplyr)
library(RMySQL)

PopulationA <- read.csv("ss15pusa.csv")
PopulationB <- read.csv("ss15pusb.csv")
HousingA <- read.csv("ss15husa.csv")
HousingB <- read.csv("ss15husb.csv")

con <- dbConnect(MySQL(), group = "kwlmi", dbname = "kwlmi")
# 
# dbWriteTable(conn = con, name = 'ss15husb', value = HousingB)


datasetA <- left_join(PopulationA, HousingA, by = "SERIALNO")
datasetB <- left_join(PopulationB, HousingB, by = "SERIALNO")

allData <- rbind(datasetA, datasetB)
rm(PopulationA, PopulationB, HousingA, HousingB, datasetA, datasetB)
dbWriteTable(conn = con, name = 'usaPUMS', value = allData)

youth <- allData %>% filter(AGEP >= 16 & AGEP <= 19)


# serialNoSum <- aggregate(test$PERNP, by=list(Category=test$SERIALNO), FUN=sum)
# colnames(serialNoSum)[1] <- "SERIALNO"
# colnames(serialNoSum)[2] <- "wageSum"

# youthMerge <- left_join(youth, serialNoSum, by = "SERIALNO")
# 
# youthMerge$wage <- youthMerge$HINCP - youthMerge$wageSum

youthHousehold <- youth %>% mutate(householdIncome = ifelse(HINCP < 30000, "< 30k", 
                                                            ifelse((HINCP >= 30000 & HINCP < 60000), "30k - 60k", 
                                                                   ifelse((HINCP >= 60000 & HINCP < 90000), "60k - 90k",
                                                                          ifelse((HINCP >= 90000 & HINCP <120000), "90k - 120k",
                                                                                 ifelse((HINCP >= 120000 & HINCP < 150000), "120k - 150k", 
                                                                                        ifelse((HINCP >= 150000 & HINCP < 180000), "150k - 180k",
                                                                                               ifelse((HINCP >= 180000 & HINCP < 210000), "180k - 210k",
                                                                                                      ifelse(HINCP >= 210000, "> 210k", "Other"))))))))) %>% 
                            mutate(employment = ifelse((ESR == 1 | ESR == 2), "Employed", 
                                                     ifelse(ESR == 3, "Unemployed", "Other")))





unemploymentAndLaborForce <- function(householdGroup, raceCategory, dataToReturn) {
  if(raceCategory == "black") {
    youthHousehold <- youthHousehold %>% filter(RAC1P == 2)
  }
  else if(raceCategory == "white") {
    youthHousehold <- youthHousehold %>% filter(RAC1P == 1)
  }
  else {
    youthHousehold
  }
  
  youthHousehold <- youthHousehold %>% filter(householdIncome == householdGroup)
  unemployed      <- youthHousehold %>% filter(employment == "Unemployed")
  employed        <- youthHousehold %>% filter(employment == "Employed")
  
  all <- sum(youthHousehold$PWGTP)
  
  unemployedNumber <- sum(unemployed$PWGTP)
  employedNumber  <- sum(employed$PWGTP)
  laborForceNumber <- unemployedNumber + employedNumber
  
  laborForceParticipationRate <- laborForceNumber/all
  unemploymentRate <- unemployedNumber/laborForceNumber
  
  if(dataToReturn == "laborForce") {
    dataOutput <- percent(laborForceParticipationRate)
  }
  else if(dataToReturn == "unemployment") {
    dataOutput <- percent(unemploymentRate)  
  }
  dataOutput
}


unemploymentAndLaborForce("< 30k", "black", "laborForce")
unemploymentAndLaborForce("30k - 60k", "black", "laborForce")
unemploymentAndLaborForce("60k - 90k", "black", "laborForce")
unemploymentAndLaborForce("90k - 120k", "black", "laborForce")
unemploymentAndLaborForce("120k - 150k", "black", "laborForce")
unemploymentAndLaborForce("150k - 180k", "black", "laborForce")
unemploymentAndLaborForce("180k - 210k", "black", "laborForce")
unemploymentAndLaborForce("> 210k", "black", "laborForce")

unemploymentAndLaborForce("< 30k", "black", "unemployment")
unemploymentAndLaborForce("30k - 60k", "black", "unemployment")
unemploymentAndLaborForce("60k - 90k", "black", "unemployment")
unemploymentAndLaborForce("90k - 120k", "black", "unemployment")
unemploymentAndLaborForce("120k - 150k", "black", "unemployment")
unemploymentAndLaborForce("150k - 180k", "black", "unemployment")
unemploymentAndLaborForce("180k - 210k", "black", "unemployment")
unemploymentAndLaborForce("> 210k", "black", "unemployment")

unemploymentAndLaborForce("< 30k", "white", "laborForce")
unemploymentAndLaborForce("30k - 60k", "white", "laborForce")
unemploymentAndLaborForce("60k - 90k", "white", "laborForce")
unemploymentAndLaborForce("90k - 120k", "white", "laborForce")
unemploymentAndLaborForce("120k - 150k", "white", "laborForce")
unemploymentAndLaborForce("150k - 180k", "white", "laborForce")
unemploymentAndLaborForce("180k - 210k", "white", "laborForce")
unemploymentAndLaborForce("> 210k", "white", "laborForce")

unemploymentAndLaborForce("< 30k", "white", "unemployment")
unemploymentAndLaborForce("30k - 60k", "white", "unemployment")
unemploymentAndLaborForce("60k - 90k", "white", "unemployment")
unemploymentAndLaborForce("90k - 120k", "white", "unemployment")
unemploymentAndLaborForce("120k - 150k", "white", "unemployment")
unemploymentAndLaborForce("150k - 180k", "white", "unemployment")
unemploymentAndLaborForce("180k - 210k", "white", "unemployment")
unemploymentAndLaborForce("> 210k", "white", "unemployment")


