library(dplyr)

PopulationA <- read.csv("ss15pusa.csv")
PopulationB <- read.csv("ss15pusb.csv")
HousingA <- read.csv("ss15husa.csv")
HousingB <- read.csv("ss15husb.csv")


datasetA <- left_join(PopulationA, HousingA, by = "SERIALNO")
datasetB <- left_join(PopulationB, HousingB, by = "SERIALNO")

allData <- rbind(datasetA, datasetB)
rm(PopulationA, PopulationB, HousingA, HousingB, datasetA, datasetB)

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

youthHousehold <- youthHousehold %>% filter(RAC1P == 2)

unemploymentAndLaborForce <- function(householdGroup) {
  
  youthHousehold <- youthHousehold %>% filter(householdIncome == householdGroup)
  unemployed      <- youthHousehold %>% filter(employment == "Unemployed")
  employed        <- youthHousehold %>% filter(employment == "Employed")
  
  all <- sum(youthHousehold$PWGTP)
  
  unemployedNumber <- sum(unemployed$PWGTP)
  employedNumber  <- sum(employed$PWGTP)
  laborForceNumber <- unemployedNumber + employedNumber
  
  laborForceParticipationRate <- laborForceNumber/all
  unemploymentRate <- unemployedNumber/laborForceNumber
  
  #percent(laborForceParticipationRate)
  percent(unemploymentRate)
  
}


unemploymentAndLaborForce("< 30k")
unemploymentAndLaborForce("30k - 60k")
unemploymentAndLaborForce("60k - 90k")
unemploymentAndLaborForce("90k - 120k")
unemploymentAndLaborForce("120k - 150k")
unemploymentAndLaborForce("150k - 180k")
unemploymentAndLaborForce("180k - 210k")
unemploymentAndLaborForce("> 210k")

