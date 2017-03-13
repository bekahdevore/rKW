library(dplyr)
library(scales)

fourStateRegion <- read.csv("fourStateData.csv")

youth <- fourStateRegion %>% filter(AGEP >= 16 & AGEP <= 19)

youthHousehold <- youth %>% mutate(householdIncome = ifelse(HINCP < 30000, "< 30k", 
                                                      ifelse((HINCP >= 30000 & HINCP < 60000), "30k - 60k", 
                                                             ifelse((HINCP >= 60000 & HINCP < 90000), "60k - 90k",
                                                                    ifelse((HINCP >= 90000 & HINCP <120000), "90k - 120k",
                                                                           ifelse(HINCP >= 120000, "> 120k", "Other")))))) %>% 
                           mutate(employment = ifelse((ESR == 1 | ESR == 2), "Employed", 
                                                      ifelse(ESR == 3, "Unemployed", "Other")))


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
  
  percent(laborForceParticipationRate)
  # percent(unemploymentRate)
  
}


unemploymentAndLaborForce("< 30k")
unemploymentAndLaborForce("30k - 60k")
unemploymentAndLaborForce("60k - 90k")
unemploymentAndLaborForce("90k - 120k")
unemploymentAndLaborForce("> 120k")
