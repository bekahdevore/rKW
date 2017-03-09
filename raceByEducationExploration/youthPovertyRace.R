library(dplyr)

allData <- read.csv("louisvilleMsaData.csv")


youthPovertyRace <- allData %>% filter(AGEP >= 16 & AGEP <= 18) %>%
                                filter(POVPIP <= 100)

sum(youthPovertyRace$PWGTP)
black <- youthPovertyRace %>% filter(RAC1P == 2)
white <- youthPovertyRace %>% filter(RAC1P == 1)

sum(black$PWGTP)
sum(white$PWGTP)


laborForce <- youthPovertyRace %>% filter(ESR == 1 | ESR == 2 | ESR == 3)
employed <- laborForce %>% filter(ESR == 1)
unemployed <- laborForce %>% filter(ESR == 3)

allInLaborForce <- sum(laborForce$PWGTP)
employedPeople <- sum(employed$PWGTP)
unemployedPeople <- sum(unemployed$PWGTP)

unemploymentRate <- unemployedPeople/allInLaborForce
unemploymentRate
