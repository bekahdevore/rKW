library(dplyr)

PopulationA <- read.csv("ss15pusa.csv")
PopulationB <- read.csv("ss15pusb.csv")
HousingA <- read.csv("ss15husa.csv")
HousingB <- read.csv("ss15husb.csv")


datasetA <- left_join(PopulationA, HousingA, by = "SERIALNO")
datasetB <- left_join(PopulationB, HousingB, by = "SERIALNO")

allData <- rbind(datasetA, datasetB)
rm(PopulationA, PopulationB, HousingA, HousingB, datasetA, datasetB)

youth16to19 <- allData %>% filter((AGEP >= 16 & AGEP <= 19 & ESR == 6 & SCH == 1))
youth16to21 <- allData %>% filter((AGEP >= 16 & AGEP <= 21 & ESR == 6 & SCH == 1)) 

write.csv(youth16to19, file = "youth16to19.csv")
write.csv(youth16to21, file = "youth16to21.csv") 
