# What's the total number for our MSA?  For the state of KY?

  # What percentage are:
      # By race:
          # Non-Hispanic white
          # Black
          # Hispanic
      # By educational attainment
          # Less than HS
          # HS
          # Assoc.
          # BA
          # Grad. degree
          # Professional/doctoral
      # By gender
      # By disability status
      #By median household income  

library(RCurl)
library(dplyr)
library(plotrix)
library(scales)
library(plotly)
library(ggplot2)
library(tidyr)

## ADD DATA
indianaHousing  <- read.csv("ss15hin.csv")
kentuckyHousing <- read.csv("ss15hky.csv")

indianaPopulation  <- read.csv("ss15pin.csv")
kentuckyPopulation <- read.csv("ss15pky.csv")

# add PUMA list for filtering
dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv")
pumas          <- read.csv(textConnection(dataConnection))
rm(dataConnection)


## FUNCTIONS
pumaFilter <- function(enterData, enterPUMASList) {
  dataSave <- enterData %>% filter(PUMA %in% pumas[, enterPUMASList])
}

## FILTER DATA
indianaHousing     <- pumaFilter(indianaHousing, "inPUMA")
kentuckyHousing    <- pumaFilter(kentuckyHousing, "kyPUMA")

indianaPopulation  <- pumaFilter(indianaPopulation, "inPUMA")
kentuckyPopulation <- pumaFilter(kentuckyPopulation, "kyPUMA")

# merge population and housing records
indiana  <- left_join(indianaPopulation, indianaHousing, by = "SERIALNO")
kentucky <- left_join(kentuckyPopulation, kentuckyHousing, by = "SERIALNO")

# merge ky and in puma files 
allData <- rbind(indiana, kentucky)
rm(indiana, indianaHousing, indianaPopulation, kentucky, kentuckyHousing, kentuckyPopulation, pumas, pumaFilter)

blackUnemployed <- allData %>% filter(ESR == 3 & RAC1P == 2)
sixteenPlus <- allData %>% filter(AGEP >= 16)
notInLaborForce <- allData %>% filter(ESR == 6)
unemployed <- allData %>% filter(ESR == 3)


cleanData <- function(enterDataHere) {
  dataOutput <- enterDataHere %>% 
                            mutate(race =
                                        ifelse(RAC1P == 1 & HISP == 01, "Non-Hispanic white",
                                        ifelse(RAC1P == 2, "Black",
                                        ifelse(HISP > 1, "Hispanic", "Other")))) %>%
                            mutate(education = 
                                        ifelse(SCHL < 16, "Less than high school", 
                                        ifelse(SCHL == 16 | SCHL == 17 , "High school diploma or equivalent", 
                                        ifelse(SCHL == 18 | SCHL == 19, "Some college, no degree", 
                                        ifelse(SCHL == 20, "Associate's degree", 
                                        ifelse(SCHL == 21, "Bachelor's degree", 
                                        ifelse(SCHL == 22 | SCHL == 24, "Graduate degree", 
                                        ifelse(SCHL == 23, "Professional degree beyond bachelor's degree", "Other")))))))) %>% 
                            mutate(householdIncome = 
                                        ifelse(HINCP < 30000, "< $30k", 
                                        ifelse((HINCP >= 30000 & HINCP < 60000), "$30k - $60k", 
                                        ifelse((HINCP >= 60000 & HINCP < 90000), "$60k - $90k",
                                        ifelse((HINCP >= 90000 & HINCP <120000), "$90k - $120k",
                                        ifelse((HINCP >= 120000 & HINCP < 150000), "$120k - $150k", 
                                        ifelse((HINCP >= 150000 & HINCP < 180000), "$150k - $180k",
                                        ifelse((HINCP >= 180000 & HINCP < 210000), "$180k - $210k",
                                        ifelse(HINCP >= 210000, "> 210k", "Other")))))))))
}

unemployedClean <- cleanData(unemployed)
notInLaborForceClean <- cleanData(notInLaborForce)
sixteenPlusClean <- cleanData(sixteenPlus)

## Disability data
disability <- notInLaborForce %>% 
                          mutate(visual = ifelse(DEYE == 1, 1, 0)) %>% 
                          mutate(hearing = ifelse(DEAR == 1, 1, 0)) %>% 
                          mutate(ambulatory = ifelse(DPHY == 1, 1, 0)) %>% 
                          mutate(cognitive = ifelse(DREM == 1, 1, 0)) %>% 
                          mutate(selfCare = ifelse(DDRS == 1, 1, 0)) %>% 
                          mutate(independentLiving = ifelse(DOUT == 1, 1, 0)) %>% 
                          select(visual, hearing, ambulatory, cognitive, selfCare, independentLiving, PWGTP, DIS) 

disabilityData <- disability %>% mutate(disability =
                        ifelse((visual == 1 & hearing == 0 & ambulatory == 0 & cognitive == 0 & selfCare == 0 & independentLiving == 0), "Visual", 
                        ifelse((visual == 0 & hearing == 1 & ambulatory == 0 & cognitive == 0 & selfCare == 0 & independentLiving == 0), "Hearing", 
                        ifelse((visual == 0 & hearing == 0 & ambulatory == 1 & cognitive == 0 & selfCare == 0 & independentLiving == 0), "Ambulatory", 
                        ifelse((visual == 0 & hearing == 0 & ambulatory == 0 & cognitive == 1 & selfCare == 0 & independentLiving == 0), "Cognitive", 
                        ifelse((visual == 0 & hearing == 0 & ambulatory == 0 & cognitive == 0 & selfCare == 1 & independentLiving == 0), "Self-Care", 
                        ifelse((visual == 0 & hearing == 0 & ambulatory == 0 & cognitive == 0 & selfCare == 0 & independentLiving == 1), "Independent Living", 
                        ifelse((DIS == 1 & sum(visual, hearing, ambulatory, cognitive, selfCare, independentLiving) > 1), "Multiple disabilities", 
                        ifelse(DIS == 2, "Without disability", "Other")))))))))


## By race
unemployedRace <- count(unemployedClean, race, wt = PWGTP)
  unemployedRace$percent <-  (unemployedRace$n/sum(unemployedRace$n))
  #unemployedRace$label <- paste(unemployedRace$race, "\n", unemployedRace$percent)
  unemployedRace$State <- "Unemployed"

notInLaborForceRace <- dplyr::count(notInLaborForceClean, race, wt = PWGTP)
  notInLaborForceRace$percent <-  notInLaborForceRace$n/sum(notInLaborForceRace$n)
  #notInLaborForceRace$label <- paste(notInLaborForceRace$race, "\n", notInLaborForceRace$percent)
  notInLaborForceRace$State <- "Not in Labor Force"
  
sixteenPlusRace <- dplyr::count(sixteenPlusClean, race, wt = PWGTP)
  sixteenPlusRace$percent <-  (sixteenPlusRace$n/sum(sixteenPlusRace$n))
  #sixteenPlusRace$label <- paste(sixteenPlusRace$race, "\n", sixteenPlusRace$percent)
  sixteenPlusRace$State <- "Population"

sixteenPlusRaceSpread <- sixteenPlusRace %>% select(State, race, percent) %>% spread(race, percent)
notInLaborForceRaceSpread <- notInLaborForceRace %>% select(State, race, percent) %>% spread(race, percent)
unemployedRaceSpread <- unemployedRace %>% select(State, race, percent) %>% spread(race, percent)

raceData <- rbind(sixteenPlusRaceSpread, unemployedRaceSpread, notInLaborForceRaceSpread)
write.csv(raceData, file = "raceData.csv")

# treemap(race, "label", "n", title = "")


## householdIncome level
education <- count(notInLaborForce, education, wt = PWGTP)
education$percent <- percent(education$n/sum(education$n))
education$label <- paste(education$education, "\n", education$percent)

#treemap(education, "label", "n", title = "")


## Sex
sex <- count(notInLaborForce, SEX, wt = PWGTP)
sex$sex <- c("Male", "Female")
sex$percent <- percent(sex$n/sum(sex$n))
sex$label <- paste(sex$sex, "\n", sex$percent)

treemap(sex, "label", "n", title = "")


## Disability
disabilityCount <- count(disabilityData, disability, wt = PWGTP)
disabilityCount$percent <- percent(disabilityCount$n/sum(disabilityCount$n))
disabilityCount$label <- paste(disabilityCount$disability, "\n", disabilityCount$percent)

treemap(disabilityCount, "label", "n", title = "")


## Household income
householdIncome <- count(notInLaborForce, householdIncome, wt = PWGTP)
householdIncome <- na.omit(householdIncome)
householdIncome$percent <- percent(householdIncome$n/sum(householdIncome$n))
householdIncome$label <- paste(householdIncome$householdIncome, "\n", householdIncome$percent)

treemap(householdIncome, "label", "n", title = "")

## AGE
householdIncome <- count(notInLaborForce, householdIncome, wt = PWGTP)
householdIncome <- na.omit(householdIncome)
householdIncome$percent <- percent(householdIncome$n/sum(householdIncome$n))
householdIncome$label <- paste(householdIncome$householdIncome, "\n", householdIncome$percent)

treemap(householdIncome, "label", "n", title = "")






