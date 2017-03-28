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
                                        ifelse(HINCP >= 210000, "> 210k", "Other"))))))))) %>% 
                          mutate(age = 
                                   ifelse(AGEP >= 16 & AGEP <= 21, "16 - 21", 
                                   ifelse(AGEP >= 22 & AGEP <= 35, "22 - 35",
                                   ifelse(AGEP >= 36 & AGEP <= 45, "36 - 45", 
                                   ifelse(AGEP >= 46 & AGEP <= 55, "46 - 54", 
                                   ifelse(AGEP >= 55, " 55 +", "Other"))))))
}

unemployedClean <- cleanData(unemployed)
notInLaborForceClean <- cleanData(notInLaborForce)
sixteenPlusClean <- cleanData(sixteenPlus)

## Disability data
disabilityClean <- function(enterDataHere) {
  disability <- enterDataHere %>% 
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
}

notInLaborForceDisability <- disabilityClean(notInLaborForceClean)
populationDisability <- disabilityClean(sixteenPlusClean)
unemployedDisability <- disabilityClean(unemployedClean)

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


## education
populationEducation <- count(sixteenPlusClean, education, wt = PWGTP)
populationEducation$percent <- (populationEducation$n/sum(populationEducation$n))
#populationEducation$label <- paste(populationEducation$education, "\n", populationEducation$percent)
populationEducation$State <- "Population"

notInLaborForceEducation <- count(notInLaborForceClean, education, wt = PWGTP)
notInLaborForceEducation$percent <- (notInLaborForceEducation$n/sum(notInLaborForceEducation$n))
#notInLaborForceEducation$label <- paste(notInLaborForceEducation$education, "\n", notInLaborForceEducation$percent)
notInLaborForceEducation$State <- "Not in Labor Force"

unemployedEducation <- count(unemployedClean, education, wt = PWGTP)
unemployedEducation$percent <- (unemployedEducation$n/sum(unemployedEducation$n))
#unemployedEducation$label <- paste(unemployedEducation$education, "\n", unemployedEducation$percent)
unemployedEducation$State <- "Unemployed"

populationEducationSpread <- populationEducation %>% select(State, education, percent) %>% spread(education, percent)
notInLaborForceEducationSpread <- notInLaborForceEducation %>% select(State, education, percent) %>% spread(education, percent)
unemployedEducationSpread <- unemployedEducation %>% select(State, education, percent) %>% spread(education, percent)

educationData <- rbind(populationEducationSpread, unemployedEducationSpread, notInLaborForceEducationSpread)
write.csv(educationData, file = "educationData.csv")


#treemap(education, "label", "n", title = "")


## Sex
    # sex <- count(notInLaborForce, SEX, wt = PWGTP)
    # sex$sex <- c("Male", "Female")
    # sex$percent <- percent(sex$n/sum(sex$n))
    # sex$label <- paste(sex$sex, "\n", sex$percent)
    
    # treemap(sex, "label", "n", title = "")
    unemployedSex <- count(unemployedClean, SEX, wt = PWGTP)
    unemployedSex$percent <-  (unemployedSex$n/sum(unemployedSex$n))
    #unemployedSex$label <- paste(unemployedSex$sex, "\n", unemployedSex$percent)
    unemployedSex$State <- "Unemployed"
    
    notInLaborForceSex <- dplyr::count(notInLaborForceClean, SEX, wt = PWGTP)
    notInLaborForceSex$percent <-  notInLaborForceSex$n/sum(notInLaborForceSex$n)
    #notInLaborForceSex$label <- paste(notInLaborForceSex$sex, "\n", notInLaborForceSex$percent)
    notInLaborForceSex$State <- "Not in Labor Force"
    
    sixteenPlusSex <- dplyr::count(sixteenPlusClean, SEX, wt = PWGTP)
    sixteenPlusSex$percent <-  (sixteenPlusSex$n/sum(sixteenPlusSex$n))
    #sixteenPlusSex$label <- paste(sixteenPlusSex$sex, "\n", sixteenPlusSex$percent)
    sixteenPlusSex$State <- "Population"
    
    sixteenPlusSexSpread <- sixteenPlusSex %>% select(State, SEX, percent) %>% spread(SEX, percent)
    notInLaborForceSexSpread <- notInLaborForceSex %>% select(State, SEX, percent) %>% spread(SEX, percent)
    unemployedSexSpread <- unemployedSex %>% select(State, SEX, percent) %>% spread(SEX, percent)
    
    sexData <- rbind(sixteenPlusSexSpread, unemployedSexSpread, notInLaborForceSexSpread)
    write.csv(sexData, file = "sexData.csv")

## Disability
  populationDisability <- dplyr::count(populationDisability, disability, wt = PWGTP)
  populationDisability$percent <-(populationDisability$n/sum(populationDisability$n))
  #populationDisability$label <- paste(populationDisability$disability, "\n", populationDisability$percent)
  populationDisability$State <- "Population"
  
  notInLaborForceDisability <- count(notInLaborForceDisability, disability, wt = PWGTP)
  notInLaborForceDisability$percent <- (notInLaborForceDisability$n/sum(notInLaborForceDisability$n))
  #notInLaborForceDisability$label <- paste(notInLaborForceDisability$disability, "\n", notInLaborForceDisability$percent)
  notInLaborForceDisability$State <- "Not In Labor Force"
  
  unemployedDisability <- count(unemployedDisability, disability, wt = PWGTP)
  unemployedDisability$percent <- (unemployedDisability$n/sum(unemployedDisability$n))
  #unemployedDisability$label <- paste(unemployedDisability$disability, "\n", unemployedDisability$percent)
  unemployedDisability$State <- "Unemployed"
  selfCare <- data.frame("Self-Care", 0, 0, "Unemployed")
  visual <- data.frame("Visual", 0, 0, "Unemployed")
  names(selfCare) <- c("disability", "n", "percent", "State")
  names(visual) <- c("disability", "n", "percent", "State")
  unemployedDisability <- rbind(unemployedDisability, selfCare, visual)
  
    
  popluationDisabilitySpread <- populationDisability %>% select(State, disability, percent) %>% spread(disability, percent)
  notInLaborForceDisabilitySpread <- notInLaborForceDisability %>% select(State, disability, percent) %>% spread(disability, percent)
  unemployedDisabilitySpread <- unemployedDisability %>% select(State, disability, percent) %>% spread(disability, percent)
  
  disabilityData <- rbind(popluationDisabilitySpread, unemployedDisabilitySpread, notInLaborForceDisabilitySpread)
  write.csv(disabilityData, file = "disabilityData.csv")

# treemap(disabilityCount, "label", "n", title = "")


## Household income
householdIncome <- count(notInLaborForce, householdIncome, wt = PWGTP)
householdIncome <- na.omit(householdIncome)
householdIncome$percent <- percent(householdIncome$n/sum(householdIncome$n))
householdIncome$label <- paste(householdIncome$householdIncome, "\n", householdIncome$percent)

treemap(householdIncome, "label", "n", title = "")

## AGE

unemployedAge <- count(unemployedClean, age, wt = PWGTP)
unemployedAge$percent <-  (unemployedAge$n/sum(unemployedAge$n))
#unemployedAge$label <- paste(unemployedAge$age, "\n", unemployedAge$percent)
unemployedAge$State <- "Unemployed"

notInLaborForceAge <- dplyr::count(notInLaborForceClean, age, wt = PWGTP)
notInLaborForceAge$percent <-  notInLaborForceAge$n/sum(notInLaborForceAge$n)
#notInLaborForceAge$label <- paste(notInLaborForceAge$age, "\n", notInLaborForceAge$percent)
notInLaborForceAge$State <- "Not in Labor Force"


sixteenPlusAge <- dplyr::count(sixteenPlusClean, age, wt = PWGTP)
sixteenPlusAge$percent <-  (sixteenPlusAge$n/sum(sixteenPlusAge$n))
#sixteenPlusAge$label <- paste(sixteenPlusAge$age, "\n", sixteenPlusAge$percent)
sixteenPlusAge$State <- "Population"

sixteenPlusAgeSpread <- sixteenPlusAge %>% select(State, age, percent) %>% spread(age, percent)
notInLaborForceAgeSpread <- notInLaborForceAge %>% select(State, age, percent) %>% spread(age, percent)
unemployedAgeSpread <- unemployedAge %>% select(State, age, percent) %>% spread(age, percent)

notInLaborForceAgeSpread <- notInLaborForceAgeSpread %>% select(-Other)

ageData <- rbind(sixteenPlusAgeSpread, unemployedAgeSpread, notInLaborForceAgeSpread)
write.csv(ageData, file = "ageData.csv")
ageData




