# What's the total number for our MSA?  For the state of KY?

library(RCurl)
library(dplyr)
library(plotrix)
library(scales)
library(plotly)
library(ggplot2)
library(tidyr)
library(treemap)

# Add data
# youth16to19 <- read.csv("youth16to19.csv")
# youth16to21 <- read.csv("youth16to21.csv")
#peerCityPums <- load("peerCityPums.RData")
peerCityPums <- read.csv("peerCityPUMS.csv")
# youth16to19Title <- "Youth 16 to 19"
# youth16to21Title <- "Youth 16 to 21"
#peerCityPums <- peerCityPums %>% filter(AGEP >= 16 & AGEP <= 21) %>% filter(ESR == 6 | ESR == 3) %>% 
#  filter(SCH == 1)

peerCityPums <- peerCityPums %>% filter(AGEP >= 16)

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
                    ifelse((AGEP >= 16 & AGEP <= 21), "Sixteen - Twenty-One", 
                           ifelse((AGEP >= 22 & AGEP <= 35), "Twenty-Two - Thirty-Five",
                                  ifelse((AGEP >= 36 & AGEP <= 54), "Thirty-Six - Fifty-Four",
                                         ifelse((AGEP >= 55), "Fifty-Five or older", "Other"))))) %>% 
    mutate(sex = 
             ifelse(SEX == 1, "Male", 
                    ifelse(SEX == 2, "Female", "Other")))
}


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

# youth16to19Disability <- disabilityClean(youth16to19)
# youth16to21Disability <- disabilityClean(youth16to21)
peerCityPumsDisability <- disabilityClean(peerCityPums)

## Disability
# 16 - 19
# youth16to19Disability <- dplyr::count(youth16to19Disability, disability, wt = PWGTP)
# youth16to19Disability$percent <- percent(youth16to19Disability$n/sum(youth16to19Disability$n))
# youth16to19Disability$label <- paste(youth16to19Disability$disability, "\n", youth16to19Disability$percent)
# View(youth16to19Disability)
# 
# treemap(youth16to19Disability, "label", "n", title = youth16to19Title)
# 
# # 16 - 21
# youth16to21Disability <- count(youth16to21Disability, disability, wt = PWGTP)
# youth16to21Disability$percent <- percent(youth16to21Disability$n/sum(youth16to21Disability$n))
# youth16to21Disability$label <- paste(youth16to21Disability$disability, "\n", youth16to21Disability$percent)
# View(youth16to21Disability)
# 
# treemap(youth16to21Disability, "label", "n", title = youth16to21Title)

# Peer City Pums
peerCityPumsDisability <- count(peerCityPumsDisability, disability, wt = PWGTP)
peerCityPumsDisability$percent <- percent(peerCityPumsDisability$n/sum(peerCityPumsDisability$n))
peerCityPumsDisability$label <- paste(peerCityPumsDisability$disability, "\n", peerCityPumsDisability$percent)

treemap(peerCityPumsDisability, "label", "n", title = "Peer City Population")

# popluationDisabilitySpread <- populationDisability %>% select(State, disability, percent) %>% spread(disability, percent)
# notInLaborForceDisabilitySpread <- notInLaborForceDisability %>% select(State, disability, percent) %>% spread(disability, percent)
# unemployedDisabilitySpread <- unemployedDisability %>% select(State, disability, percent) %>% spread(disability, percent)
# 
# disabilityData <- rbind(popluationDisabilitySpread, unemployedDisabilitySpread, notInLaborForceDisabilitySpread)
# write.csv(disabilityData, file = "disabilityData.csv")
# 
# # treemap(disabilityCount, "label", "n", title = "")

# youth16to19Clean <- cleanData(youth16to19)
# youth16to21Clean <- cleanData(youth16to21)
peerCityPumsClean <- cleanData(peerCityPums)


## By race
#16 - 19
# youth16to19Race <- count(youth16to19Clean, race, wt = PWGTP)
# youth16to19Race$percent <- percent(youth16to19Race$n/sum(youth16to19Race$n))
# youth16to19Race$label <- paste(youth16to19Race$race, "\n", youth16to19Race$percent)
# 
# treemap(youth16to19Race, "label", "n", title = youth16to19Title)
# 
# 
# #16 - 21
# youth16to21Race <- dplyr::count(youth16to21Clean, race, wt = PWGTP)
# youth16to21Race$percent <- percent(youth16to21Race$n/sum(youth16to21Race$n))
# youth16to21Race$label <- paste(youth16to21Race$race, "\n", youth16to21Race$percent)
# 
# treemap(youth16to21Race, "label", "n", title = youth16to21Title)

## Peer City Pums
peerCityPumsRace <- count(peerCityPumsClean, race, wt = PWGTP)
peerCityPumsRace$percent <- percent(peerCityPumsRace$n/sum(peerCityPumsRace$n))
peerCityPumsRace$label <- paste(peerCityPumsRace$race, "\n", peerCityPumsRace$percent)

treemap(peerCityPumsRace, "label", "n", title = peerCityPumsTitle)


# notInLaborForceRaceSpread <- notInLaborForceRace %>% select(State, race, percent) %>% spread(race, percent)
# unemployedRaceSpread <- unemployedRace %>% select(State, race, percent) %>% spread(race, percent)
# 
# raceData <- rbind(sixteenPlusRaceSpread, unemployedRaceSpread, notInLaborForceRaceSpread)
# 
# treemap(race, "label", "n", title = "")


# ## education
# populationEducation <- count(sixteenPlusClean, education, wt = PWGTP)
# populationEducation$percent <- (populationEducation$n/sum(populationEducation$n))
# #populationEducation$label <- paste(populationEducation$education, "\n", populationEducation$percent)
# populationEducation$State <- "Population"
# 
# notInLaborForceEducation <- count(notInLaborForceClean, education, wt = PWGTP)
# notInLaborForceEducation$percent <- (notInLaborForceEducation$n/sum(notInLaborForceEducation$n))
# #notInLaborForceEducation$label <- paste(notInLaborForceEducation$education, "\n", notInLaborForceEducation$percent)
# notInLaborForceEducation$State <- "Not in Labor Force"
# 
# unemployedEducation <- count(unemployedClean, education, wt = PWGTP)
# unemployedEducation$percent <- (unemployedEducation$n/sum(unemployedEducation$n))
# #unemployedEducation$label <- paste(unemployedEducation$education, "\n", unemployedEducation$percent)
# unemployedEducation$State <- "Unemployed"
# 
# populationEducationSpread <- populationEducation %>% select(State, education, percent) %>% spread(education, percent)
# notInLaborForceEducationSpread <- notInLaborForceEducation %>% select(State, education, percent) %>% spread(education, percent)
# unemployedEducationSpread <- unemployedEducation %>% select(State, education, percent) %>% spread(education, percent)
# 
# educationData <- rbind(populationEducationSpread, unemployedEducationSpread, notInLaborForceEducationSpread)
# write.csv(educationData, file = "educationData.csv")


#treemap(education, "label", "n", title = "")

peerCityPumsEducation <- count(peerCityPumsClean, education, wt = PWGTP)
peerCityPumsEducation$percent <- percent(peerCityPumsEducation$n/sum(peerCityPumsEducation$n))
peerCityPumsEducation$label <- paste(peerCityPumsEducation$education, "\n", peerCityPumsEducation$percent)
treemap(peerCityPumsEducation, "label", "n", title = "")



## Sex

# youth16to19Sex <- count(youth16to19Clean, sex, wt = PWGTP)
# youth16to19Sex$percent <-  percent(youth16to19Sex$n/sum(youth16to19Sex$n))
# youth16to19Sex$label <- paste(youth16to19Sex$sex, "\n", youth16to19Sex$percent)
# 
# treemap(youth16to19Sex, "label", "n", title = youth16to19Title)
# 
# 
# youth16to21Sex <- dplyr::count(youth16to21Clean, sex, wt = PWGTP)
# youth16to21Sex$percent <-  percent(youth16to21Sex$n/sum(youth16to21Sex$n))
# youth16to21Sex$label <- paste(youth16to21Sex$sex, "\n", youth16to21Sex$percent)
# 
# treemap(youth16to21Sex, "label", "n", title = youth16to21Title)

peerCityPumsSex <- dplyr::count(peerCityPumsClean, sex, wt = PWGTP)
peerCityPumsSex$percent <-  percent(peerCityPumsSex$n/sum(peerCityPumsSex$n))
peerCityPumsSex$label <- paste(peerCityPumsSex$sex, "\n", peerCityPumsSex$percent)

treemap(peerCityPumsSex, "label", "n", title = peerCityPumsTitle)


# sixteenPlusSexSpread <- sixteenPlusSex %>% select(State, SEX, percent) %>% spread(SEX, percent)
# notInLaborForceSexSpread <- notInLaborForceSex %>% select(State, SEX, percent) %>% spread(SEX, percent)
# unemployedSexSpread <- unemployedSex %>% select(State, SEX, percent) %>% spread(SEX, percent)
# 
# sexData <- rbind(sixteenPlusSexSpread, unemployedSexSpread, notInLaborForceSexSpread)
# write.csv(sexData, file = "sexData.csv")




## Household Age
# #16-19
# youth16to19Income <- count(youth16to19Clean, householdIncome, wt = PWGTP)
# youth16to19Income <- na.omit(youth16to19Income)
# youth16to19Income$percent <- percent(youth16to19Income$n/sum(youth16to19Income$n))
# youth16to19Income$label <- paste(youth16to19Income$householdIncome, "\n", youth16to19Income$percent)
# 
# treemap(youth16to19Income, "label", "n", title = youth16to19Title)
# 
# #16-21
# youth16to21Income <- count(youth16to21Clean, householdIncome, wt = PWGTP)
# youth16to21Income <- na.omit(youth16to21Income)
# youth16to21Income$percent <- percent(youth16to21Income$n/sum(youth16to21Income$n))
# youth16to21Income$label <- paste(youth16to21Income$householdIncome, "\n", youth16to21Income$percent)
# 
# treemap(youth16to21Income, "label", "n", title = youth16to21Title)

peerCityPumsIncome <- count(peerCityPumsClean, householdIncome, wt = PWGTP)
peerCityPumsIncome <- na.omit(peerCityPumsIncome)
peerCityPumsIncome$percent <- percent(peerCityPumsIncome$n/sum(peerCityPumsIncome$n))
peerCityPumsIncome$label <- paste(peerCityPumsIncome$householdIncome, "\n", peerCityPumsIncome$percent)

treemap(peerCityPumsIncome, "label", "n", title = "Peer City Pums")




## AGE

# #16-19
# youth16to19Age <- count(youth16to19Clean, AGEP, wt = PWGTP)
# youth16to19Age <- na.omit(youth16to19Age)
# youth16to19Age$percent <- percent(youth16to19Age$n/sum(youth16to19Age$n))
# youth16to19Age$label <- paste(youth16to19Age$AGEP, "\n", youth16to19Age$percent)
# 
# treemap(youth16to19Age, "label", "n", title = youth16to19Title)
# 
# #16-21
# youth16to21Age <- count(youth16to21Clean, AGEP, wt = PWGTP)
# youth16to21Age <- na.omit(youth16to21Age)
# youth16to21Age$percent <- percent(youth16to21Age$n/sum(youth16to21Age$n))
# youth16to21Age$label <- paste(youth16to21Age$AGEP, "\n", youth16to21Age$percent)
# 
# treemap(youth16to21Age, "label", "n", title = youth16to21Title)

peerCityPumsAge <- count(peerCityPumsClean, age, wt = PWGTP)
peerCityPumsAge <- na.omit(peerCityPumsAge)
peerCityPumsAge$percent <- percent(peerCityPumsAge$n/sum(peerCityPumsAge$n))
peerCityPumsAge$label <- paste(peerCityPumsAge$age, "\n", peerCityPumsAge$percent)

treemap(peerCityPumsAge, "label", "n", title = "")





