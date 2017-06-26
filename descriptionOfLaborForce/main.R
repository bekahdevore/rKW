#load packages
library(dplyr)
library(tidyr)
library(lazyeval)
# get peer city data
load("peerCities.RData")

# filter 16 and overand < 55
peerCities <- peerCities %>% filter(AGEP >= 16 & AGEP <=55)

cleanData <- peerCities %>%
  ## AGE GROUPS
  mutate(age = ifelse(AGEP >= 16 & AGEP <= 19, "16-19", 
                        ifelse(AGEP >= 20 & AGEP <= 24, "20-24",
                               ifelse(AGEP >= 25 & AGEP <= 29, "25-29", 
                                      ifelse(AGEP >= 30 & AGEP <= 34, "30-34", 
                                             ifelse(AGEP >= 35 & AGEP <= 44, "35-44", 
                                                    ifelse(AGEP >= 45 & AGEP <= 54, "45-54", "Other"))))))) %>%
  ## RACE
  mutate(race = ifelse(RAC1P == 1, "White", 
                       ifelse(RAC1P == 2, "Black", "Other"))) %>% 
  mutate(hispanic = ifelse(HISP == 1, "non-Hispanic", "Hispanic")) %>%
  ##  SEX
  mutate(sex = ifelse(SEX == 1, "Male", "Female")) %>% 
  ## ED ATTAINMENT
  mutate(eduAttainment = ifelse(SCHL <= 15, "Less than High School", 
                                ifelse(SCHL >= 16 | SCHL <= 19, "High School", 
                                       ifelse(SCHL == 20, "Associates", 
                                              ifelse(SCHL == 21, "Bachelors", 
                                                     ifelse(SCHL == 22, "Masters", 
                                                            ifelse(SCHL >= 23, "Professional or Graduate", "Other")))))))  %>%
  ## RELIES ON TRANSIT
  mutate(publicTransit = ifelse(JWTR == 2 | JWTR == 3, "Public Transit", "Other")) %>% 
  ## Number of Vehicles, VEH
  ##  Single Parent Home
  ## Primary caregiver
  mutate(primaryCaretaker = ifelse((HHT != 1 & HUPAOC <= 3 & PARTNER == 0), "Primary Caretaker", "Not Primary Caretaker")) %>% 
  ## Median Household Income
  mutate(householdIncome = ifelse(HINCP >= 50190, "Above Family-Supporting Wage", "Other")) %>%
  ## Poverty Status
  mutate(poverty = ifelse(POVPIP >= 100, "In Poverty", "Other")) %>%
  ## ESR STATUS
  mutate(wf = ifelse(ESR == 1 | ESR == 2, "Employed", 
                     ifelse(ESR == 3, "Unemployed", 
                            ifelse(ESR == 6, "Not in Labor Force", "Other"))))
  
  ## Language (Eng. vs. non-Eng.)
  ## Industry
  ## Occupation


# group by ESR group (Not in LF, Unemployed, Employed)

#Visualize difference in groups

## STOPPED HERE MONDAY WORKING ON DEVELOPING FUNCTION TO PRINT PERCENTS
sex <- variableWF("sex", "wf")
age <- variableWF("age", "wf")
age %>% mutate(sumEmployed = sum(age[,"Employed"]))
sum(age$Employed)
