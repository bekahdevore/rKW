# LOAD  PACKAGES
library(RCurl)
library(dplyr)
library(ENmisc)
library(spatstat)

# LOAD DATA
occpGroupConnection <- getURL("https://docs.google.com/spreadsheets/d/1n2WLOqZjzUwJHmx8s3ILh0L0o54CjUaJT6_Yk4rcNio/pub?gid=0&single=true&output=csv")
fod1pGroupConnection <- getURL("https://docs.google.com/spreadsheets/d/1uE6r5SxXnw1z14bouQqi0_1cMd9xshvCgf9RGWpWVAA/pub?gid=0&single=true&output=csv")
naicsGroupConnection <- getURL("https://docs.google.com/spreadsheets/d/1VGfsMCB3qF7WwPAbmQglxumh-9_Fj6ROdKXkNMGL-Do/pub?gid=0&single=true&output=csv")

occpGroup <- read.csv(textConnection(occpGroupConnection))
fod1pGroup <- read.csv(textConnection(fod1pGroupConnection))
naicsGroup <- read.csv(textConnection(naicsGroupConnection))
rm(occpGroupConnection, fod1pGroupConnection, naicsGroupConnection)

load("peerCities.RData")

# If all the people NOT in the workforce were 100 people:
## FILTER TO PEOPLE NOT IN WORKFORCE, ESR == 6

peerCities <- peerCities %>% filter(ESR == 6 & AGEP >= 16)

startingPopulation <- count(peerCities, wt=PWGTP)  
print(startingPopulation)

############################ Labor Force ###########################

#         X% are over the age of 55
over55 <- peerCities %>% mutate(over55 =  ifelse(AGEP >= 55, "Yes", "No"))

over55Count <- count(over55, over55, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(over55Count)

#         x% have a household income more than $100,000 (i.e. these people are too rich to need to work)
householdIncome <- over55 %>% filter(over55 == "No") %>% 
  mutate(incomeMoreThanX = ifelse(HINCP >= 75000, "Yes", "No"))
incomeCount <- count(householdIncome, incomeMoreThanX, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(incomeCount)

#         x% have a disability
disability <- householdIncome %>% filter(incomeMoreThanX == "No") %>% 
  mutate(disability = ifelse(DIS == 1, "Yes", "No"))
disabilityCount <- count(disability, disability, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(disabilityCount)

#         x% are the primary caretakers for dependents
primaryCaretakers <- disability %>% filter(disability == "No") %>% 
  mutate(primaryCaretaker = ifelse((HHT != 1 & HUPAOC <= 3 & PARTNER == 0), "Yes", "No")) 

primaryCaretakersCount <- count(primaryCaretakers, primaryCaretaker, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(primaryCaretakersCount)

#         x% are currently in school
enrolledInSchool <- primaryCaretakers %>% filter(primaryCaretaker == "No") %>% 
  mutate(enrolledInSchool = ifelse(SCH >= 2, "Yes", "No"))
enrolledCount <- count(enrolledInSchool, enrolledInSchool, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(enrolledCount)



#  x% not worked in 5 years or longer
notWorkedIn5yearsPlus <- enrolledInSchool %>% filter(enrolledInSchool == "No") %>% 
  mutate(notWorked = ifelse(WKL == 3, "Yes", "No"))

notWorkedCount <- count(notWorkedIn5yearsPlus, notWorked, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(notWorkedCount)

### EDIT
#         x% have a HS education or less
hsEducationOrLess <- notWorkedIn5yearsPlus %>% filter(notWorked == "No") %>% 
  mutate(hsEducationOrLess = ifelse(SCHL <= 15, "Yes", "No"))
hsEducationOrLessCount <- count(hsEducationOrLess, hsEducationOrLess, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(hsEducationOrLessCount)



leftover <- hsEducationOrLess %>% filter(hsEducationOrLess == "No")
sum(leftover$PWGTP)
leftover <- left_join(leftover, occpGroup, by = "OCCP")
leftover <- left_join(leftover, fod1pGroup, by = "FOD1P")
leftover <- left_join(leftover, naicsGroup, by = "NAICSP")
leftover <- leftover %>% mutate(ageGroup = ifelse(AGEP <= 19, "16 - 19", 
                                                  ifelse(AGEP >= 20 & AGEP <= 24, "20 - 24", 
                                                         ifelse(AGEP >= 25 & AGEP <= 29, "25 - 29",
                                                                ifelse(AGEP >= 30 & AGEP <= 34, "30 - 34", 
                                                                       ifelse(AGEP >= 35 & AGEP <= 44, "35 - 44", 
                                                                              ifelse(AGEP >= 45 & AGEP <= 54, "45 - 54", "Other"))))))) %>% 
  mutate(familySize = ifelse(NPF >= 3, "3 or more", "Other"))

wt <- leftover$PWGTP
wtd.boxplot(leftover$AGEP, weights = wt)
wtd.boxplot(leftover$CITWP, weights = wt )
wtd.boxplot(leftover$COW, weights = wt )
wtd.boxplot(leftover$NPF, weights = wt )


fod1p <- count(leftover, majorShort, wt = PWGTP, sort = TRUE)
fod1p
percent(fod1p)

wkl <- count(leftover, WKL, wt = PWGTP, sort = TRUE)
wkl
percent(wkl)

naics <- count(leftover, naicsGroup, wt = PWGTP, sort = TRUE)
naics
percent(naics)

nativity <- count(leftover, NATIVITY, wt = PWGTP, sort = TRUE)
nativity
percent(nativity)

occ <- count(leftover, occGroup, wt = PWGTP, sort = TRUE)
occ
percent(occ)

race <- count(leftover, RAC1P, wt = PWGTP, sort = TRUE)
race
percent(race)

age <- count(leftover, ageGroup, wt = PWGTP, sort = FALSE)
age
percent(age)

familySize <- count(leftover, familySize, wt = PWGTP, sort = TRUE)
familySize
percent(familySize)

fb <- leftover %>% filter(NATIVITY == 2)
fb <- fb %>% mutate(yoe = ifelse(YOEP >= 2011, "2011 +", "Other"))
fbYear <- count(fb, yoe, wt = PWGTP, sort = TRUE)
percent(fbYear)

yon <- count(leftover, CITWP, wt = PWGTP, sort = TRUE)
percent(yon)

# type of tenure (own, rent, etc)
ten <- count(leftover, TEN, wt = PWGTP, sort = TRUE)
percent(ten)

# household type
hht <- count(leftover, HHT, wt = PWGTP, sort = TRUE)
head(percent(hht)) 

left_partner <- count(leftover, PARTNER, wt = PWGTP, sort = TRUE)
percent(left_partner)

wif <- count(leftover, WIF, wt = PWGTP, sort = TRUE)
percent(wif)

workstat <- count(leftover, WORKSTAT, wt = PWGTP, sort = TRUE)
head(percent(workstat))

# property value
valp <- leftover %>% mutate(valp = ifelse(VALP >= 160000, "More than 160k", "Less than 160k"))
valp <- count(valp, valp, wt = PWGTP, sort = TRUE)
valp <- na.omit(valp)
percent(valp)

vehicles <- count(leftover, VEH, wt = PWGTP, sort = TRUE)
percent(vehicles)

ybl <- count(leftover, YBL, wt = PWGTP, sort = TRUE)
head(percent(ybl))

# rent as a percent of household income
rentPercent <- leftover %>% filter(TEN == 3) %>%
  mutate(rentPercent = ifelse(GRPIP >= 30, "Rent more than 30% of household income", "Not"))
rentPercent <- count(rentPercent, rentPercent, wt = PWGTP, sort = TRUE)
rentPercent <- na.omit(rentPercent)
percent(rentPercent)

wtd.mean(leftover$GRNTP, leftover$PWGTP) # mean rent
weighted.median(leftover$GRNTP, leftover$PWGTP) # median rent
weighted.median(leftover$HINCP, leftover$PWGTP) # median household income


