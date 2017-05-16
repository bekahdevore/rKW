# Can you use the PUMS data to determine how many 16-21 year-olds in Jefferson County are:
#   Not working: 29395
#   Not in school: 14142
#   Not working and not in school: 4169
#   African-American: 15561
#   White: 37098
#   Living below poverty: 11859
#           White: 6019
#           Black: 4687
#   

library(dplyr)

## GET PUMS
kentucky <- read.csv("ss15pky.csv")

## FILTER TO JEFFERSON COUNTY
jeffersonCountyPumas <- c('1701',
                          '1702',
                          '1703',
                          '1704',
                          '1705',
                          '1706')

allData <- kentucky %>% filter(PUMA %in% jeffersonCountyPumas)

## FILTER YOUTH
youth <- allData %>% filter(AGEP >= 16 & AGEP <= 24)
sum(youth$PWGTP)
## Make new variables to clarify questions

## CALCULATE DATA POINTS
notWorking <- youth %>% filter(ESR == 3 | ESR == 6) %>% select(ESR, AGEP, PWGTP, SCH)
sum(notWorking$PWGTP)

notInSchool <- youth %>% filter(SCH == 1) %>% select(SCH, AGEP, PWGTP)
sum(notInSchool$PWGTP)

notInSchoolNotWorking <- notWorking %>% filter(SCH == 1) %>% select(AGEP, PWGTP, SCH, ESR)
sum(notInSchoolNotWorking$PWGTP)

black <- youth %>% filter(RAC1P == 2) %>% select(RAC1P, AGEP, PWGTP)
sum(black$PWGTP)

white <- youth %>% filter(RAC1P == 1) %>% select(RAC1P, AGEP, PWGTP)
sum(white$PWGTP)

poverty <- youth %>% filter(POVPIP <= 100)
sum(poverty$PWGTP)

whitePoverty <- poverty %>% filter(RAC1P == 1)
sum(whitePoverty$PWGTP)

blackPoverty <- poverty %>% filter(RAC1P == 2)
sum(blackPoverty$PWGTP)
