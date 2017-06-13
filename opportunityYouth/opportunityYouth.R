library(dplyr)
library(RMySQL)
library(scales)
library(treemap)

#  MySQL database connection
con <- dbConnect(MySQL(), group = "kwlmi", dbname = "kwlmi")

## Variables
kentucky <- "kentuckyPUMS"
peerCities <- "peerCityPUMS"
louisville <- "louisvilleMSA_PUMS"
## Add variables to grab from PUMS data here
pumsVariables <- "AGEP,SCHL,SCH,ESR,RAC1P,SEX,PWGTP,DIS,WKL,HINCP,DREM,FER,PAOC,SSIP,SSP,FS"

### FUNCTIONS
percentNew <- function(data){
  data <-  data %>% mutate(per=n/sum(n))
  print(data)
}

statement <- function(place) {
  paste("SELECT", pumsVariables, "FROM", place, ";")
}


# Pull data from MySQL Database, change place argument in statement to run different different population data
kentucky <- dbGetQuery(conn = con, statement = statement(kentucky))
peerCities <- dbGetQuery(conn = con, statement = statement(peerCities))
louisvilleMSA <- dbGetQuery(conn = con, statement = statement(louisville))
                            
save(kentucky, file = "kentuckyAllData.RData")
save(peerCities, file = "peerCitiesAllData.RData")
save(louisvilleMSA, file = "louisvilleMsaAllData.RData")

## Filter and Make data more readable
dataFilter <- function(dataInput) {
    opportunityYouth <- dataInput %>% 
                          filter(ESR == 3 | ESR == 6) %>% 
                          filter(SCH == 1) %>%
                          filter(AGEP >= 16 & AGEP <= 24)
    
    opportunityYouth <- opportunityYouth %>% mutate(race = ifelse(RAC1P == 1, "White", 
                                                                  ifelse(RAC1P == 2, "Black", 
                                                                         ifelse(RAC1P == 3, "American Indian", 
                                                                                ifelse(RAC1P == 8, "Some other race alone", 
                                                                                       ifelse(RAC1P == 9, "Two or more races", "Other"))))))
    opportunityYouth <- opportunityYouth %>% mutate(disability = ifelse(DIS == 1, "With disability",
                                                                        ifelse(DIS == 2, "Without disability", "Other")))
    
    opportunityYouth <- opportunityYouth %>% mutate(cognitiveDifficulty = ifelse(DREM == 1, "With Cognitive Difficulty", 
                                                                                  ifelse(DREM == 2, "Without Cognitive Difficulty", "Other")))
    ### FEMALES ONLY ###################################################
    opportunityYouth <- opportunityYouth %>% mutate(childWithin12Months = ifelse(FER == 1, "Gave birth to child in past 12 months", 
                                                                                 ifelse(FER == 2, "Did not give birth in past 12 months", "Other")))
    
    opportunityYouth <- opportunityYouth %>% mutate(children = ifelse(PAOC == 1, "Children under 6", 
                                                                      ifelse(PAOC == 2, "Children 6 - 17 yrs. old", 
                                                                             ifelse(PAOC == 3, "Children under 6 & 6 - 17 years old", 
                                                                                    ifelse(PAOC == 4, "No children", "Other")))))
    #################################################################
    #################################################################
    opportunityYouth <- opportunityYouth %>% mutate(supplementarySecurityIncome = ifelse(SSIP <= 0, "No Supplementary Security income in the past 12 months", 
                                                                                         ifelse(SSIP > 0, "Received Supplementary Security income in the past 12 months", "Other")))
    
    opportunityYouth <- opportunityYouth %>% mutate(socialSecurity = ifelse(SSP <= 0, "No Social Security Income in the past 12 months", 
                                                                            ifelse(SSP > 0, "Received Social Security Income in the past 12 months", "Other" )))
    
    opportunityYouth <- opportunityYouth %>% mutate(foodStamps = ifelse(FS == 1, "Received SNAP benefits", 
                                                                        ifelse(FS == 2, "Did not receive SNAP benefits", "Other")))
    
    opportunityYouth <- opportunityYouth %>% mutate(lastWorked = ifelse(WKL == 1, "Last worked within past 12 months", 
                                                                        ifelse(WKL == 2, "Last worked 1 - 5 years ago", 
                                                                               ifelse(WKL == 3, "Last worked over 5 years ago or never", "Other"))))
    
    
    opportunityYouth <- opportunityYouth %>% mutate(sex = ifelse(SEX == 1, "Male", 
                                                                 ifelse(SEX == 2, "Female", "Other")))
    
    opportunityYouth <- opportunityYouth %>% mutate(employmentStatus = ifelse(ESR == 3, "Unemployed", 
                                                                              ifelse(ESR == 6, "Not in labor force", "Other")))
    
    opportunityYouth <- opportunityYouth %>% mutate(age = ifelse(AGEP <= 19, "16 - 19 yrs. old", 
                                                    ifelse(AGEP >= 20, "20 - 24 yrs. old", "Other")))
    
    opportunityYouth <- opportunityYouth %>% mutate(education = ifelse(SCHL <= 11, "Middle School (8th grade) or less", 
                                                                                     ifelse(SCHL >= 12 & SCHL <= 15, "Less than high school diploma", 
                                                                                            ifelse(SCHL >= 16 & SCHL <= 17, "High diploma or equivalent", 
                                                                                                   ifelse(SCHL >= 18 & SCHL <= 19, "Some college, no degree", 
                                                                                                          ifelse(SCHL == 20, "Associate's degree", 
                                                                                                                 ifelse(SCHL == 21, "Bachelor's degree", "Other")))))))
}

kentucky <- dataFilter(kentucky)
peerCities <- dataFilter(peerCities)
louisvilleMSA <- dataFilter(louisvilleMSA)

save(kentucky, file = "kentucky.RData")
save(peerCities, file = "peerCities.RData")
save(louisvilleMSA, file = "louisvilleMSA.RData")

## WRITE DATA HERE AND RUN OTHER FUNCTION DATA IN .RMD
dataVisulization <- function(dataInput) {
## Run datapoints of interest
sum(opportunityYouth$PWGTP)

race <- count(opportunityYouth, race, wt=PWGTP)
race <- na.omit(race)
race$percent <- scales::percent(race$n/sum(race$n))
race$label <- paste(race$race, "\n", race$percent)  

disability <- count(opportunityYouth, disability, wt=PWGTP)
disability <- na.omit(disability)
disability$percent <- scales::percent(disability$n/sum(disability$n))
disability$label <- paste(disability$disability, "\n", disability$percent)  

sex <- count(opportunityYouth, sex, wt=PWGTP)
sex <- na.omit(sex)
sex$percent <- scales::percent(sex$n/sum(sex$n))
sex$label <- paste(sex$sex, "\n", sex$percent)  

workStatus <- count(opportunityYouth, employmentStatus, wt=PWGTP)
workStatus <- na.omit(workStatus)
workStatus$percent <- scales::percent(workStatus$n/sum(workStatus$n))
workStatus$label <- paste(workStatus$employmentStatus, "\n", workStatus$percent)  

age <- count(opportunityYouth, age, wt=PWGTP)
age <- na.omit(age)
age$percent <- scales::percent(age$n/sum(age$n))
age$label <- paste(age$age, "\n", age$percent)    

lastWorked <- count(opportunityYouth, lastWorked, wt=PWGTP)
lastWorked <- na.omit(lastWorked)
lastWorked$percent <- scales::percent(lastWorked$n/sum(lastWorked$n))
lastWorked$label <- paste(lastWorked$lastWorked, "\n", lastWorked$percent)   

foodStamps <- count(opportunityYouth, foodStamps, wt=PWGTP)
foodStamps <- na.omit(foodStamps)
foodStamps$percent <- scales::percent(foodStamps$n/sum(foodStamps$n))
foodStamps$label <- paste(foodStamps$foodStamps, "\n", foodStamps$percent)  

socialSecurity <- count(opportunityYouth, socialSecurity, wt=PWGTP)
socialSecurity <- na.omit(socialSecurity)
socialSecurity$percent <- scales::percent(socialSecurity$n/sum(socialSecurity$n))
socialSecurity$label <- paste(socialSecurity$socialSecurity, "\n", socialSecurity$percent)  

supplementarySecurityIncome <- count(opportunityYouth, supplementarySecurityIncome, wt=PWGTP)
supplementarySecurityIncome <- na.omit(supplementarySecurityIncome)
supplementarySecurityIncome$percent <- scales::percent(supplementarySecurityIncome$n/sum(supplementarySecurityIncome$n))
supplementarySecurityIncome$label <- paste(supplementarySecurityIncome$supplementarySecurityIncome, "\n", supplementarySecurityIncome$percent)  

children <- count(opportunityYouth, children, wt=PWGTP)
children <- na.omit(children)
children$percent <- scales::percent(children$n/sum(children$n))
children$label <- paste(children$children, "\n", children$percent)  
 

childWithin12Months <- count(opportunityYouth, childWithin12Months, wt=PWGTP)
childWithin12Months <- na.omit(childWithin12Months)
childWithin12Months$percent <- scales::percent(childWithin12Months$n/sum(childWithin12Months$n))
childWithin12Months$label <- paste(childWithin12Months$childWithin12Months, "\n", childWithin12Months$percent)  

cognitiveDifficulty <- count(opportunityYouth, cognitiveDifficulty, wt=PWGTP)
cognitiveDifficulty <- na.omit(cognitiveDifficulty)
cognitiveDifficulty$percent <- scales::percent(cognitiveDifficulty$n/sum(cognitiveDifficulty$n))
cognitiveDifficulty$label <- paste(cognitiveDifficulty$cognitiveDifficulty, "\n", cognitiveDifficulty$percent)  

school <- count(opportunityYouth, education, wt=PWGTP)
school <- na.omit(school)
school$percent <- scales::percent(school$n/sum(school$n))
school$label <- paste(school$education, "\n", school$percent)

treemap(race, "label", "n", title = "race")
treemap(disability, "label", "n", title = "disability")
treemap(sex, "label", "n", title = "sex")
treemap(workStatus, "label", "n", title = "workStatus")
treemap(age, "label", "n", title = "age")
treemap(lastWorked, "label", "n", title = "lastWorked")
treemap(foodStamps, "label", "n", title = "foodStamps")
treemap(socialSecurity, "label", "n", title = "socialSecurity")
treemap(supplementarySecurityIncome, "label", "n", title = "supplementarySecurityIncome")
treemap(children, "label", "n", title = "Children")
treemap(childWithin12Months, "label", "n", title = "Child within 12 Months")
treemap(cognitiveDifficulty, "label", "n", title = "Cognitive Difficulty")
treemap(school, "label", "n", title = "Educational Attainment")
}

