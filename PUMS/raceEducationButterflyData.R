library(dplyr)
library(cwhmisc)
library(tidyr)
library(plyr)
library(stringr)
library(RCurl)
library(googlesheets)
library(matrixStats)

pumaConnection   <- getURL('https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv')

pumas            <- read.csv(textConnection(pumaConnection), check.names = FALSE)
indianaPUMS      <- read.csv('ss15pin.csv')
kentuckyPUMS     <- read.csv('ss15pky.csv')

inPUMS      <- indianaPUMS %>%
                      filter(indianaPUMS$PUMA  %in% pumas$inPUMA)

kyPUMS     <- kentuckyPUMS %>%
                      filter(kentuckyPUMS$PUMA %in% pumas$kyPUMA)

louMsaData       <- rbind(inPUMS, kyPUMS)
#rm(kyPUMS, inPUMS)

kyInData <- rbind(indianaPUMS, kentuckyPUMS)


unemploymentWageFunction <- function(pums2015){
pums2015         <- pums2015 %>%
                    select(PWGTP, AGEP, RAC1P, HISP, SCHL, PERNP, ESR)

variables        <- c('PWGTP', 'AGEP', 'RAC1P', 'HISP', 'SCHL', 'PERNP', 'ESR')
                        
pums2015[,variables] <- lapply(pums2015[,variables] , as.character)
pums2015[,variables] <- lapply(pums2015[,variables] , as.numeric)

rm(variables)

pums2015 <- pums2015 %>%
              filter(AGEP >= 25 & AGEP <= 64)


pums2015 <- pums2015 %>%
              filter(ESR != 4 | ESR != 5) %>%
              mutate(Employment = ifelse(ESR == 3,  "Unemployed", 
                                  ifelse(ESR == 6,  "Not in Labor Force", 
                                  "Employed")))

# Assign race categories
pums2015        <- pums2015 %>%
                     mutate(Race = ifelse(RAC1P == 1 & HISP == 1, "White", 
                                   ifelse(RAC1P == 2 & HISP == 1, "Black or African American",
                                   ifelse(HISP != 1, "Hispanic", "Other"))))

# Assign educational attainment catagories
lessThanHighSchool <- 1:15
highSchool         <- 15:16
someCollege        <- 18:19
doctoralProf       <- 23:24

pums2015           <- pums2015 %>%
                        mutate(Education = ifelse(SCHL %in% lessThanHighSchool,  "Less than high school", 
                                           ifelse(SCHL %in% highSchool,          "High School Diploma",
                                           ifelse(SCHL %in% someCollege,         "Some College, no degree",
                                           ifelse(SCHL ==   20,                  "Associate's degree",
                                           ifelse(SCHL ==   21,                  "Bachelor's degree", 
                                           ifelse(SCHL ==   22,                  "Master's degree", 
                                           ifelse(SCHL %in% doctoralProf,        "Doctoral or Professional Degree", 
                                           "Other"))))))))

pums2015$edRace <- paste(pums2015$Education, pums2015$Race)

pums2015weightedMedian <- pums2015 %>%
                           filter(PERNP != 0)              
        
weightedMedian <- ddply(pums2015weightedMedian,
                      .(edRace), 
                       summarise, 
                       wMedian=weightedMedian(PERNP, PWGTP))


pums2015 <- left_join(pums2015, weightedMedian, by = 'edRace')

pums2015$edRaceEmpl <- paste(pums2015$Education, pums2015$Race, pums2015$Employment)

#Calculate unemployment rate by group
#Add column with unemployment count by edRace
unemployment <- pums2015 %>%
                filter(Employment == "Unemployed")
                unemployed <- dplyr::count(unemployment, edRace, wt = PWGTP)
                colnames(unemployed)[2] <- "unemployed"


totalLaborForce        <- pums2015 %>%
                            filter(Employment != "Not in Labor Force")

laborForce             <- dplyr::count(totalLaborForce, edRace, wt = PWGTP)
                          colnames(laborForce)[2] <- "laborForce"

#Add column with calculation unemployment/total labor force by edRace 
unemploymentRate <- full_join(laborForce, unemployed, by = "edRace")

unemploymentRate[is.na(unemploymentRate)] <- 0
unemploymentRate$rate <- unemploymentRate$unemployed/unemploymentRate$laborForce

pums2015 <- left_join(pums2015, unemploymentRate, by = 'edRace')
                colnames(pums2015)[16] <- 'Unemployment Rate'
                colnames(pums2015)[13] <- 'Education, Race, and Employment'
                colnames(pums2015)[11] <- 'Education and Race'

pums2015 <- pums2015 %>%
              select(8:11, 13, 12, 16) %>%
              distinct(pums2015$"Education and Race", .keep_all = TRUE) %>%
              select(2:4, 6:7)
              
print(pums2015)
}

louMsaUnemploymentWages <- unemploymentWageFunction(louMsaData) 
kyUnemploymentWages     <- unemploymentWageFunction(kentuckyPUMS)
kyInUnemploymentWages   <- unemploymentWageFunction(kyInData)

#OUTPUT FOR LOUISVILLE MSA
educationWagesButterfly <- gs_title('pums2015')
educationWagesButterfly <- educationWagesButterfly %>% 
                            gs_edit_cells(input  = louMsaUnemploymentWages,
                                      ws = 1,
                                      anchor = "A1", 
                                      byrow  = TRUE)

#OUTPUT FOR KY
educationWagesButterfly <- educationWagesButterfly %>% 
                            gs_edit_cells(input  = kyUnemploymentWages,
                                          ws = 2,
                                          anchor = "A1", 
                                          byrow  = TRUE)
#OUTPUT FOR KY, IN, TN
educationWagesButterfly <- educationWagesButterfly %>% 
                            gs_edit_cells(input  = kyInUnemploymentWages,
                                          ws = 3,
                                          anchor = "A1", 
                                          byrow  = TRUE)
#OUTPUT FOR Lou, Ind, Nash
educationWagesButterfly <- educationWagesButterfly %>% 
                            gs_edit_cells(input  = unemploymentRate,
                                          ws = 4,
                                          anchor = "A1", 
                                          byrow  = TRUE)





#write.csv(pums2015, file = 'educationWagesButterfly.csv')
