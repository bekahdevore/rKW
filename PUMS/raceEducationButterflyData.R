library(dplyr)
library(cwhmisc)
library(tidyr)
library(plyr)
library(stringr)

pumaConnection   <- getURL('https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv')

pumas            <- read.csv(textConnection(pumaConnection), check.names = FALSE)
indianaPUMS      <- read.csv('ss15pin.csv')
kentuckyPUMS     <- read.csv('ss15pky.csv')

indianaPUMS      <- indianaPUMS %>%
                      filter(indianaPUMS$PUMA  %in% pumas$inPUMA)

kentuckyPUMS     <- kentuckyPUMS %>%
                      filter(kentuckyPUMS$PUMA %in% pumas$kyPUMA)

pums2015         <- rbind(indianaPUMS, kentuckyPUMS)

pums2015         <- pums2015 %>%
                    select(PWGTP, AGEP, RAC1P, HISP, SCHL, PERNP, ESR)

variables        <- c('PWGTP', 'AGEP', 'RAC1P', 'HISP', 'SCHL', 'PERNP', 'ESR')
                        
pums2015[,variables] <- lapply(pums2015[,variables] , as.character)
pums2015[,variables] <- lapply(pums2015[,variables] , as.numeric)

rm(indianaPUMS,
   kentuckyPUMS, 
   pumaConnection, 
   pumas, 
   variables)

pums2015 <- pums2015 %>%
              filter(PERNP > 0) %>%
              filter(ESR != "b") %>%
              filter(AGEP >= 16 & AGEP <= 64)


pums2015 <- pums2015 %>%
              mutate(Employment = ifelse(ESR == 3,  "Unemployed", 
                                  ifelse(ESR == 6,  "Not in Labor Force", 
                                  "Employed")))

# Assign race categories
pums2015        <- pums2015 %>%
                     mutate(Race = ifelse(RAC1P == 1 & HISP == 1, "White", 
                                   ifelse(RAC1P == 2 & HISP == 1, "Black or African American",
                                   ifelse(HISP != 1, "Latina/o", "Other"))))

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


weightedMedian <- ddply(pums2015,
                      .(edRace), 
                       summarise, 
                       wMedian=w.median(PERNP, PWGTP))


pums2015 <- left_join(pums2015, weightedMedian, by = 'edRace')

pums2015$edRaceEmpl <- paste(pums2015$Education, pums2015$Race, pums2015$Employment)

#Calculate unemployment rate by group
#Add column with unemployment count by edRace
unemployment <- pums2015 %>%
                filter(Employment == "Unemployed")
unemployed <- count(unemployment, 'edRace', wt = "PWGTP")
colnames(unemployed)[2] <- "unemployed"


totalLaborForce        <- pums2015 %>%
                            filter(Employment != "Not in Labor Force")
laborForce             <- count(totalLaborForce, 'edRace', wt = "PWGTP")
colnames(laborForce)[2] <- "laborForce"

#Add column with calculation unemployment/total labor force by edRace 
unemploymentRate <- full_join(laborForce, unemployed, by = "edRace")

unemploymentRate[is.na(unemploymentRate)] <- 0
unemploymentRate$rate <- unemploymentRate$unemployed/unemploymentRate$laborForce

pums2015 <- left_join(pums2015, unemploymentRate, by = 'edRace')
colnames(pums2015)[15] <- 'Unemployment Rate'
colnames(pums2015)[11] <- 'Education and Race'

pums2015 <- pums2015 %>%
              select(8:12, 15)

write.csv(pums2015, file = 'educationRaceUnemploymentMedianWage.csv')

educationRaceUnemploymentMedianWage <- gs_title('pums2015louisvilleMSA')
educationRaceUnemploymentMedianWage <- educationRaceUnemploymentMedianWage %>% 
                                               gs_edit_cells(input  = pums2015,
                                                             anchor = "A1", 
                                                             byrow  = TRUE)
