library(dplyr)
library(RCurl)
library(stringr)
library(googlesheets)

#Adjust living wage here
mitLivingWage <- 24.02

save(mitLivingWage, file = "mitLivingWage.RData")
# found here: http://livingwage.mit.edu/metros/31140

#Import Data from google sheets 
burningGlassQuarterConnection   <- getURL('https://docs.google.com/spreadsheets/d/1gk0cOCFlAfZCN_Yh86qB0ob7N7PJVzXIRWsUVl81Lks/pub?gid=0&single=true&output=csv') #updated    
burningGlassAnnualConnection    <- getURL('https://docs.google.com/spreadsheets/d/1gk0cOCFlAfZCN_Yh86qB0ob7N7PJVzXIRWsUVl81Lks/pub?gid=919296133&single=true&output=csv') #updated
emsiDataConnection              <- getURL('https://docs.google.com/spreadsheets/d/1gk0cOCFlAfZCN_Yh86qB0ob7N7PJVzXIRWsUVl81Lks/pub?gid=396778125&single=true&output=csv') #updated
socCrosswalkConnection          <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1551915918&single=true&output=csv')

burningGlassQuarter             <- read.csv(textConnection(burningGlassQuarterConnection), check.names = FALSE)
burningGlassAnnual              <- read.csv(textConnection(burningGlassAnnualConnection),  check.names = FALSE)
emsiData                        <- read.csv(textConnection(emsiDataConnection),            check.names = FALSE)
socCrosswalk                    <- read.csv(textConnection(socCrosswalkConnection),        check.names = FALSE)

# MAIN DATA FILE 
mainDataFile <- full_join(burningGlassQuarter, emsiData, by = 'SOC')
colnames(mainDataFile)[3] <- 'Job Postings'
mainDataFile <- full_join(mainDataFile, socCrosswalk, by = 'SOC')
mainDataFile <- mainDataFile %>% select(1, 12, 3, 5:11)

variables    <- c('Job Postings',
                  '2016 - 2026 Change', 
                  'Age 55-64', 'Age 65+', 
                  'Median Hourly Earnings')

## REMOVE COMMAS, DOLLAR SIGNS, REPLACE <10 w/ 5, and Insf. Data w/ 0 
mainDataFile[,variables] <- as.data.frame(lapply(mainDataFile[,variables], function(x){gsub(',', '',    x)}))
mainDataFile[,variables] <- as.data.frame(lapply(mainDataFile[,variables], function(x){gsub('<10', '5', x)}))
mainDataFile[,variables] <- as.data.frame(lapply(mainDataFile[,variables], function(x){gsub('\\$', '',  x)}))
mainDataFile[,variables] <- as.data.frame(lapply(mainDataFile[,variables], function(x){gsub('Insf. Data', '0', x)}))

## CONVERT TO CHARACTER THEN NUMERIC
mainDataFile[,variables] <- lapply(mainDataFile[,variables] , as.character)
mainDataFile[,variables] <- lapply(mainDataFile[,variables] , as.numeric)

# TOP JOB POSTINGS DATA 
topJobPostings  <- mainDataFile %>% select(1:3, 5:6, 9:10)
topJobPostings$deduplicated <- topJobPostings$`Job Postings` * 0.8

# HIGH DEMAND DATA 
highDemandData <- full_join(mainDataFile, burningGlassAnnual, by = 'SOC')

rm(socCrosswalkConnection, 
   emsiDataConnection,
   burningGlassAnnualConnection, 
   burningGlassQuarterConnection, 
   burningGlassQuarter, 
   burningGlassAnnual,
   socCrosswalk,
   emsiData)

highDemandData <- highDemandData %>% select(1:2, 12, 4:5, 7:8)

## REMOVE COMMAS
variables <- c('Number of Job Postings')

highDemandData$`Number of Job Postings` <- str_replace_all(highDemandData$`Number of Job Postings`, ",", "")
highDemandData$`Number of Job Postings` <- as.numeric(as.character(highDemandData$`Number of Job Postings`))

#Filter median wage to above living wage
highDemandData <- highDemandData %>% filter(highDemandData$`Median Hourly Earnings` >= mitLivingWage)
#colnames(highDemandData)[3] <- 'Job Postings'

## ADD RETIREMENTS 
highDemandData$retirements  <- highDemandData$`Age 55-64` + highDemandData$`Age 65+`
highDemandData$growthRetire <- highDemandData$`2016 - 2026 Change` + highDemandData$retirements

## ADD RANKINGS
highDemandData              <- highDemandData %>% 
  mutate(postingsRank = dense_rank(desc(highDemandData$`Number of Job Postings`))) 
highDemandData              <- highDemandData %>% 
  mutate(growthRank = dense_rank(desc(highDemandData$growthRetire))) 

highDemandData$indexRank    <- (highDemandData$postingsRank * highDemandData$growthRank)/2
highDemandData$deduplicated <- round((highDemandData$`Number of Job Postings`*.8), 0)

## SELECT NECESSARY VARIABLES 
highDemandData              <- highDemandData %>% select(1:2, 13, 4, 8:9, 12)


## .CSV OUTPUT
write.csv(highDemandData, file = 'highDemandData.csv')
write.csv(topJobPostings, file = 'topJobPostings.csv')



## Connect to Google Sheet
#quarterlyReport <- gs_title('quarterlyReportRscriptOutput')

## JOB POSTINGS OUTPUT 
#quarterlyReport <- quarterlyReport %>% 
#                      gs_edit_cells(input  = topJobPostings,
#                                    ws     = 1,
#                                    anchor = "A1", 
#                                    byrow  = TRUE)


## HIGH DEMAND OUTPUT
#quarterlyReport <- quarterlyReport %>% 
#                      gs_edit_cells(input  = highDemandData,
#                                    ws = 3,
#                                    anchor = "A1", 
#                                    byrow = TRUE)


