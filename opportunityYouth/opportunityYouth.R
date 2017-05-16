library(dplyr)
library(RMySQL)
library(RCurl)

#  MySQL database connection
con <- dbConnect(MySQL(), group = "kwlmi", dbname = "kwlmi")
pumasDataConnection <- getURL('https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv')

louisvillePumas <- read.csv(textConnection(pumasDataConnection))
kyPUMAS <- louisvillePumas$kyPUMA
inPUMAS <- louisvillePumas$inPUMA

# Pull data from MySQL Database
kentuckyData <- dbGetQuery(conn = con, statement = "SELECT AGEP,SCHL,SCH,ESR,RAC1P,SEX,PUMA,PWGTP FROM ss15pky;")
indianaData  <- dbGetQuery(conn = con, statement = "SELECT AGEP,SCHL,SCH,ESR,RAC1P,SEX,PUMA,PWGTP FROM ss15pin;")

kentuckyData <- kentuckyData %>% filter(PUMA %in% louisvillePumas$kyPUMA) 
indianaData  <- indianaData  %>% filter(PUMA %in% louisvillePumas$inPUMA)

allData <- rbind(kentuckyData, indianaData)
rm(kentuckyData, indianaData, louisvillePumas, inPUMAS, kyPUMAS, pumasDataConnection, con)

opportunityYouth <- allData %>% 
                      filter(ESR == 3 | ESR == 6) %>% 
                      filter(SCH == 1) %>%
                      filter(AGEP >= 16 & AGEP <= 24)

sum(opportunityYouth$PWGTP)
