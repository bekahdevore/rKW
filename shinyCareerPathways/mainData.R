library(dplyr)
library(shiny)
library(RCurl)
library(stringr)
library(googlesheets)
library(shinythemes)

#Load data
burningGlassQuarterConnection   <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=0&single=true&output=csv')
emsiDataConnection              <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1224165436&single=true&output=csv') 
sectorsConnection               <- getURL('https://docs.google.com/spreadsheets/d/1rL0sCtUSzBbhlZYSGvUgx3fXip55o2OpMWUMK_6TKaA/pub?gid=487558132&single=true&output=csv')
socCrosswalkConnection          <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1551915918&single=true&output=csv')
#socConnection                   <- getURL('https://docs.google.com/spreadsheets/d/1wWVpXkU7OG2dGjCEEOK4Z4sS02tgK9_zee9cl0MdQRE/pub?gid=0&single=true&output=csv')

burningGlassQuarter             <- read.csv(textConnection(burningGlassQuarterConnection), check.names = FALSE)
emsiData                        <- read.csv(textConnection(emsiDataConnection),            check.names = FALSE)
sectors                         <- read.csv(textConnection(sectorsConnection))
socCrosswalk                    <- read.csv(textConnection(socCrosswalkConnection),        check.names = FALSE)
#majorSocCodeNames               <- read.csv(textConnection(socConnection))

rm(burningGlassQuarterConnection, 
   emsiDataConnection,
   sectorsConnection,
   socCrosswalkConnection)

#Merge Data
mainDataFile                    <- full_join(burningGlassQuarter, emsiData, by = 'SOC')
mainDataFile                    <- full_join(mainDataFile, socCrosswalk, by = 'SOC')
mainDataFile                    <- left_join(mainDataFile, sectors, by = "SOC")

rm(burningGlassQuarter,
   emsiData, 
   sectors,
   socCrosswalk)

#Select necessary variables
mainDataFile                    <- mainDataFile %>%
  select(1, 12, 3, 10:11, 7, 13:14)

mainDataFile                    <- as.data.frame(lapply(mainDataFile, function(x) {
  gsub(',', '', x) }))

mainDataFile                    <- as.data.frame(lapply(mainDataFile, function(x) {
  gsub('\\$', '', x )}))

variables <- c('Number.of.Job.Postings' ,
               'Pct..25.Hourly.Earnings',
               'Pct..75.Hourly.Earnings')

mainDataFile[,variables] <- lapply(mainDataFile[,variables] , as.character)
mainDataFile[,variables] <- lapply(mainDataFile[,variables] , as.numeric)

mainDataFile$deduplicatedPostings <- (mainDataFile$Number.of.Job.Postings)* .8
mainDataFile$deduplicatedPostings <- round(mainDataFile$deduplicatedPostings, digits = 0)