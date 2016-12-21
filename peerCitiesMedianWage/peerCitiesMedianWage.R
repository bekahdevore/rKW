library(RCurl)
library(dplyr)
library(googlesheets)
library(stringr)

msaConnection <- getURL('https://docs.google.com/spreadsheets/d/1MeUsZ_VyIwIg1ctOzVmwBDss-XqpYkW1xnOIC1dmvcA/pub?gid=0&single=true&output=csv')
msaList       <- read.csv(textConnection(msaConnection), check.names = FALSE)

## Read data
msa2005_01 <- read.csv('msa2005_1.csv')
msa2005_02 <- read.csv('msa2005_2.csv')
msa2005_03 <- read.csv('msa2005_3.csv')

msa2006_01 <- read.csv('msa2006_1.csv')
msa2006_02 <- read.csv('msa2006_2.csv')
msa2006_03 <- read.csv('msa2006_3.csv')

msa2007_01 <- read.csv('msa2007_1.csv')
msa2007_02 <- read.csv('msa2007_2.csv')
msa2007_03 <- read.csv('msa2007_3.csv')

msa2008_01 <- read.csv('msa2008_1.csv')
msa2008_02 <- read.csv('msa2008_2.csv')
msa2008_03 <- read.csv('msa2008_3.csv')

msa2009_01 <- read.csv('msa2009_1.csv')
msa2009_02 <- read.csv('msa2009_2.csv')
msa2009_03 <- read.csv('msa2009_3.csv')

msa2010_01 <- read.csv('msa2010_1.csv')
msa2010_02 <- read.csv('msa2010_2.csv')
msa2010_03 <- read.csv('msa2010_3.csv')

msa2011_01 <- read.csv('msa2011_1.csv')
msa2011_02 <- read.csv('msa2011_2.csv')
msa2011_03 <- read.csv('msa2011_3.csv')

msa2012_01 <- read.csv('msa2012_1.csv')
msa2012_02 <- read.csv('msa2012_2.csv')
msa2012_03 <- read.csv('msa2012_3.csv')

msa2013_01 <- read.csv('msa2013_1.csv')
msa2013_02 <- read.csv('msa2013_2.csv')
msa2013_03 <- read.csv('msa2013_3.csv')

msa2014 <- read.csv('msa2014.csv')
msa2015 <- read.csv('msa2015.csv')

## Filter
occGroupFilter <- function(dataToEnter){dataToEnter <- dataToEnter %>% filter(OCC_TITLE == 'All Occupations')}
#groupFilter    <- function(dataToEnter){dataToEnter <- dataToEnter %>% filter(GROUP     == 'total')}
msaFilter      <- function(dataToEnter){dataToEnter <- dataToEnter %>% filter(AREA %in% msaList$AREA)}

occMsaFilter <- function(dataToEnter, year){
                  dataToEnter <- occGroupFilter(dataToEnter)
                  dataToEnter <- msaFilter(dataToEnter) 
                  dataToEnter[,'Year'] <- year
                  dataToEnter <- dataToEnter %>% select(AREA, AREA_NAME, A_MEDIAN, Year)
}

# groupMsaFilter <- function(dataToEnter, year){
#                   dataToEnter <- groupFilter(dataToEnter)
#                   dataToEnter <- msaFilter(dataToEnter) 
#                   dataToEnter[,'Year'] <- year
#                   dataToEnter <- dataToEnter %>% select(AREA, AREA_NAME, A_MEDIAN, Year)
# }

msa2015 <- occMsaFilter(msa2015, 2015)
msa2014 <- occMsaFilter(msa2014, 2014)  
msa2013_01 <- occMsaFilter(msa2013_01, 2013)
msa2013_02 <- occMsaFilter(msa2013_02, 2013)
msa2013_03 <- occMsaFilter(msa2013_03, 2013)
msa2012_01 <- occMsaFilter(msa2012_01, 2012)
msa2012_02 <- occMsaFilter(msa2012_02, 2012)
msa2012_03 <- occMsaFilter(msa2012_03, 2012)

msa2011_01 <- occMsaFilter(msa2011_01, 2011)
msa2011_02 <- occMsaFilter(msa2011_02, 2011)
msa2011_03 <- occMsaFilter(msa2011_03, 2011)
msa2010_01 <- occMsaFilter(msa2010_01, 2010)
msa2010_02 <- occMsaFilter(msa2010_02, 2010)
msa2010_03 <- occMsaFilter(msa2010_03, 2010)
msa2009_01 <- occMsaFilter(msa2009_01, 2009)
msa2009_02 <- occMsaFilter(msa2009_02, 2009)
msa2009_03 <- occMsaFilter(msa2009_03, 2009)
msa2008_01 <- occMsaFilter(msa2008_01, 2008)
msa2008_02 <- occMsaFilter(msa2008_02, 2008)
msa2008_03 <- occMsaFilter(msa2008_03, 2008)
msa2007_01 <- occMsaFilter(msa2007_01, 2007)
msa2007_02 <- occMsaFilter(msa2007_02, 2007)
msa2007_03 <- occMsaFilter(msa2007_03, 2007)
msa2006_01 <- occMsaFilter(msa2006_01, 2006)
msa2006_02 <- occMsaFilter(msa2006_02, 2006)
msa2006_03 <- occMsaFilter(msa2006_03, 2006)
msa2005_01 <- occMsaFilter(msa2005_01, 2005)
msa2005_02 <- occMsaFilter(msa2005_02, 2005)
msa2005_03 <- occMsaFilter(msa2005_03, 2005)

allData <- rbind(msa2015, 
                 msa2014, 
                 msa2013_01, 
                 msa2013_02, 
                 msa2013_03, 
                 msa2012_01, 
                 msa2012_02, 
                 msa2012_03, 
                 msa2011_01, 
                 msa2011_02, 
                 msa2011_03, 
                 msa2010_01, 
                 msa2010_02, 
                 msa2010_03, 
                 msa2009_01, 
                 msa2009_02, 
                 msa2009_03, 
                 msa2008_01, 
                 msa2008_02, 
                 msa2008_03, 
                 msa2007_01, 
                 msa2007_02, 
                 msa2007_03, 
                 msa2006_01, 
                 msa2006_02, 
                 msa2006_03,
                 msa2005_01, 
                 msa2005_02, 
                 msa2005_03)
rm(msa2015, 
   msa2014, 
   msa2013_01, 
   msa2013_02, 
   msa2013_03, 
   msa2012_01, 
   msa2012_02, 
   msa2012_03, 
   msa2011_01, 
   msa2011_02, 
   msa2011_03, 
   msa2010_01, 
   msa2010_02, 
   msa2010_03, 
   msa2009_01, 
   msa2009_02, 
   msa2009_03, 
   msa2008_01, 
   msa2008_02, 
   msa2008_03, 
   msa2007_01, 
   msa2007_02, 
   msa2007_03, 
   msa2006_01, 
   msa2006_02, 
   msa2006_03,
   msa2005_01, 
   msa2005_02, 
   msa2005_03)

allData <- left_join(allData, msaList, by = "AREA")
allData <- allData %>% select(1, 5, 3, 4)
colnames(allData)[2] <- 'AREA_NAME'

sheet <- gs_title('Louisville and Peer Cities, Median Wage 2005 - 2015')
sheet <- sheet %>% gs_edit_cells(input = allData, anchor = "A1", byrow = TRUE)
