#Load packages
library(dplyr)
library(RCurl)
library(stringr)
library(googlesheets)

#Load data
burningGlassQuarterConnection   <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=0&single=true&output=csv')
socCrosswalkConnection          <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1551915918&single=true&output=csv')
emsiDataConnection              <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1224165436&single=true&output=csv') 
soc                             <- getURL('https://docs.google.com/spreadsheets/d/1wWVpXkU7OG2dGjCEEOK4Z4sS02tgK9_zee9cl0MdQRE/pub?gid=0&single=true&output=csv')

burningGlassQuarter             <- read.csv(textConnection(burningGlassQuarterConnection), check.names = FALSE)
emsiData                        <- read.csv(textConnection(emsiDataConnection),            check.names = FALSE)
socCrosswalk                    <- read.csv(textConnection(socCrosswalkConnection),        check.names = FALSE)
majorSocCodeNames               <- read.csv(textConnection(soc))

#Merge Data
mainDataFile                    <- full_join(burningGlassQuarter, emsiData, by = 'SOC')
mainDataFile                    <- full_join(mainDataFile, socCrosswalk, by = 'SOC')

#Only keep necessary variables
mainDataFile                    <- mainDataFile %>%
                                      select(1, 12, 3, 10:11, 7)

#Split 1st 2 digits of SOC code and store in new data frame
splitSOC                        <- as.data.frame(t(sapply(mainDataFile$SOC, 
                                                          function(x) substring(x, 
                                                                                first=c(1, 1), 
                                                                                last=c(2, 7)))))

#Change column names 
colnames(splitSOC)[1]           <- "socGroup"
colnames(splitSOC)[2]           <- "SOC"
colnames(majorSocCodeNames)[1]  <- 'socGroup'
colnames(majorSocCodeNames)[2]  <- 'Occupation Group'

#Merge data with major soc group codes data frame
mainDataFile                    <- full_join(mainDataFile, splitSOC, by = 'SOC')
#Change socGroup variable to factor 
majorSocCodeNames$socGroup      <- as.factor(majorSocCodeNames$socGroup)
mainDataFile$socGroup           <- as.factor(mainDataFile$socGroup)
#Join data to add major soc group names
mainDataFile                    <- full_join(mainDataFile, majorSocCodeNames, by = 'socGroup')
                
  

#Explore
itJobNamesBachelors <-as.vector(mainDataFile %>%
                                  filter(Sector == "IT" & mainDataFile$`Typical Entry Level Education` == "Bachelor's degree") %>%
                                  select(2))

itJobNamesAssociates <-as.vector(mainDataFile %>%
                                   filter(Sector == "IT" & mainDataFile$`Typical Entry Level Education` == "Associate's degree") %>%
                                   select(2))

itJobNamesByEducationLevel <-as.vector(mainDataFile %>%
                                   filter(Sector == "IT") %>%
                                   select(1, 2, 6, 3))
              
