library(RCurl)
library(dplyr)
library(plotrix)
library(scales)
library(plotly)
library(ggplot2)
library(googleVis)

## ADD DATA
indiana  <- read.csv("ss15pin.csv")
kentucky <- read.csv("ss15pky.csv")
ohio <- read.csv("ss15poh.csv")
tennessee <- read.csv("ss15ptn.csv")

# add PUMA list for filtering
occupationGroupsConnection <- getURL('https://docs.google.com/spreadsheets/d/1n2WLOqZjzUwJHmx8s3ILh0L0o54CjUaJT6_Yk4rcNio/pub?gid=0&single=true&output=csv')
dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv")
pumas          <- read.csv(textConnection(dataConnection))
occupationGroups <- read.csv(textConnection(occupationGroupsConnection))

rm(dataConnection, occupationGroupsConnection)


## FUNCTIONS
countWeight <- function(enterData){
  enterData %>% count(occGroup, wt = PWGTP)
}

percentFunction <- function(enterData){
  enterData$percent <- enterData %>% dplyr::mutate(percent = ((enterData$n)/sum(enterData$n)))
}

addColumnsDataPoint <- function(enterData, dataPoint) {
  enterData <- enterData %>% 
    dplyr::mutate(DataPoint = dataPoint)  
}

addColumns <- function(enterData, race, education) {
  enterData <- enterData %>% 
    dplyr::mutate(race = race) %>% 
    dplyr::mutate(education = education)
}

weightPercent <- function(enterData, dataPoint) {
  newData <- percentFunction(countWeight(enterData))
  newData <- addColumnsDataPoint(newData, dataPoint)
  # colnames(newData)[1] <- "Type"
  newData
}



# merge ky and in puma files 
allData <- rbind(indiana, kentucky, ohio, tennessee)
rm(indiana, kentucky, ohio, tennessee)

## Connect occupation groups to main data file 
allData <- left_join(allData, occupationGroups, by = "OCCP")

allData <- allData %>% mutate(race = ifelse(RAC1P == 1, "White", 
                                            ifelse(RAC1P == 2, "Black", "Other")))
allData <- allData %>% filter(race != "Other") %>% select(race, PWGTP, occGroup, SCHL)


#seperate into groups by race and education 
bachelors <- allData %>% filter(SCHL == 21)
masters   <- allData %>% filter(SCHL == 22)

masters <- masters %>% select(race, occGroup, PWGTP)
bachelors <- bachelors %>% select(race, occGroup, PWGTP)

blackBachelors <- bachelors %>% filter(race == "Black")
whiteBachelors <- bachelors %>% filter(race == "White")

blackMasters   <- masters %>% filter(race == "Black")
whiteMasters   <- masters %>% filter(race == "White")

blackMasters <- weightPercent(blackMasters, "Black")
whiteMasters <- weightPercent(whiteMasters, "White")
blackBachelors <- weightPercent(blackBachelors, "Black")
whiteBachelors <- weightPercent(whiteBachelors, "White")


mastersFinal <- as.data.frame(na.omit(rbind(blackMasters, whiteMasters)) %>% select(DataPoint, occGroup, percent))
mastersFinal$percent <- mastersFinal$percent* 100

bachelorsFinal <- as.data.frame(na.omit(rbind(blackBachelors, whiteBachelors)) %>% select(DataPoint, occGroup, percent))
bachelorsFinal$percent <- bachelorsFinal$percent* 100
# mastersFinal$DataPoint <- as.character(mastersFinal$DataPoint)
# mastersFinal$occGroup <- as.character(mastersFinal$occGroup)

write.csv(bachelorsFinal, "bachelorsFinalFourState.csv")
write.csv(mastersFinal, "mastersFinalFourState.csv")


colors_link <- c('#9C0059', '#A4D7F4')
colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

colors_node <- c('grey', 'grey')
colors_node_array <- paste0("[", paste0("'", colors_node,"'", collapse = ','), "]")

opts <- paste0("{
               link: { colorMode: 'source',
               colors: ", colors_link_array ," },
               node: { colors: ", colors_node_array ," }
               }" )


educationRaceSankey <- plot(gvisSankey(
  mastersFinal, 
  from= "occGroup",
  to="DataPoint",
  weight = "percent",
  options=list(height =1000,
               width = 1000, 
               sankey = opts
               # sankey = "{link:{
               # color:{fill:'black'}
               # }}"
  )
))
