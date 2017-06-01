library(RCurl)
library(dplyr)
library(plotly)
library(ggplot2)
library(scales)


## Load and read data
load("allData.RData") ## BLS data for all metros

msaCodesConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
rppConnection <- getURL("https://docs.google.com/spreadsheets/d/101D20GoMgT50II1U_fzS0Cc4uwrjxDvdoovbbi74gu0/pub?gid=0&single=true&output=csv")

msaCodes <- read.csv(textConnection(msaCodesConnection))
rpp <- read.csv(textConnection(rppConnection))


## Filter to all occupations and annual median and mean wage
allData <- allData %>% filter(datatype_code == 13 | datatype_code == 4 & occupation_code == 0) 

## Filter to area
msaData <- allData %>% filter(area_code %in% msaCodes$area_code) 
kyData <- allData %>% filter(area_code == 21)


## join with RPP cost of living index
msaDataRPP <- left_join(msaData, rpp, by = "area_code")
msaDataRPP <- msaDataRPP %>% filter(LineCode == 1)

changeFormNumeric <- function(dataHere){as.numeric(as.character(dataHere))}

msaDataRPP$value<- changeFormNumeric(msaDataRPP$value)
msaDataRPP$X2014 <- changeFormNumeric(msaDataRPP$X2014)

## ADJUST DATA 
msaDataRPP <- msaDataRPP %>% mutate(medianWage = value/(X2014/100)) 
dataToVisualize <- left_join(msaDataRPP, msaCodes, by = "area_code")
dataToVisualize <- dataToVisualize %>% select(MSA, medianWage)
dataToVisualize$wage <- dollar(round(dataToVisualize$medianWage))
dataToVisualize$MSA <- factor(dataToVisualize$MSA, levels = unique(dataToVisualize$MSA)[order(dataToVisualize$medianWage, decreasing = FALSE)])
dataToVisualize <- dataToVisualize %>% mutate(group = ifelse(MSA == "Louisville", "Louisville", "Other"))


g <- ggplot(dataToVisualize, aes(MSA, medianWage, fill = group, label = wage))
# Number of cars in each class:
g + geom_bar(stat = "identity") + 
  coord_flip() + labs(title = "Louisville and Peer Cities", subtitle = "Median Wage Adjusted for Cost of Living", y = "Median Wage") +
  geom_text(position = position_stack(vjust = 0.9)) + guides(fill = FALSE) + scale_y_continuous(labels = dollar)
 