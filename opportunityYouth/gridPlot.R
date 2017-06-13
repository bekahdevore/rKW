library(dplyr)
library(scales)
library(ggplot2)
library(RColorBrewer)


load("kentuckyAllData.RData")
load("louisvilleMsaAllData.RData")
load("peerCitiesAllData.RData")

load("shinyOpportunityYouth/kentucky.RData")
load("shinyOpportunityYouth/louisvilleMSA.RData")
load("shinyOpportunityYouth/peerCities.RData")

dataFilterAllYouth <- function(dataInput) {
  opportunityYouth <- dataInput %>%
    filter(AGEP >= 16 & AGEP <= 24)
}

raceCategories <- function(dataHere){
  dataHere %>% mutate(race = ifelse(RAC1P == 1, "White",
                                    ifelse(RAC1P == 2, "Black",
                                           ifelse(RAC1P == 3, "American Indian",
                                                  ifelse(RAC1P == 8, "Some other race alone",
                                                         ifelse(RAC1P == 9, "Two or more races", "Other"))))))
}

raceGroups <- as.data.frame(unique(peerCities$race))
raceGroups$color <- c("#ffb732", "#ffffff", "#e8e8e8", "#cccccc", "#c0c0c0", "#e8e8e8")
colnames(raceGroups)[1] <- "race"


cleanData <- function(allDataHere, oyDataHere) {
  allYouth <- raceCategories(dataFilterAllYouth(allDataHere))
  
  oy <- count(oyDataHere, race, wt = PWGTP)
  allYouth <- count(allYouth, race, wt = PWGTP)
  
  colnames(oy)[2] <- "oy"
  colnames(allYouth)[2] <- "all"
  
  allData <- left_join(allYouth, oy, by = "race")
  
  allData <- allData %>% mutate(percentOY = oy/all) %>% filter(race != "Other" & race != "American Indian") %>%
    mutate(label = percent(percentOY))
}
  
kentuckyData <- cleanData(kentuckyAllData, kentucky) %>% mutate(place = "Kentucky")
louisvilleData <- cleanData(louisvilleMsaAllData, louisvilleMSA) %>% mutate(place = "Louisville")
peerCityData <- cleanData(peerCitiesAllData, peerCities) %>% mutate(place = "Peer Cities")

allData <- rbind(
  kentuckyData, 
  louisvilleData, 
  peerCityData
)

## Add colors
allData <- left_join(allData, raceGroups, by = "race") 
  
allData$race <- factor(allData$race, levels = unique(allData$race)[order(allData$percentOY, decreasing = FALSE)])
  
  
  g <- ggplot(allData, aes(race, percentOY, fill = race, label = label)) %>% 
    facet_wrap(race ~ place)
  
  g + geom_bar(stat = "identity") +
    coord_flip() + labs(title = titleHere, subtitle = "PUMS 2015 1yr", y = "Percent Opportunity Youth", x = "") +
    geom_text(position = position_stack(vjust = 0.5)) + 
    guides(fill = FALSE) + 
    scale_y_continuous(limits = c(0,.2), labels = percent) + 
    scale_fill_manual(values=setNames(allData$color, allData$race))+
    theme_minimal()


dataFilterAndVisualization(peerCitiesAllData, peerCities, "Peer Cities")
dataFilterAndVisualization(kentuckyAllData, kentucky, "Kentucky")
dataFilterAndVisualization(louisvilleMsaAllData, louisvilleMSA, "Louisville MSA")

