library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

kentuckyTopOccupationsData <- read.csv("kentuckyStateTopOccupations.csv")

kentuckyTopOccupationsData$Openings.in.Top.Occupations <- as.numeric(as.character(kentuckyTopOccupationsData$Openings.in.Top.Occupations))
kentuckyTopOccupationsData$Openings.in.Top.Occupations <- str_replace_all(
                                                               kentuckyTopOccupationsData$Openings.in.Top.Occupations, 
                                                               ",","")



###################################################### FUNCTIONS
filterAndPlot <- function(occupationOrSector) {
              kentuckyTopOccupationsData <- kentuckyTopOccupationsData %>% 
                                                 filter(Type == occupationOrSector)
       
              ggplot(kentuckyTopOccupationsData, aes(x = reorder(Sector.Occupation, Openings.in.Top.Occupations), 
                                                     y = Openings.in.Top.Occupations)) +
                     geom_bar(stat = 'identity') +
                     coord_flip() +
                     scale_y_continuous(labels = comma)
}



filterAndPlot('Total') 
filterAndPlot('Healthcare')

kentuckyTopOccupationsData <- kentuckyTopOccupationsData %>% 
       filter(Type == 'Total')

ggplot(kentuckyTopOccupationsData, aes(x = Sector.Occupation, y = Openings.in.Top.Occupations)) +
       geom_bar(stat = 'identity') +
       coord_flip()   