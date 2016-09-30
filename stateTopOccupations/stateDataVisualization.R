library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(treemap)
library(googleVis)
library(RColorBrewer)


kentuckyTopOccupationsData <- read.csv("kentuckyStateTopOccupations.csv")

kentuckyTopOccupationsData$Openings.in.Top.Occupations <- str_replace_all(
       kentuckyTopOccupationsData$Openings.in.Top.Occupations, 
       ",","")
kentuckyTopOccupationsData$Openings.in.Top.Occupations <- as.numeric(as.character(kentuckyTopOccupationsData$Openings.in.Top.Occupations))
kentuckyTopOccupationsData$Sector.Occupation <- as.character(kentuckyTopOccupationsData$Sector.Occupation)


###################################################### FUNCTIONS
filterAndPlot <- function(occupationOrSector) {
              topOccupationsData <- kentuckyTopOccupationsData %>% 
                                                 filter(Type == occupationOrSector)
              
              outlier <- ifelse(topOccupationsData$Openings.in.Top.Occupations > 2000, '#D6D8DE', '#36454f')
              
              ggplot(topOccupationsData,     aes(x = reorder(Sector.Occupation, Openings.in.Top.Occupations), 
                                                 y     = Openings.in.Top.Occupations, 
                                                 fill  = Sector.Occupation, 
                                                 label = Openings.in.Top.Occupations)) +
                     geom_bar(stat    = 'identity', 
                              width   = .7)             +
                     scale_fill_hue(c = 45, 
                                    l = 45)             +
                     coord_flip()                       +
                     scale_y_continuous(labels = comma) +
                     ylab("Number of Job Openings")     +
                     labs(title             = occupationOrSector)+
                     geom_text(aes(label    = format(Openings.in.Top.Occupations, 
                                                     big.mark=",", 
                                                     scientific = FALSE)), 
                                             color = outlier, 
                                             size  = 6.8, 
                                             hjust = 'inward', 
                                             vjust = .4)  +
                     theme(plot.title       = element_text(size  = 30, 
                                                           color = '#333333'), 
                           axis.title.y     = element_blank(), 
                           axis.title.x     = element_text(size  = 15, 
                                                           color = '#36454f'),
                           axis.text.y      = element_text(size  = 18,
                                                           color = '#333333'),
                           axis.text.x      = element_text(size  = 17, 
                                                           color = '#333333'),
                           axis.ticks.y     = element_blank(), 
                           #panel.background = element_blank(),
                           legend.position  = "none")
       }

filterAndPlot('Healthcare')
filterAndPlot('Advanced Manufacturing')
filterAndPlot('Construction')
filterAndPlot('Transportation & Logistics')
filterAndPlot('Business & IT Services')

######################################################### TREEMAP #########################################################

kentuckyTopOccupationsData$numberLabel <- format(kentuckyTopOccupationsData$Openings.in.Top.Occupations,
                                                 big.mark   = ",", 
                                                 scientific = FALSE)

kentuckyTopOccupationsData$label       <- paste(kentuckyTopOccupationsData$Sector.Occupation, 
                                                 kentuckyTopOccupationsData$numberLabel, 
                                                 sep = "\n")


treemapMaker <- function(occupationSector) {
       kentuckyTopOccupationsData <- kentuckyTopOccupationsData %>% 
                                          filter(Type == occupationSector) %>%
       
       treemap(kentuckyTopOccupationsData, index = c('label'), vSize = 'Openings.in.Top.Occupations', 
                      fontsize.labels = 25,
                      border.col = 0,
                      title = "",
                      fontface.labels = 1,
                      fontfamily.labels = "sans")
       }



treemapMaker('Total')

