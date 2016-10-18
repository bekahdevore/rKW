library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(treemap)
library(googleVis)
library(RColorBrewer)
library(ggthemes)


kentuckyTopOccupationsData <- read.csv("stateRegionalData.csv")

kentuckyTopOccupationsData$Openings.in.Top.Occupations <- str_replace_all(
       kentuckyTopOccupationsData$Openings.in.Top.Occupations, 
       ",","")
kentuckyTopOccupationsData$Openings.in.Top.Occupations <- as.numeric(as.character(kentuckyTopOccupationsData$Openings.in.Top.Occupations))
kentuckyTopOccupationsData$Sector.Occupation <- as.character(kentuckyTopOccupationsData$Sector.Occupation)


###################################################### FUNCTIONS
filterAndPlot <- function(occupationOrSector, region, whiteNumbers) {
       topOccupationsData <- kentuckyTopOccupationsData %>% 
              filter(Type == occupationOrSector) %>%
              filter(Area == region)
       
       outlier <- ifelse(topOccupationsData$Openings.in.Top.Occupations > whiteNumbers, '#D6D8DE', '#36454f')
       
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
                    panel.background = element_rect(fill = 0),
                    legend.position  = "none") 
       
}


######################################################### TREEMAP #########################################################
treemapMaker <- function(occupationSector, region) {
       kentuckyTopOccupationsData$numberLabel <- format(kentuckyTopOccupationsData$Openings.in.Top.Occupations,
                                                        big.mark   = ",", 
                                                        scientific = FALSE)
       
       kentuckyTopOccupationsData$label       <- paste(kentuckyTopOccupationsData$Sector.Occupation, 
                                                       kentuckyTopOccupationsData$numberLabel, 
                                                       sep = "\n")
       
       kentuckyTopOccupationsData <- kentuckyTopOccupationsData %>% 
              filter(Type == occupationSector) %>%
              filter(Area == region)
       
       treemap(kentuckyTopOccupationsData, index = c('label'), vSize = 'Openings.in.Top.Occupations', 
               fontsize.labels = 25,
               border.col = 0,
               title = "",
               fontface.labels = 1,
               fontfamily.labels = "sans")
}



## EAST 
treemapMaker('Total', 'East')
filterAndPlot('Healthcare', 'East', 300)
filterAndPlot('Advanced Manufacturing', 'East', 300)
filterAndPlot('Construction', 'East', 200)
filterAndPlot('Transportation & Logistics', 'East', 80)
filterAndPlot('Business & IT Services', 'East', 50)

## WEST 
treemapMaker('Total', 'West')
filterAndPlot('Healthcare', 'West', 300)
filterAndPlot('Advanced Manufacturing', 'West', 300)
filterAndPlot('Construction', 'West', 200)
filterAndPlot('Transportation & Logistics', 'West', 200)
filterAndPlot('Business & IT Services', 'West', 50)


## SOUTH
treemapMaker('Total', 'South')
filterAndPlot('Healthcare', 'South', 300)
filterAndPlot('Advanced Manufacturing', 'South', 300)
filterAndPlot('Construction', 'South', 200)
filterAndPlot('Transportation & Logistics', 'South', 200)
filterAndPlot('Business & IT Services', 'South', 50)


## CENTRAL
treemapMaker('Total', 'Central')
filterAndPlot('Healthcare', 'Central', 2500)
filterAndPlot('Advanced Manufacturing', 'Central', 5000)
filterAndPlot('Construction', 'Central', 1000)
filterAndPlot('Transportation & Logistics', 'Central', 2000)
filterAndPlot('Business & IT Services', 'Central', 500)





