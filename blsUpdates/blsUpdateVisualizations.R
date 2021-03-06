#load packages
library(stringr)
library(ggplot2)

#import data 
laborForceData         <- read.csv("laborForceJuly.csv")
unemploymentRateData   <- read.csv("unemploymentRateJuly.csv")

#Function to remove unecessary characters: (R), (D). and (P)
replaceCharacters  <- function(dataName) {
                            as.data.frame(lapply(dataName, function(x) {
                            gsub("\\(R)|\\(D)|\\(P)", "", x)
                            }))
                     }

#Remove characters from datasets
laborForceData       <- replaceCharacters(laborForceData)
unemploymentRateData <- replaceCharacters(unemploymentRateData)

laborForceData$laborForce       <- as.numeric(as.character(laborForceData$laborForce))
unemploymentRateData$unemploymentRate <- as.numeric(as.character(unemploymentRateData$unemploymentRate)) 

#plot on line graph
ggplot(laborForceData, aes(x=year, y=laborForce, group=area, color=area)) +
       geom_line(size=1.2) +
       geom_point(size=3)  +
       xlab("Year")        +
       ylab("Labor Force") +
       labs(title = "Size of Labor Force, July 2006-2016") +
       theme(plot.title = element_text(size = rel(2.5)), 
             axis.title.y = element_text(size = rel(2)), 
             axis.title.x = element_text(size = rel(2)),
             axis.text.y  = element_text(size = 18),
             axis.text.x  = element_text(size = 18, angle = 45),
             axis.ticks   = element_blank(), 
             legend.position = c(.8, .63), 
             legend.background = element_blank(), 
             #legend.background = element_rect(color = "black"),
             legend.text  = element_text(size = 18), 
             legend.title = element_blank()
       )

       
ggplot(unemploymentRateData, aes(x=year, y=unemploymentRate, group=area, color=area)) +
       geom_line(size=1.2)                              +
       geom_point(size=3)                               +
       xlab("Year")                                     +
       ylab("Unemployment Rate")                        +
       labs(title="Unemployment Rates, July 2006-2016") +
       theme(plot.title = element_text(size = rel(2.5)), 
              axis.title.y = element_text(size = rel(2)), 
              axis.title.x = element_text(size = rel(2)),
              axis.text.y  = element_text(size = 18),
              axis.text.x  = element_text(size = 18, angle = 45),
              axis.ticks   = element_blank(), 
              legend.position   = c(.85, .75), 
              legend.background = element_blank(), 
              #legend.background = element_rect(color = "black"),
              legend.text  = element_text(size = 18), 
              legend.title = element_blank()
             )

