library(ggplot2); library(ggmap); library(RColorBrewer); library(dplyr)

#Load 
BOTH.R <- read.csv("../../rData/rightToWorkData/unemploymentRateByState.csv", header = TRUE)
rightToWorkStates <- read.csv("../../rData/rightToWorkData/rightToWorkStatesData.csv", header = TRUE)
allstates <- (map_data("state"))

#Merge
Total <- merge(allstates, BOTH.R, by="region")
Total <- merge(Total, rightToWorkStates, by="region")

nationalUnemploymentRate <- 4.7
#Manipulate
Total <- Total%>%
              mutate(unemploymentRateComparedToNational = ifelse(unemploymentRate < nationalUnemploymentRate, 1, 
                                                                 ifelse(unemploymentRate >= nationalUnemploymentRate, 2, 0)))

Total <- Total%>%
              mutate(unemployementRateAndRightToWork = ifelse(unemploymentRateComparedToNational == 1 & rightToWork == 0, "Non-RTW BELOW Natl. Unemployment Rate", 
                                                              ifelse(unemploymentRateComparedToNational == 2 & rightToWork == 0, "Non-RTW ABOVE Natl. Unemployment Rate", 
                                                                     ifelse(unemploymentRateComparedToNational == 1 & rightToWork == 1, "RTW BELOW Natl. Unemployment Rate", 
                                                                            ifelse(unemploymentRateComparedToNational == 2 & rightToWork == 1, "RTW ABOVE Natl. Unemployment Rate", 0)))))

#Visualize
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group,  fill=Total$unemployementRateAndRightToWork), 
                      colour = "white") + scale_fill_brewer(palette="RdGy", 
                                                            direction = -1, name="Legend")

P1 <- p + theme_bw()+ labs(title= "Unemployment Rate by State", x="", y="")+
       theme(plot.title=element_text(size = 24, face = "bold"), 
             legend.text=element_text(size=24), 
             legend.title=element_blank())
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


#Count number of states in each catagory
states <- Total %>% 
              distinct(region)%>%
              count(unemployementRateAndRightToWork)
states
              
       
                                
belowMedian + aboveUnemployment

count(Total, unemployementRateAndRightToWork)
