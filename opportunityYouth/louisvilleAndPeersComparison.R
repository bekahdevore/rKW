library(scales)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(spatstat)
library(tidyr)
library(reshape2)
library(dplyr)

load("louisvilleAllDataAllVariables.RData")
load("peerCitiesAllDataAllVariables.RData")
load("kentuckyAllDataAllVariables.RData")


cleanData <- function(dataHere) {
#CLEAN
  ## Select variables needed
  allData <- dataHere %>% select(PWGTP, RAC1P, HISP, AGEP, DIS, SCHL, PERNP, ESR, NAICSP, OCCP, FOD1P)
  ## RACE
  allData <- allData %>% mutate(race = ifelse(RAC1P == 1, "White", 
                                              ifelse(RAC1P == 2, "Black", "Other")))
  allData <- allData %>% mutate(hispanic = ifelse(HISP == 1, "nonHispanic", "Hispanic"))
  
  ## AGE
  allData <- allData %>% mutate(age = ifelse(AGEP < 16, "Under 16", 
                                             ifelse(AGEP >= 16 & AGEP <= 54, "16 to 54", 
                                                    ifelse(AGEP >= 55, "55 +", "Other"))))
  ## DISABILITY
  allData <- allData %>% mutate(disability = ifelse(DIS == 1, "With Disability", "Without Disability"))
  
  ## EDU ATTAINMENT
  allData <- allData %>% mutate(edu = ifelse(SCHL <= 15, "< High School", 
                                             ifelse(SCHL >= 16 & SCHL <= 19, "High School", 
                                                    ifelse(SCHL == 20, "Associates", 
                                                           ifelse(SCHL == 21, "Bachelors", 
                                                                  ifelse(SCHL >= 22, "Grad. Degree", "Other"))))))  
  ## MEDIAN INCOME
  allData <- allData %>% mutate(medianIncome = weighted.median(PERNP, PWGTP, na.rm = TRUE))
  allData <- allData %>% mutate(meanIncome = weighted.mean(PERNP, PWGTP, na.rm = TRUE))
  
  ## INCOME DISTRIBUTION
  ## ESR DATA
  allData <- allData %>% mutate(laborStats = ifelse(ESR == 1 | ESR == 2, "Employed", 
                                                    ifelse(ESR == 3, "Unemployed", 
                                                           ifelse(ESR == 6, "Not in Labor Force", 
                                                                "Other"))))
      a <- group_by(allData, laborStats)
      b <- count(a, wt = PWGTP)
      c <- spread(b, laborStats, n)
      ## % UNEMPLOYED
      unemploymentRateVariable <- (c$Unemployed/(c$Employed + c$Unemployed))
      ## LABOR FORCE PARTICIPATION
      laborForceParticipationVariable <- (c$Unemployed+c$Employed)/(c$Unemployed+c$Employed+c$`Not in Labor Force`)
      ## % NOT IN LABOR FORCE
      notInLaborForceVariable <- c$`Not in Labor Force`/(c$Unemployed+c$Employed+c$`Not in Labor Force`)
  ## ADD VARIABLES TO DATASET
  allData <- allData %>% mutate(unemploymentRate = unemploymentRateVariable) %>%
    mutate(laborForceParticipation = laborForceParticipationVariable) %>%
    mutate(notInLaborForce = notInLaborForceVariable)
  ## INDUSTRY
  ## OCCUPATION
  ## FIELD OF DEGREE (EXTRA IF TIME)
}

peerCitiesData <- cleanData(peerCitiesAllDataAllVariables) %>% mutate(place = "Peers")
louisvilleData <- cleanData(louisvilleAllDataAllVariables) %>% mutate(place = "Louisville")
kentuckyData <- cleanData(kentuckyAllDataAllVariables) %>% mutate(place = "Kentucky")

allData <- rbind(
  peerCitiesData, 
  louisvilleData, 
  kentuckyData
)

colorsRace <- c("#FFFF66", "#D9E87D", "#267DE8")
colorsEdu <- c("#FFFF66", "#D9E87D", "#BFD98C", "#0066FF", "#408CD9", "#80B2B2")
colorsAge <- c("#D9E87D", "#267DE8", "#FFFF66")
colorsUR <-  c("#267DE8", "#FFFF66", "#D9E87D")
colorsDis <- c("#FFFF66", "#267DE8", "#D9E87D")

# VISUALIZE
  ## STACKED BARS
  a <- allData %>% group_by(race, place) 
  a <- count(a, wt = PWGTP)
  a <- spread(a, race, n)
  a <- a %>% mutate(percentBlack = Black/(Black + White + Other)) %>% 
    mutate(percentOther = Other/(Black + White + Other)) %>% 
    mutate(percentWhite = White/(Black + White + Other)) %>% 
    select(1, 5:7)
  colnames(a)[2] <- "Black"
  colnames(a)[3] <- "Other"
  colnames(a)[4] <- "White"
  a <- melt(a)


  b <- allData %>% group_by(edu, place) 
  b <- count(b, wt = PWGTP)
  b <- spread(b, edu, n)
  b <- b %>% 
    mutate(lessHS = `< High School`/(`< High School` + `High School` + Associates + Bachelors + `Grad. Degree`)) %>% 
    mutate(highSchool = `High School`/(`< High School` + `High School` + Associates + Bachelors + `Grad. Degree`)) %>% 
    mutate(associate = Associates/(`< High School` + `High School` + Associates + Bachelors + `Grad. Degree`)) %>% 
    mutate(bachelor = Bachelors/(`< High School` + `High School` + Associates + Bachelors + `Grad. Degree`)) %>% 
    mutate(grad = `Grad. Degree`/(`< High School` + `High School` + Associates + Bachelors + `Grad. Degree`)) %>% 
    select(1, 8:12)
  colnames(b)[2] <- "< High School"
  colnames(b)[3] <- "High School"
  colnames(b)[4] <- "Associates"
  colnames(b)[5] <- "Bachelors"
  colnames(b)[6] <- "Graduate +"
  b <- melt(b)
  
  c <- allData %>% group_by(age, place) 
  c <- count(c, wt = PWGTP)
  c <- spread(c, age, n)
  c <- c %>% 
    mutate(under16 = `Under 16`/(`Under 16` + `16 to 54` + `55 +`)) %>% 
    mutate(sixteenTo54 = `16 to 54`/(`Under 16` + `16 to 54` + `55 +`)) %>% 
    mutate(fiftyFivePlus = `55 +`/(`Under 16` + `16 to 54` + `55 +`)) %>% 
    select(1, 5:7)
  colnames(c)[2] <- "< 16"
  colnames(c)[3] <- "16 to 54"
  colnames(c)[4] <- "55 +"
  c <- melt(c)
  
  visualizeThis <- function(dataHere, colorsHere, titleHere, hideLegend) {  
    g <- ggplot(dataHere, aes(x = place, y = value,  fill = variable))
    g <- g + geom_bar(position = "fill", stat = "identity") + 
      geom_text(aes(label = variable), position = position_stack(vjust = 0.5)) +
      scale_y_continuous(labels = percent_format()) +
      theme_minimal() +
      scale_fill_manual(values = colorsHere) + 
      ggtitle(titleHere)
    if(hideLegend == TRUE) {
      g + theme(
        legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank()
      )
    } else {
      g + theme(
        legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks = element_blank()
      )
    }
  }
  
  # BARS
  ## HISPANIC
  d <- allData %>% group_by(hispanic, place) 
  d <- count(d, wt = PWGTP)
  d <- spread(d, hispanic, n)
  d <- d %>% 
    mutate(percentHispanic = Hispanic/(nonHispanic + Hispanic)) %>% 
    select(1, 4)
  colnames(d)[2] <- "value"
  
  ## DISABILITY
  e <- allData %>% group_by(disability, place) 
  e <- count(e, wt = PWGTP)
  e <- spread(e, disability, n)
  e <- e %>% 
    mutate(percentDis = `With Disability`/(`With Disability` + `Without Disability`)) %>% 
    select(1, 4)
  colnames(e)[2] <- "value"
  e$place <- factor(e$place, levels=c("Peers", "Louisville", "Kentucky"))
  
  ## Unemployment Rate
  f <- subset(allData, !duplicated(place))
  lfpr <- f %>% select(place, laborForceParticipation)
  ur <- f %>% select(place, unemploymentRate)
  colnames(lfpr)[2] <- "value"
  colnames(ur)[2] <- "value"
  ur$place <- factor(ur$place, levels=c("Louisville", "Peers", "Kentucky"))

simpleBar <- function(dataHere, colorsHere, titleHere, upperLimit, hideLegend) {
  
  g <- ggplot(dataHere, aes(x = place, y = value, fill = place))  
  g <- g + geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, upperLimit),labels = percent_format()) +
    theme_minimal() +
    scale_fill_manual(values = colorsHere) +
    ggtitle(titleHere) 
  if(hideLegend == TRUE) {
  g + theme(
      legend.position = "none", 
      axis.title = element_blank(),
      axis.ticks = element_blank(), 
      axis.text.y = element_blank()
    )
  } else {
    g + theme(
      legend.position = "none", 
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  }
}






gridLabels <- c("Percent Hispanic", "Unemployment Rate", "Percent with Disability", 
                "Labor Force Participation Rate", "Race", "Educational Attainment", "Age")

plot1 <- simpleBar(d, colorsAge, "Percent Hispanic", .07, FALSE) #Hispanic
plot2 <- simpleBar(ur, colorsUR, "Unemployment Rate", .07, TRUE) 
plot3 <- simpleBar(e, colorsDis, "Percent with Disability", .2, FALSE) #Disability
plot4 <- simpleBar(lfpr, colorsAge, "Labor Force Participation Rate", 1, TRUE) #laborForceParticipation
plot5 <- visualizeThis(a, colorsRace, "Race", FALSE)
plot6 <- visualizeThis(b, colorsEdu, "Education", TRUE)
plot7 <- visualizeThis(c, colorsAge, "Age", FALSE)

plot_grid(plot5, plot6, plot7, plot4, plot1, plot2, plot3, moneyPlotFinal,
          labels = "", ncol = 2, align = 'v')




# library(vioplot)
# moneyStuff <- allData %>% filter(ESR == 1 | ESR == 2)
# ky <- moneyStuff %>% filter(place == "Kentucky")
# lou <- moneyStuff %>% filter(place == "Louisville")
# peerGroup <- moneyStuff %>% filter(place == "Peers")
# vioplot(ky$PERNP, lou$PERNP, peerGroup$PERNP,
#         names = c("Kentucky", "Louisville", "Peers"), 
#                   col = (c("#D9E87D", "#267DE8", "#FFFF66")))

  ## FACET GRID
  ## BOX-PLOT



