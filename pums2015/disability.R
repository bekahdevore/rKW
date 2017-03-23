library(RCurl)
library(dplyr)
library(plotrix)
library(scales)
library(plotly)
library(ggplot2)
library(treemap)

## Read pums data
indianaPopulation  <- read.csv("ss15pin.csv")
kentuckyPopulation <- read.csv("ss15pky.csv")

# add PUMA list for filtering
dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv")
pumas          <- read.csv(textConnection(dataConnection))
rm(dataConnection)


## FUNCTIONS
pumaFilter <- function(enterData, enterPUMASList) {
  dataSave <- enterData %>% filter(PUMA %in% pumas[, enterPUMASList])
}

## Filter data
indianaPopulation  <- pumaFilter(indianaPopulation, "inPUMA")
kentuckyPopulation <- pumaFilter(kentuckyPopulation, "kyPUMA")

## Merge data
allData <- rbind(indianaPopulation, kentuckyPopulation)
rm(indianaPopulation, kentuckyPopulation, pumas, pumaFilter)

## Look at weighted totals for specific populations
totalPopulation <- sum(allData$PWGTP)
withDisability <- sum(subset(allData, DIS == 1, select=PWGTP))

percentWithDisability <- percent(withDisability/totalPopulation)

specificDisability <- allData %>% 
  filter(DIS == 1) %>% 
  mutate(visual = ifelse(DEYE == 1, 1, 0)) %>% 
  mutate(hearing = ifelse(DEAR == 1, 1, 0)) %>% 
  mutate(ambulatory = ifelse(DPHY == 1, 1, 0)) %>% 
  mutate(cognitive = ifelse(DREM == 1, 1, 0)) %>% 
  mutate(selfCare = ifelse(DDRS == 1, 1, 0)) %>% 
  mutate(independentLiving = ifelse(DOUT == 1, 1, 0)) %>% 
  select(visual, hearing, ambulatory, cognitive, selfCare, independentLiving, PWGTP) 

#set NA to zero
specificDisability[is.na(specificDisability)] <- 0

specificDisability <- specificDisability %>% 
  mutate(disability =
                  ifelse((visual == 1 & hearing == 0 & ambulatory == 0 & cognitive == 0 & selfCare == 0 & independentLiving == 0), "Visual", 
                  ifelse((visual == 0 & hearing == 1 & ambulatory == 0 & cognitive == 0 & selfCare == 0 & independentLiving == 0), "Hearing", 
                  ifelse((visual == 0 & hearing == 0 & ambulatory == 1 & cognitive == 0 & selfCare == 0 & independentLiving == 0), "Ambulatory", 
                  ifelse((visual == 0 & hearing == 0 & ambulatory == 0 & cognitive == 1 & selfCare == 0 & independentLiving == 0), "Cognitive", 
                  ifelse((visual == 0 & hearing == 0 & ambulatory == 0 & cognitive == 0 & selfCare == 1 & independentLiving == 0), "Self-Care", 
                  ifelse((visual == 0 & hearing == 0 & ambulatory == 0 & cognitive == 0 & selfCare == 0 & independentLiving == 1), "Independent Living", 
                  "Multiple disabilities")))))))

disabilityCount <- count(specificDisability, disability, wt = PWGTP)
disabilityCount$percent <- disabilityCount$n/withDisability
disabilityCountVisualizationData <- disabilityCount %>% select(disability, percent)

disabilityCountVisualizationData$label <- paste0(disabilityCountVisualizationData$disability, "\n", percent(disabilityCountVisualizationData$percent))
treemap(disabilityCountVisualizationData, "label", "percent", title = "Specific Disabilities, Louisville MSA, PUMS 2015")

louisvilleMSA <- as.data.frame(c((totalPopulation-withDisability), withDisability))
colnames(louisvilleMSA)[1] <- "n"
louisvilleMSA$percent <- percent(louisvilleMSA$n/sum(louisvilleMSA$n))
louisvilleMSA$names <- c("Without Disability", "With Disability")
louisvilleMSA$label <- paste0(louisvilleMSA$names, "\n", louisvilleMSA$percent)

treemap(louisvilleMSA, "label", "n", title = "")
treemap(disabilityCountVisualizationData, "label", "percent", title = "")


