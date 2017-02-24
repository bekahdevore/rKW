library(RCurl)
library(dplyr)
library(plotrix)
library(scales)
library(plotly)
#Add pums household and population data
indianaHousing  <- read.csv("ss15hin.csv")
kentuckyHousing <- read.csv("ss15hky.csv")

indianaPopulation  <- read.csv("ss15pin.csv")
kentuckyPopulation <- read.csv("ss15pky.csv")

# add PUMA list for filtering
dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv")
pumas          <- read.csv(textConnection(dataConnection))
rm(dataConnection)


# filter for MSA
pumaFilter <- function(enterData, enterPUMASList) {
   dataSave <- enterData %>% filter(PUMA %in% pumas[, enterPUMASList])
}

indianaHousing     <- pumaFilter(indianaHousing, "inPUMA")
kentuckyHousing    <- pumaFilter(kentuckyHousing, "kyPUMA")

indianaPopulation  <- pumaFilter(indianaPopulation, "inPUMA")
kentuckyPopulation <- pumaFilter(kentuckyPopulation, "kyPUMA")

# merge population and housing records
indiana  <- left_join(indianaPopulation, indianaHousing, by = "SERIALNO")
kentucky <- left_join(kentuckyPopulation, kentuckyHousing, by = "SERIALNO")

# merge ky and in puma files 
allData <- rbind(indiana, kentucky)
rm(indiana, indianaHousing, indianaPopulation, kentucky, kentuckyHousing, kentuckyPopulation, pumas, pumaFilter)

#seperate into groups by race and education 
bachelors <- allData %>% filter(SCHL == 21)
masters   <- allData %>% filter(SCHL == 22)

blackBachelors <- bachelors %>% filter(RAC1P == 2)
whiteBachelors <- bachelors %>% filter(RAC1P == 1)

blackMasters   <- masters %>% filter(RAC1P == 2)
whiteMasters   <- masters %>% filter(RAC1P == 1)


countWeight <- function(enterData, countThis){
  if(countThis == "SEX") {
  enterData %>% count(SEX, wt = PWGTP)}
  else if(countThis == "AGE") {
    enterData %>% count(AGEP, wt = PWGTP)
  }
  else if(countThis == "DIS") {
    enterData %>% count(DIS, wt = PWGTP)
  }
}

percentFunction <- function(enterData){
  enterData$percent <- enterData %>% dplyr::mutate(percent = ((enterData$n)/sum(enterData$n)))}

age = "AGE"
dis = "DIS"
sex = "SEX"

blackMastersAge   <- countWeight(blackMasters,   age)
whiteMastersAge   <- countWeight(whiteMasters,   age)
blackBachelorsAge <- countWeight(blackBachelors, age)
whiteBachelorsAge <- countWeight(whiteBachelors, age)

blackMastersDis   <- percentFunction(countWeight(blackMasters,   dis))
whiteMastersDis   <- percentFunction(countWeight(whiteMasters,   dis))
blackBachelorsDis <- percentFunction(countWeight(blackBachelors, dis))
whiteBachelorsDis <- percentFunction(countWeight(whiteBachelors, dis))

blackMastersSex   <- percentFunction(countWeight(blackMasters,   sex))
whiteMastersSex   <- percentFunction(countWeight(whiteMasters,   sex))
blackBachelorsSex <- percentFunction(countWeight(blackBachelors, sex))
whiteBachelorsSex <- percentFunction(countWeight(whiteBachelors, sex))

addColumns <- function(enterData, race, education) {
                  enterData <- enterData %>% dplyr::mutate(race = race) %>% dplyr::mutate(education = education)
}

black <- "black"
white <- "white"
masters <- "masters"
bachelors <- "bachelors"

blackMastersSex <- addColumns(blackMastersSex, black, masters)
whiteMastersSex <- addColumns(whiteMastersSex, white, masters)

blackBachelorsSex <- addColumns(blackMastersSex, black, bachelors)
whiteBachelorsSex <- addColumns(whiteMastersSex, white, bachelors)





## VISUALIZATIONS
# sexLabels <- c("Male", "Female")
# disabilityLabels <- c("Yes", "No")
# 
# colors <- c('CCCC66', '#00CCFF')
# makePie <- function(enterData, enterTitle, enterLabels) {
#         p <- plot_ly(enterData, labels = enterLabels, values = ~percent, type = 'pie',
#                      textposition = 'inside',
#                      textinfo = 'label+percent',
#                      insidetextfont = list(color = '#003333', 
#                                            size  = 18),
#                      # hoverinfo = 'text',
#                      # text = ~paste('$', X1960, ' billions'),
#                      marker = list(colors = colors,
#                                    line = list(color = '#FFFFFF', width = 1)),
#                      #The 'pull' attribute can also be used to create space between the sectors
#                      showlegend = FALSE) %>%
#           layout(title = enterTitle,
#                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#         p
# }
# 
# makePie(blackMastersSex, "Black with Master's Degree, Sex", sexLabels)
# makePie(whiteMastersSex, "White with Master's Degree, Sex", sexLabels)
# makePie(blackBachelorsSex, "Black with Bachelor's Degree, Sex", sexLabels)
# makePie(whiteBachelorsSex, "White with Bachelor's Degree, Sex", sexLabels)
# 
# makePie(blackMastersDis, "Black with Master's Degree, Disablity", disabilityLabels)
# makePie(whiteMastersDis, "White with Master's Degree, Disability", disabilityLabels)
# makePie(blackBachelorsDis, "Black with Bachelor's Degree, Disability", disabilityLabels)
# makePie(whiteBachelorsDis, "White with Bachelor's Degree, Disability", disabilityLabels)

