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

indianaHousing    <- pumaFilter(indianaHousing, "inPUMA")
kentuckyHousing   <- pumaFilter(kentuckyHousing, "kyPUMA")

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
test <- percentFunction(blackMastersSex)

blackMastersAge   <- countWeight(blackMasters, "AGE")
whiteMastersAge   <- countWeight(whiteMasters, "AGE")
blackBachelorsAge <- countWeight(blackBachelors, "AGE")
whiteBachelorsAge <- countWeight(whiteBachelors, "AGE")

blackMastersSex   <- percentFunction(countWeight(blackMasters, "SEX"))
whiteMastersSex   <- percentFunction(countWeight(whiteMasters, "SEX"))
blackBachelorsSex <- percentFunction(countWeight(blackBachelors, "SEX"))
whiteBachelorsSex <- percentFunction(countWeight(whiteBachelors, "SEX"))

blackMastersDis   <- percentFunction(countWeight(blackMasters, "DIS"))
whiteMastersDis   <- percentFunction(countWeight(whiteMasters, "DIS"))
blackBachelorsDis <- percentFunction(countWeight(blackBachelors, "DIS"))
whiteBachelorsDis <- percentFunction(countWeight(whiteBachelors, "DIS"))


# Calculate Percents
disabilityBA$percent <- (disabilityBA$n)/sum(disabilityBA$n)
disabilityMA$percent <- (disabilityMA$n)/sum(disabilityMA$n)

genderBA$percent <- (genderBA$n)/sum(genderBA$n)
genderMA$percent <- (genderMA$n)/sum(genderMA$n)

## VISUALIZATOINS

sexLabels <- c("Male", "Female")
disabilityLabels <- c("Yes", "No")

colors <- c('CCCC66', '#00CCFF')
makePie <- function(enterData, enterTitle, enterLabels) {
        p <- plot_ly(enterData, labels = enterLabels, values = ~percent, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#003333', 
                                           size  = 18),
                     # hoverinfo = 'text',
                     # text = ~paste('$', X1960, ' billions'),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE) %>%
          layout(title = enterTitle,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        p
}

makePie(genderMA, "Black or African American, Masters", sexLabels)
makePie(disabilityBA, "Black or African American with Bachelors, Disablity", c("Yes", "No"))
makePie(disabilityMA, "Black or African American with Masters, Disability", c("Yes", "No"))
