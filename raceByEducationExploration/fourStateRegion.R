library(RCurl)
library(dplyr)
library(plotrix)
library(scales)
library(plotly)
library(ggplot2)

## ADD DATA
indianaHousing  <- read.csv("ss15hin.csv")
kentuckyHousing <- read.csv("ss15hky.csv")
ohioHousing <- read.csv("ss15hoh.csv")
tennesseeHousing <- read.csv("ss15htn.csv")

indianaPopulation  <- read.csv("ss15pin.csv")
kentuckyPopulation <- read.csv("ss15pky.csv")
ohioPopulation <- read.csv("ss15poh.csv")
tennesseePopulation <- read.csv("ss15ptn.csv")

# add PUMA list for filtering
dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv")
pumas          <- read.csv(textConnection(dataConnection))
rm(dataConnection)


## FUNCTIONS
# pumaFilter <- function(enterData, enterPUMASList) {
#   dataSave <- enterData %>% filter(PUMA %in% pumas[, enterPUMASList])
# }

countWeight <- function(enterData, countThis){
  if(countThis == "SEX") {
    enterData %>% count(maleFemale, wt = PWGTP)}
  else if(countThis == "AGE") {
    enterData %>% count(AGEP, wt = PWGTP)
  }
  else if(countThis == "DIS") {
    enterData %>% count(disability, wt = PWGTP)
  }
  else if(countThis == "ageMajor") {
    enterData %>% count(ageGroup, wt = PWGTP)
  }
  else if(countThis == "insurance") {
    enterData %>% count(healthInsurance, wt = PWGTP)
  }
  else if(countThis == "married") {
    enterData %>% count(married, wt = PWGTP)
  }
  
  else if(countThis == "children") {
    enterData %>% count(children, wt = PWGTP)
  }
  else if(countThis == "income") {
    enterData %>% count(householdIncome, wt = PWGTP)
  }
  else if(countThis == "singleMoms") {
    enterData %>% count(singleMoms, wt = PWGTP)
  }
  else if(countThis == "poverty") {
    enterData %>% count(poverty, wt = PWGTP)
  }
  else if(countThis == "homeValue") {
    enterData %>% count(homeValue, wt = PWGTP)
  }
  else if(countThis == "rentOwn") {
    enterData %>% count(rentOwn, wt = PWGTP)
  }
  else if(countThis == "householdSize") {
    enterData %>% count(householdSize, wt = WGTP)
  }
  else if(countThis == "childrenUnder6") {
    enterData %>% count(childrenUnder6, wt = PWGTP)
  }
}

percentFunction <- function(enterData){
  enterData$percent <- enterData %>% dplyr::mutate(percent = ((enterData$n)/sum(enterData$n)))
}

addColumnsDataPoint <- function(enterData, dataPoint) {
  enterData <- enterData %>% 
    dplyr::mutate(DataPoint = dataPoint)  
}

addColumns <- function(enterData, race, education) {
  enterData <- enterData %>% 
    dplyr::mutate(race = race) %>% 
    dplyr::mutate(education = education)
}

weightPercent <- function(enterData, dataPoint) {
  newData <- percentFunction(countWeight(enterData, dataPoint))
  newData <- addColumnsDataPoint(newData, dataPoint)
  colnames(newData)[1] <- "Type"
  newData
}


## FILTER DATA
# indianaHousing     <- pumaFilter(indianaHousing, "inPUMA")
# kentuckyHousing    <- pumaFilter(kentuckyHousing, "kyPUMA")
# 
# indianaPopulation  <- pumaFilter(indianaPopulation, "inPUMA")
# kentuckyPopulation <- pumaFilter(kentuckyPopulation, "kyPUMA")

# merge population and housing records
indiana  <- left_join(indianaPopulation, indianaHousing, by = "SERIALNO")
kentucky <- left_join(kentuckyPopulation, kentuckyHousing, by = "SERIALNO")
ohio <- left_join(ohioPopulation, ohioHousing, by = "SERIALNO")
tennessee <- left_join(tennesseePopulation, tennesseeHousing, by = "SERIALNO")

# merge ky and in puma files 
allData <- rbind(indiana, kentucky, ohio, tennessee)
rm(indiana, indianaHousing, indianaPopulation, kentucky, kentuckyHousing, kentuckyPopulation, pumas, pumaFilter)


## CREATE AGE GROUPS
# 25 - 54, 55 +
allData <- allData %>% mutate(ageGroup = ifelse(AGEP >= 25 & AGEP <= 54, "25 - 54", 
                                                ifelse(AGEP >= 55, "55 + ", "Other"))) %>% 
  mutate(disability = ifelse(DIS == 1, "With Disability", 
                             ifelse(DIS == 2, "Without Disability", "Other"))) %>%
  mutate(maleFemale = ifelse(SEX == 1, "Male", 
                             ifelse(SEX == 2, "Female", "Other"))) %>% 
  mutate(healthInsurance = ifelse(PRIVCOV == 1, "Private Health Insurance", 
                                  ifelse(PUBCOV == 1, "Public Health Insurance", 
                                         ifelse(HICOV == 2, "No Insurance", "Other")))) %>%
  mutate(married = ifelse(MAR == 1, "Married", "Other")) %>% 
  mutate(children = ifelse(NRC == 0, "Children (None)", 
                           ifelse(NRC == 1, "Children (1 - 3)", 
                                  ifelse(NRC == 2, "Children (1 - 3)", 
                                         ifelse(NRC == 3, "Children (1 - 3)", 
                                                ifelse(NRC > 3, "Children (4 or more)", "Other")))))) %>% 
  mutate(householdIncome = ifelse(HINCP >= 47273, "Household Income above family-supporting wage", 
                                  "Household Income less than family-supporting wage")) %>% 
  mutate(singleMoms = ifelse((HHT == 3 | HHT == 6 | HHT == 7) & (HUPAOC == 1 | HUPAOC == 2 | HUPAOC == 3), "Single Mom", "Other")) %>% 
  mutate(poverty = ifelse(POVPIP <= 100, "In Poverty", "Other")) %>% 
  mutate(homeValue = ifelse(VALP <= 155700, "Home value less than median value ($155,700)", "Other")) %>% 
  mutate(rentOwn = ifelse((TEN == 1 | TEN == 2), "Own home", 
                          ifelse(TEN == 3, "Rent home", "Other"))) %>% 
  mutate(householdSize = ifelse(NP >= 5, "Household Size (5 or more)", "Other")) %>% 
  mutate(childrenUnder6 = ifelse((HUPAOC == 1 | HUPAOC == 3), "Children under 6", "Other"))



#seperate into groups by race and education 
bachelors <- allData %>% filter(SCHL == 21)
masters   <- allData %>% filter(SCHL == 22)

blackBachelors <- bachelors %>% filter(RAC1P == 2)
whiteBachelors <- bachelors %>% filter(RAC1P == 1)

blackMasters   <- masters %>% filter(RAC1P == 2)
whiteMasters   <- masters %>% filter(RAC1P == 1)


## Variables
age <-  "AGE"
dis <-  "DIS"
sex <-  "SEX"
black <- "black"
white <- "white"
masters <- "masters"
bachelors <- "bachelors"
ageMajor  <- "ageMajor"
insurance <- "insurance"
married   <- "married"
children <- "children"
income <- "income"
singleMoms <- "singleMoms"
poverty <- "poverty"
homeValue <- "homeValue"
rentOwn <- "rentOwn"
householdSize <- "householdSize"
childrenUnder6 <- "childrenUnder6"


## COUNT PERCENTAGES, Add columns, change column 1 name to Type
blackMastersAgeMajor   <- weightPercent(blackMasters, ageMajor)
whiteMastersAgeMajor   <- weightPercent(whiteMasters, ageMajor)
blackBachelorsAgeMajor <- weightPercent(blackBachelors, ageMajor)
whiteBachelorsAgeMajor <- weightPercent(whiteBachelors, ageMajor)


blackMastersDis   <- weightPercent(blackMasters,   dis)
whiteMastersDis   <- weightPercent(whiteMasters,   dis)
blackBachelorsDis <- weightPercent(blackBachelors, dis)
whiteBachelorsDis <- weightPercent(whiteBachelors, dis)

blackMastersSex   <- weightPercent(blackMasters, sex)
whiteMastersSex   <- weightPercent(whiteMasters, sex)
blackBachelorsSex <- weightPercent(blackBachelors, sex)
whiteBachelorsSex <- weightPercent(whiteBachelors, sex)

blackMastersInsurance <- weightPercent(blackMasters, insurance)
whiteMastersInsurance <- weightPercent(whiteMasters, insurance)
blackBachelorsInsurance <- weightPercent(blackBachelors, insurance)
whiteBachelorsInsurance <- weightPercent(whiteBachelors, insurance)

blackMastersMarried <- weightPercent(blackMasters, married)
whiteMastersMarried <- weightPercent(whiteMasters, married)
blackBachelorsMarried <- weightPercent(blackBachelors, married)
whiteBachelorsMarried <- weightPercent(whiteBachelors, married)

blackMastersChildren <- weightPercent(blackMasters, children)
whiteMastersChildren <- weightPercent(whiteMasters, children)
blackBachelorsChildren <- weightPercent(blackBachelors, children)
whiteBachelorsChildren <- weightPercent(whiteBachelors, children)

blackMastersIncome <- weightPercent(blackMasters, income)
whiteMastersIncome <- weightPercent(whiteMasters, income)
blackBachelorsIncome <- weightPercent(blackBachelors, income)
whiteBachelorsIncome <- weightPercent(whiteBachelors, income)

blackMastersSingleMoms <- weightPercent(blackMasters, singleMoms)
whiteMastersSingleMoms <- weightPercent(whiteMasters, singleMoms)
blackBachelorsSingleMoms <- weightPercent(blackBachelors, singleMoms)
whiteBachelorsSingleMoms <- weightPercent(whiteBachelors, singleMoms)

blackMastersPoverty <- weightPercent(blackMasters, poverty)
whiteMastersPoverty <- weightPercent(whiteMasters, poverty)
blackBachelorsPoverty <- weightPercent(blackBachelors, poverty)
whiteBachelorsPoverty <- weightPercent(whiteBachelors, poverty)

blackMastersHomeValue <- weightPercent(blackMasters, homeValue)
whiteMastersHomeValue <- weightPercent(whiteMasters, homeValue)
blackBachelorsHomeValue <- weightPercent(blackBachelors, homeValue)
whiteBachelorsHomeValue <- weightPercent(whiteBachelors, homeValue)

blackMastersRentOwn <- weightPercent(blackMasters, rentOwn)
whiteMastersRentOwn <- weightPercent(whiteMasters, rentOwn)
blackBachelorsRentOwn <- weightPercent(blackBachelors, rentOwn)
whiteBachelorsRentOwn <- weightPercent(whiteBachelors, rentOwn)

blackMastersHouseholdSize <- weightPercent(blackMasters, householdSize)
whiteMastersHouseholdSize <- weightPercent(whiteMasters, householdSize)
blackBachelorsHouseholdSize <- weightPercent(blackBachelors, householdSize)
whiteBachelorsHouseholdSize <- weightPercent(whiteBachelors, householdSize)

blackMastersChildrenUnder6 <- weightPercent(blackMasters, childrenUnder6)
whiteMastersChildrenUnder6 <- weightPercent(whiteMasters, childrenUnder6)
blackBachelorsChildrenUnder6 <- weightPercent(blackBachelors, childrenUnder6)
whiteBachelorsChildrenUnder6 <- weightPercent(whiteBachelors, childrenUnder6)








### BIND DATA SETS TOGETHER
blackMasters <- rbind(blackMastersSex, blackMastersAgeMajor, blackMastersDis, blackMastersInsurance, 
                      blackMastersMarried, blackMastersChildren, blackMastersIncome, blackMastersSingleMoms, 
                      blackMastersPoverty, blackMastersHomeValue, blackMastersRentOwn, blackMastersHouseholdSize, 
                      blackMastersChildrenUnder6)
whiteMasters <- rbind(whiteMastersSex, whiteMastersAgeMajor, whiteMastersDis, whiteMastersInsurance, 
                      whiteMastersMarried, whiteMastersChildren, whiteMastersIncome, whiteMastersSingleMoms, 
                      whiteMastersPoverty, whiteMastersHomeValue, whiteMastersRentOwn, whiteMastersHouseholdSize, 
                      whiteMastersChildrenUnder6)
blackBachelors <- rbind(blackBachelorsSex, blackBachelorsAgeMajor, blackBachelorsDis, blackBachelorsInsurance, 
                        blackBachelorsMarried, blackBachelorsChildren, blackBachelorsIncome, blackBachelorsSingleMoms, 
                        blackBachelorsPoverty, blackBachelorsHomeValue, blackBachelorsRentOwn, blackBachelorsHouseholdSize, 
                        blackBachelorsChildrenUnder6)
whiteBachelors <- rbind(whiteBachelorsSex, whiteBachelorsAgeMajor, whiteBachelorsDis, whiteBachelorsInsurance, 
                        whiteBachelorsMarried, whiteBachelorsChildren, whiteBachelorsIncome, whiteBachelorsSingleMoms, 
                        whiteBachelorsPoverty, whiteBachelorsHomeValue, whiteBachelorsRentOwn, whiteBachelorsHouseholdSize, 
                        whiteBachelorsChildrenUnder6)

blackMasters <- addColumns(blackMasters, black, masters)
whiteMasters <- addColumns(whiteMasters, white, masters)
blackBachelors <- addColumns(blackBachelors, black, bachelors)
whiteBachelors <- addColumns(whiteBachelors, white, bachelors)

allDataFinal <- as.data.frame(rbind(blackMasters, whiteMasters, blackBachelors, whiteBachelors))
allDataFinal <- allDataFinal %>% filter(Type != "Other" & Type != "Without Disability" & 
                                          Type != "Household Income above family-supporting wage" & 
                                          Type != "Male")

cbPalette <- c(
  '#9C0059',
  '#A4D7F4',
  '#8DC63F',
  '#F8971D',
  '#D31245',
  '#A4D7F4',
  '#00853F',
  '#767662'
)


allDataFinal$label <- percent(allDataFinal$percent)

allDataFinal$Type <- factor(allDataFinal$Type, levels = allDataFinal$Type[order(allDataFinal$percent)])

p <- ggplot(allDataFinal, aes(x = Type, y = percent, fill = race, label = label)) + 
  geom_bar(stat = 'identity', position = 'dodge') + facet_grid(~ education) + 
  geom_text(position = position_dodge(width = 1), hjust = -.10)


highDemand <- p                                  + 
  coord_flip()                       + 
  facet_grid(education ~ ., switch = 'y') + 
  #theme_minimal()                    +
  theme(strip.text.y = element_text(#angle = 180, 
    # hjust = 1, 
    size = 9, 
    face = 'bold'),
    # strip.background  = element_rect(fill   = 'white'),
    # color  = 'grey'),
    # panel.background  = element_rect(fill   = 'white'),
    # color  = 'grey'),
    # axis.text.y       = element_blank(), 
    axis.ticks.y      = element_blank(), 
    axis.text.x       = element_text(size = 9), 
    legend.title      = element_blank(), 
    legend.text       = element_text(size = 14),
    # face = 'bold'), 
    legend.position   = c(.9, .6), 
    legend.background = element_blank(),
    # legend.key        = element_rect(color = 'white', 
    #                                  size = 3),
    legend.key.size   = unit(1, 'lines'),
    axis.title        = element_blank()) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) + 
  scale_fill_manual(values = cbPalette)

highDemand


write.csv(allDataFinal, file = "allDataFinalFourState.csv")
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


