library(RCurl)
library(dplyr)
library(plotrix)
library(scales)
library(plotly)
library(ggplot2)
library(treemap)

## Read pums data
alabamaPopulation <- read.csv("ss15pal.csv")
arkansasPopulation <- read.csv("ss15par.csv")
indianaPopulation <- read.csv("ss15pin.csv")
iowaPopulation <- read.csv("ss15pia.csv")
kentuckyPopulation <- read.csv("ss15pky.csv")
mississippiPopulation <- read.csv("ss15pms.csv")
nebraskaPopulation <- read.csv("ss15pne.csv")
northCarolinaPopulation <- read.csv("ss15pnc.csv")
ohioPopulation <- read.csv("ss15poh.csv")
southCarolinaPopulation <- read.csv("ss15poh.csv")
tennesseePopulation <- read.csv("ss15ptn.csv")
virginiaPopulation <- read.csv("ss15pva.csv")

alabamaHousing <- read.csv("ss15pal.csv") 
arkansasHousing <- read.csv("ss15par.csv")
indianaHousing <- read.csv("ss15pin.csv")
iowaHousing <- read.csv("ss15pia.csv")
kentuckyHousing <- read.csv("ss15pky.csv")
mississippiHousing <- read.csv("ss15pms.csv")
nebraskaHousing <- read.csv("ss15pne.csv")
northCarolina <- read.csv("ss15pnc.csv")
ohioHousing <- read.csv("ss15poh.csv")
southCarolinaHousing <- read.csv("ss15psc.csv")
tennesseeHousing <- read.csv("ss15ptn.csv")
virginiaHousing <- read.csv("ss15pva.csv")




# add PUMA list for filtering
dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1iPaxw8E2MWhPlgxo5IYOywfmr1YI3LVZ4-_OkoXsbwo/pub?gid=142112935&single=true&output=csv")
pumas          <- read.csv(textConnection(dataConnection))
rm(dataConnection)


## FUNCTIONS
pumaFilter <- function(enterData, enterPUMASList) {
  dataSave <- enterData %>% filter(PUMA %in% pumas[, enterPUMASList])
}

## FILTER DATA
indianaHousing     <- pumaFilter(indianaHousing, "inPUMA")
kentuckyHousing    <- pumaFilter(kentuckyHousing, "kyPUMA")

indianaPopulation  <- pumaFilter(indianaPopulation, "inPUMA")
kentuckyPopulation <- pumaFilter(kentuckyPopulation, "kyPUMA")

# merge population and housing records
indiana  <- left_join(indianaPopulation, indianaHousing, by = "SERIALNO")
kentucky <- left_join(kentuckyPopulation, kentuckyHousing, by = "SERIALNO")






