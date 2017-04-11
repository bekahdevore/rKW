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
northCarolinaHousing <- read.csv("ss15pnc.csv")
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
alabamaPopulation <- pumaFilter(alabamaPopulation, "alabamaPumas")
arkansasPopulation <- pumaFilter(arkansasPopulation, "arkansasPumas")
indianaPopulation <- pumaFilter(indianaPopulation, "indianaPumas")
iowaPopulation <- pumaFilter(iowaPopulation, "iowaPumas")
kentuckyPopulation <- pumaFilter(kentuckyPopulation, "kentuckyPumas")
mississippiPopulation <- pumaFilter(mississippiPopulation, "mississippiPumas")
nebraskaPopulation <- pumaFilter(nebraskaPopulation, "nebraskaPumas")
northCarolinaPopulation <- pumaFilter(northCarolinaPopulation, "nCarolinaPumas")
ohioPopulation <- pumaFilter(ohioPopulation, "ohioPumas")
southCarolinaPopulation <- pumaFilter(southCarolinaPopulation, "sCarolinaPumas")
tennesseePopulation <- pumaFilter(tennesseePopulation, "tennesseePumas")
virginiaPopulation <- pumaFilter(virginiaPopulation, "virginiaPumas")

alabamaHousing <- pumaFilter(alabamaHousing, "alabamaPumas")
arkansasHousing <- pumaFilter(arkansasHousing, "arkansasPumas") 
indianaHousing <- pumaFilter(indianaHousing, "indianaPumas")
iowaHousing <- pumaFilter(iowaHousing, "iowaPumas")
kentuckyHousing <- pumaFilter(kentuckyHousing, "kentuckyPumas")
mississippiHousing <- pumaFilter(mississippiHousing, "mississippiPumas")
nebraskaHousing <- pumaFilter(nebraskaHousing, "nebraskaPumas")
northCarolinaHousing <- pumaFilter(northCarolinaHousing, "nCarolinaPumas")
ohioHousing <- pumaFilter(ohioHousing, "ohioPumas")
southCarolinaHousing <- pumaFilter(southCarolinaHousing, "sCarolinaPumas")
tennesseeHousing <- pumaFilter(tennesseeHousing, "tennesseePumas")
virginiaHousing <- pumaFilter(virginiaHousing, "virginiaPumas")

# merge population and housing records
alabama <- left_join(alabamaPopulation, alabamaHousing, by = "SERIALNO")
arkansas <- left_join(arkansasPopulation, arkansasHousing, by = "SERIALNO")
indiana <- left_join(indianaPopulation, indianaHousing, by = "SERIALNO")
iowa <- left_join(iowaPopulation, iowaHousing, by = "SERIALNO")
kentucky <- left_join(kentuckyPopulation, kentuckyHousing, by = "SERIALNO")
mississippi <- left_join(mississippiPopulation, mississippiHousing, by = "SERIALNO")
nebraska <- left_join(nebraskaPopulation, nebraskaHousing, by = "SERIALNO")
northCarolina <- left_join(northCarolinaPopulation, northCarolinaHousing, by = "SERIALNO")
ohio <- left_join(ohioPopulation, ohioHousing, by = "SERIALNO")
southCarolina <- left_join(southCarolinaPopulation, southCarolinaHousing, by = "SERIALNO")
tennessee <- left_join(tennesseePopulation, tennesseeHousing, by = "SERIALNO")
virginia <- left_join(virginiaPopulation, virginiaHousing, by = "SERIALNO")


allData <- rbind(
                  alabama,
                  arkansas, 
                  indiana, 
                  iowa,
                  kentucky, 
                  mississippi,
                  nebraska, 
                  northCarolina,
                  ohio, 
                  southCarolina, 
                  tennessee, 
                  virginia
              )

#write.csv(allData, file = "peerCityPUMS.csv" )
save(allData, file = "peerCityPums.RData")
