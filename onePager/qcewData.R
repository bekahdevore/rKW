### BLS  QCEW DATA 
qcewDataKentucky_2011 <- read.csv("2011_Kentucky.csv") 
qcewDataLouisville_2011 <- read.csv("2011_LouisvilleMSA.csv")

qcewDataKY_2011 <- qcewDataFilter2011(qcewDataKentucky_2011)
qcewDataLouisville_2011 <- qcewDataFilter2011(qcewDataLouisville_2011)

qcewDataUS <- qcewDataFilter(getQcewData(quarter, currentYear, blsUS), "United States")
qcewDataKY <-  qcewDataFilter(getQcewData(quarter, currentYear, blsKY), "Kentucky")
qcewDataBirmingham <- qcewDataFilter(getQcewData(quarter, currentYear, blsBirmingham), "Birmingham")
qcewDataCharlotte <- qcewDataFilter(getQcewData(quarter, currentYear, blsCharlotte), "Charlotte")
qcewDataCincinnati <- qcewDataFilter(getQcewData(quarter, currentYear, blsCincinnati), "Cincinnati")
qcewDataColumbus <- qcewDataFilter(getQcewData(quarter, currentYear, blsColumbus), "Columbus")
qcewDataDayton <- qcewDataFilter(getQcewData(quarter, currentYear, blsDayton), "Dayton")
qcewDataGreensboro <- qcewDataFilter(getQcewData(quarter, currentYear, blsGreensboro), "Greensboro")
qcewDataIndianapolis <- qcewDataFilter(getQcewData(quarter, currentYear, blsIndianapolis), "Indianapolis")
qcewDataJacksonville <- qcewDataFilter(getQcewData(quarter, currentYear, blsJacksonville), "Jacksonville")
qcewDataKansasCity <- qcewDataFilter(getQcewData(quarter, currentYear, blsKansasCity), "Kansas City")
qcewDataLouisville <- qcewDataFilter(getQcewData(quarter, currentYear, blsLouisville), "Louisville")
qcewDataMemphis <- qcewDataFilter(getQcewData(quarter, currentYear, blsMemphis), "Memphis")
qcewDataNashville <- qcewDataFilter(getQcewData(quarter, currentYear, blsNashville), "Nashville")
qcewDataOmaha <- qcewDataFilter(getQcewData(quarter, currentYear, blsOmaha), "Omaha")
qcewDataRaleigh <- qcewDataFilter(getQcewData(quarter, currentYear, blsRaleigh), "Raleigh")
qcewDataRichmond <- qcewDataFilter(getQcewData(quarter, currentYear, blsRichmond), "Richmond")

allCurrentQcewData <- rbind(qcewDataUS, qcewDataKY, qcewDataBirmingham, 
                            qcewDataCharlotte, qcewDataCincinnati, 
                            qcewDataColumbus, qcewDataDayton, qcewDataGreensboro, 
                            qcewDataIndianapolis, qcewDataJacksonville, qcewDataKansasCity, 
                            qcewDataLouisville, qcewDataMemphis, qcewDataNashville, 
                            qcewDataOmaha, qcewDataRaleigh, qcewDataRichmond)

allCurrentQcewData <- allCurrentQcewData %>% select(-1)

privateQcewData <- allCurrentQcewData %>% filter(own_code == 5) %>% select(2:4)
allQcewData <- allCurrentQcewData %>% filter(own_code == 0) %>% select(2:4)

establishmentsPrivate <- "Establishments (private)"
establishmentsAll <- "Establishments (all)"

employmentPrivate <- "Employment (private)"
employmentAll <- "Employment (all)"

colnames(privateQcewData)[1] <- establishmentsPrivate
colnames(allQcewData)[1] <- establishmentsAll

colnames(privateQcewData)[2] <- employmentPrivate
colnames(allQcewData)[2] <- employmentAll

allCurrentQcewData <- left_join(privateQcewData, allQcewData, by = "MSA") %>% select(3, 1, 4, 2, 5)

## GET GROWTH SINCE 2011 IN LOUISVILLE

# Prepare data for merge
qcewDataLouisville <- qcewDataLouisville %>% select(-1)
colnames(qcewDataLouisville)[2] <- "Current Establishments"
colnames(qcewDataLouisville)[3] <- "Current Employment"

qcewGrowthLouisville <- qcewDataLouisville_2011 %>% select(2, 6:7) 
colnames(qcewGrowthLouisville)[2] <- "2011 Establishments" 
colnames(qcewGrowthLouisville)[3] <- "2011 Employment"

qcewGrowthLouisville <- full_join(qcewGrowthLouisville, qcewDataLouisville, by = "own_code")

qcewGrowthLouisville$`Establishment Growth since 2011` <- qcewGrowthLouisville$`Current Establishments`- qcewGrowthLouisville$`2011 Establishments`
qcewGrowthLouisville$`Employment Growth since 2011` <- qcewGrowthLouisville$`Current Employment` - qcewGrowthLouisville$`2011 Employment`

qcewGrowthLouisville <- qcewGrowthLouisville %>% mutate("Ownership type" = ifelse(own_code == 0, "All", "Private"))
qcewGrowthLouisville <- qcewGrowthLouisville %>% select(9, 4:5, 7:8)

