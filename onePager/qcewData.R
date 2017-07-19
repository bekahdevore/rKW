### BLS  QCEW DATA 
qcewDataLouisville_2011 <- read.csv("2011_LouisvilleMSA.csv")
qcewDataKentucky_2011 <- read.csv("2011_Kentucky.csv") 

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

colnames(privateQcewData)[1] <- "Quarterly Establishments (private)"
colnames(allQcewData)[1] <- "Quarterly Establishments (all)"

colnames(privateQcewData)[2] <- "Employment (private)"
colnames(allQcewData)[2] <- "Employment (all)"

allCurrentQcewData <- left_join(privateQcewData, allQcewData, by = "MSA") %>% select(3, 1, 4, 2, 5)
