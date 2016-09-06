library(plyr)
library(dplyr)
library(matrixStats)
library(cwhmisc)


ky         <- read.csv("ss14pky.csv") 
oh         <- read.csv("ss14poh.csv")
ind        <- read.csv("ss14pin.csv")
tn         <- read.csv("ss14ptn.csv")
socNames   <- read.csv("socCodeTitleCrosswalk.csv")
majorNames <- read.csv("fod1pNames.csv")



regionalAreaData     <- rbind(ky, oh, ind, tn)

regionalAreaData     <- regionalAreaData %>%
                            filter(FOD1P != "<NA>") %>%
                            filter(SOCP  != "<NA>") %>%
                            filter(WAGP  >   0)     %>%
                            select(FOD1P, SOCP, WAGP, PWGTP)

regionalData         <- merge(regionalAreaData, socNames, by="SOCP")

#test2 <- summaryBy(WAGP ~ title, data = test, FUN = median)
regionalData$FOD1P   <- as.factor(regionalData$FOD1P)


occupationMedians    <- ddply(regionalData, .(title), summarize, wMedian=w.median(WAGP, PWGTP))
majorsMedians        <- ddply(regionalData, .(FOD1P), summarize, wMedian=w.median(WAGP, PWGTP))

dataWithMajorMedians <- merge(regionalData, majorsMedians, by="FOD1P")
majorsData           <- merge(dataWithMajorMedians, majorNames, by="FOD1P")

majorsData           <- majorsData %>%
                            select(wMedian, majorName, PWGTP)

write.csv(majorsData, file = "majorsData.csv")
count(majorsData, majorName, wt=PWGTP)



wage                 <- c(100, 300, 550, 400, 571, 682)
weight               <- c(2, 2, 2, 7, 5, 8)
factor               <- c("One", "One", "Two", "Two", "One", "Two")
dataTest             <- as.data.frame(cbind(wage, weight, factor))

dataTest$weight      <- as.numeric(as.character(dataTest$weight))
dataTest$wage        <- as.numeric(as.character(dataTest$wage))



