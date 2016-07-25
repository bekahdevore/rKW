library(plyr)
library(dplyr)
library(matrixStats)


ky       <- read.csv("ss14pky.csv") 
oh       <- read.csv("ss14poh.csv")
ind      <- read.csv("ss14pin.csv")
tn       <- read.csv("ss14ptn.csv")
socNames <- read.csv("socCodeTitleCrosswalk.csv")

regionalAreaData <- rbind(ky, oh, ind, tn)

regionalAreaData <- regionalAreaData %>%
                            filter(FOD1P != "<NA>") %>%
                            filter(SOCP  != "<NA>") %>%
                            filter(WAGP  >   0)     %>%
                            select(FOD1P, SOCP, WAGP, PWGTP)

test <- merge(regionalAreaData, socNames, by="SOCP")
#test2 <- summaryBy(WAGP ~ title, data = test, FUN = median)


test3 <- weightedMedian(test$WAGP, w=test$PWGTP, idxs=test$title, na.rm = TRUE)

testing <- ddply(test, .(title), summarize, wMedian=weightedMedian(WAGP, title))


