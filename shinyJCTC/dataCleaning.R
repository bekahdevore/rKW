library(dplyr)
library(RCurl)


originalDataConnection <- getURL('https://docs.google.com/spreadsheets/d/15lJU7mouxsQH9f09gLNMRCuL0FCIVUF0xfWqsYybgRs/pub?gid=0&single=true&output=csv')
originalData           <- read.csv(textConnection(originalDataConnection), check.names = FALSE)

variables <- c("startYearJobs", 
               "endYearJobs")

originalData[,variables] <- round(originalData[,variables], 0)
originalData[,"percentChange"] <- paste(round(100* originalData[,"percentChange"], 2), "%")


projected <- originalData %>% filter(group == "projected16_2026")
historic  <- originalData %>% filter(group == "historicChange06_16")

topProjected  <- projected %>% top_n(10, change) %>% arrange(-change)
lossProjected <- projected %>% top_n(-5, change) %>% arrange(change)
historicLoss  <- historic  %>% top_n(-8, change) %>% arrange(change)

historicAndProjectedLoss <- merge(historicLoss, lossProjected, by = "Description")

colnames(topProjected)[3] <- "2016 Jobs"
colnames(topProjected)[4] <- "2026 Jobs"
colnames(topProjected)[5] <- "2016 - 2026 Change"
colnames(topProjected)[6] <- "Percent Change"
colnames(topProjected)[2] <- "Industry"

topProjected <- topProjected %>% select(-group, -1)
#write.csv(topProjected, file = "topProjected.csv")

colnames(historicAndProjectedLoss)[5]  <- "Decline 2006 - 2016"
colnames(historicAndProjectedLoss)[11] <- "Forecast Decline 2016 - 2026"
colnames(historicAndProjectedLoss)[1]  <- "Industry"
historicAndProjectedLoss               <- historicAndProjectedLoss %>% select(1,5,11)
#write.csv(historicAndProjectedLoss, file = "historicAndProjectedLoss.csv")
 