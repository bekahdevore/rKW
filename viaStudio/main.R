library(RCurl)
library(dplyr)

dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1JyhbM8P8vWzX_V2bnZYGPHAX7zuT523Wwm7UB-xSvNQ/pub?gid=860354194&single=true&output=csv")
cipCodesData <- read.csv(textConnection(dataConnection), check.names = FALSE) 

cipCodesData <- cipCodesData %>% mutate(numberLength = nchar(cipCodesData$`Academic Major CIP Code`)) %>% filter(numberLength < 8)
cipCodesData[1:3,1] <- paste0("0", cipCodesData[1:3,1])



cipCodesData <- cipCodesData %>% select(1:3)
write.csv(cipCodesData, file = "cipCodesData.csv")
