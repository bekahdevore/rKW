library(dplyr)
library(plotly)
library(RCurl)
library(ggplot2)


dataConnection <- getURL('https://docs.google.com/spreadsheets/d/1WzX4fMNuQKEoCfAUwaC-SOwtE4Ax63gySeWwaXBk8Cs/pub?gid=1285744002&single=true&output=csv')

highDemandData <- read.csv(textConnection(dataConnection))
highDemandData$Job.Postings <- as.numeric(as.factor(highDemandData$Job.Postings))

p <- plotly(highDemandData, x = ~Occupation, 
                       y = ~Job.Postings, 
                         type = 'bar') %>%
                        add_trace(y = )

