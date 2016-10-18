library(dplyr)
library(DT)

emsi         <- read.csv('emsiData.csv')
burningGlass <- read.csv('burningGlassData.csv')
onTheJob     <- read.csv('onTheJobTraining.csv')

burningGlass$SOC        <-  burningGlass$SOC.Code

occupationEducationData <- inner_join(emsi, burningGlass, by = 'SOC')
occupationEducationData <- inner_join(occupationEducationData, onTheJob, by = 'SOC')



occupationEducationData <- occupationEducationData %>%
                                   select(1:4, 8, 11, 5:6, 22, 7)

colnames(occupationEducationData)[2]  <- 'Occupation'
colnames(occupationEducationData)[3]  <- '2016 Jobs'
colnames(occupationEducationData)[4]  <- '2018 Jobs'
colnames(occupationEducationData)[5]  <- 'Regional Completions (2015)'
colnames(occupationEducationData)[6]  <- 'Job Postings'
colnames(occupationEducationData)[7]  <- 'Typical Entry Level Education'
colnames(occupationEducationData)[8]  <- 'Work Experience Required'
colnames(occupationEducationData)[9]  <- 'Typical on the Job Training'
colnames(occupationEducationData)[10] <- 'Median Earnings'

datatable(occupationEducationData)

