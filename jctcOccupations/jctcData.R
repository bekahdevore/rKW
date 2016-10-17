library(dplyr)
library(stringr)

socCodes             <- c("15-1134", "15-1151", "15-1151", "15-1152","17-3023", "17-3024", "17-3026", "17-3013", 
                            "49-2011", "49-2094", "49-2094", "49-2094", "49-3023", "51-4121",
                            "49-9012", "49-9041", "49-9043", "49-9044", "49-9071", "49-9071",
                            "51-4011", "51-4041", "51-4071", "51-4081", "51-4111", "51-4122")

burningGlassDataJCTC <- read.csv('burningGlassDataJCTC.csv', check.names = FALSE)
emsiDataJCTC         <- read.csv('emsiDataJCTC.csv', check.names = FALSE)


burningGlassDataJCTC <- burningGlassDataJCTC %>%
                            filter(SOC %in% socCodes) 

jctcData <- left_join(emsiDataJCTC, burningGlassDataJCTC, by = 'SOC')
jctcData <- jctcData %>%
              select(1:10, 12)


jctcData$`Age 55-64`  <-str_replace_all(jctcData$`Age 55-64`, 
                                                   '<10', '5')
jctcData$`Age 65+`    <-str_replace_all(jctcData$`Age 65+`, 
                                                   '<10', '5')
jctcData$`Median Hourly Earnings`  <- str_replace_all(jctcData$`Median Hourly Earnings`, 
                                          '\\$', '')
jctcData$`Age 55-64`  <-as.numeric(str_replace_all(jctcData$`Age 55-64`, 
                                                                '\\,', ''))
jctcData$`Age 65+`    <-as.numeric(str_replace_all(jctcData$`Age 65+`, 
                                                                '\\,', ''))
jctcData$`2016 Jobs`               <-as.numeric(str_replace_all(jctcData$`2016 Jobs`, 
                                           '\\,', ''))
jctcData$`2018 Jobs`               <-as.numeric(str_replace_all(jctcData$`2018 Jobs`, 
                                          '\\,', ''))
jctcData$`2021 Jobs`               <-as.numeric(str_replace_all(jctcData$`2021 Jobs`, 
                                          '\\,', ''))
jctcData$`Number of Job Postings`  <-as.numeric(str_replace_all(jctcData$`Number of Job Postings`, 
                                                                '\\,', ''))

jctcData$retirement <- (jctcData$`Age 55-64`) + (jctcData$`Age 65+`)
jctcData$retirement <- as.numeric(as.character(jctcData$retirement))
jctcData$`2018 - 2021 Change` <- as.numeric(as.character(jctcData$`2018 - 2021 Change`))
jctcData$jobsAdded <- (jctcData$`2018 - 2021 Change`) + (jctcData$retirement)
jctcData <- jctcData %>%
              select(1:2, 11, 3:6, 13)

colnames(jctcData)[3] <- "Number of Job Postings (Oct. 2015 - Sept. 2016)"
colnames(jctcData)[5] <- "2018 Jobs (Projected)"
colnames(jctcData)[6] <- "2021 Jobs (Projected)"
colnames(jctcData)[8] <- "Job Growth + Retirements (projected by 2021)"

