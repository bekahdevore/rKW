library(dplyr)

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
              select(SOC, `Description`, `Number of Job Postings`, 
                     `2016 Jobs`, `2018 Jobs`, `2021 Jobs`, `Median Hourly Earnings`)

jctcData$`Median Hourly Earnings`  <- str_replace_all(jctcData$`Median Hourly Earnings`, 
                                          '\\$', '')
jctcData$`Number of Job Postings`  <-as.numeric(str_replace_all(jctcData$`Number of Job Postings`, 
                                          '\\,', ''))
jctcData$`2016 Jobs`               <-as.numeric(str_replace_all(jctcData$`2016 Jobs`, 
                                                                '\\,', ''))
jctcData$`2018 Jobs`               <-as.numeric(str_replace_all(jctcData$`2018 Jobs`, 
                                          '\\,', ''))
jctcData$`2021 Jobs`               <-as.numeric(str_replace_all(jctcData$`2021 Jobs`, 
                                          '\\,', ''))


     





