library(dplyr)
library(plyr)

majors <- read.csv('majors.csv')
majors$n <- 1

  
majorsData <- majors %>%
          filter(Degree   == "Bachelor's") %>%
          filter(STDMajor == "ENGINEERING, GENERAL")
majorsData$Certification <- as.character(majorsData$Certification)

majors <- dplyr::count(majorsData, Certification, n)
majors <- majors %>% select(1,3)
majors$nn <- as.numeric(as.character(majors$nn))
majors <- head(arrange(majors, desc(nn)), n = 15)
         treemap(majors,  index = 'Certification', vSize = 'nn',
                  vColor = 'Certification', 
                  title  = '')
         