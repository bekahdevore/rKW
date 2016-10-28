#Load packages
library(dplyr)
library(shiny)
library(RCurl)
library(stringr)
library(googlesheets)
library(shinythemes)

#Load data
burningGlassQuarterConnection   <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=0&single=true&output=csv')
emsiDataConnection              <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1224165436&single=true&output=csv') 
sectorsConnection               <- getURL('https://docs.google.com/spreadsheets/d/1rL0sCtUSzBbhlZYSGvUgx3fXip55o2OpMWUMK_6TKaA/pub?gid=487558132&single=true&output=csv')
socCrosswalkConnection          <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1551915918&single=true&output=csv')
#socConnection                   <- getURL('https://docs.google.com/spreadsheets/d/1wWVpXkU7OG2dGjCEEOK4Z4sS02tgK9_zee9cl0MdQRE/pub?gid=0&single=true&output=csv')

burningGlassQuarter             <- read.csv(textConnection(burningGlassQuarterConnection), check.names = FALSE)
emsiData                        <- read.csv(textConnection(emsiDataConnection),            check.names = FALSE)
sectors                         <- read.csv(textConnection(sectorsConnection))
socCrosswalk                    <- read.csv(textConnection(socCrosswalkConnection),        check.names = FALSE)
#majorSocCodeNames               <- read.csv(textConnection(socConnection))

rm(burningGlassQuarterConnection, 
   emsiDataConnection,
   sectorsConnection,
   socCrosswalkConnection)

#Merge Data
mainDataFile                    <- full_join(burningGlassQuarter, emsiData, by = 'SOC')
mainDataFile                    <- full_join(mainDataFile, socCrosswalk, by = 'SOC')
mainDataFile                    <- left_join(mainDataFile, sectors, by = "Description")

rm(burningGlassQuarter,
   emsiData, 
   sectors,
   socCrosswalk)

#Select necessary variables
mainDataFile                    <- mainDataFile %>%
                                      select(1, 12, 3, 10:11, 7, 13:14)

mainDataFile                    <- as.data.frame(lapply(mainDataFile, function(x) {
                                                            gsub(',', '', x) }))

mainDataFile                    <- as.data.frame(lapply(mainDataFile, function(x) {
                                                            gsub('\\$', '', x )}))

variables <- c('Number.of.Job.Postings' ,
               'Pct..25.Hourly.Earnings',
               'Pct..75.Hourly.Earnings')

mainDataFile[,variables] <- lapply(mainDataFile[,variables] , as.character)
mainDataFile[,variables] <- lapply(mainDataFile[,variables] , as.numeric)

mainDataFile$deduplicatedPostings <- (mainDataFile$Number.of.Job.Postings)* .8
mainDataFile$deduplicatedPostings <- round(mainDataFile$deduplicatedPostings, digits = 0)
#Split 1st 2 digits of SOC code and store in new data frame
#splitSOC                        <- as.data.frame(t(sapply(mainDataFile$SOC, 
#                                                          function(x) substring(x, 
#                                                                                first=c(1, 1), 
#                                                                                last=c(2, 7)))))

#Change column names 
#colnames(splitSOC)[1]           <- "socGroup"
#colnames(splitSOC)[2]           <- "SOC"
#colnames(majorSocCodeNames)[1]  <- 'socGroup'
#colnames(majorSocCodeNames)[2]  <- 'Occupation Group'


#Merge data with major soc group codes data frame
#mainDataFile                    <- full_join(mainDataFile, splitSOC, by = 'SOC')
#Change socGroup variable to factor 
#majorSocCodeNames$socGroup      <- as.factor(majorSocCodeNames$socGroup)
#mainDataFile$socGroup           <- as.factor(mainDataFile$socGroup)
#Join data to add major soc group names
#mainDataFile                    <- full_join(mainDataFile, majorSocCodeNames, by = 'socGroup')

###################################### IT SECTOR #######################################

######################## DATA FILTERS ########################################
techJobs <- mainDataFile %>%
               filter(Sector == "IT")

###### EDUCATION LEVEL DATA 
  ###### TOTALS BY EDUCATION LEVEL 
  techJobsBa <- techJobs %>% 
                  filter(Typical.Entry.Level.Education == "Bachelor's degree")
  
  techJobsAs <- techJobs %>%
                  filter(Typical.Entry.Level.Education == "Associate's degree"  |
                         Typical.Entry.Level.Education == 'Some college no degree')


      ##### INFORMATION SUPPORT COLUMN 
      infoBa <- techJobs %>%
                  filter(Category                      == 'informationSupport' &
                         Typical.Entry.Level.Education == "Bachelor's degree")
      
      infoAs <- techJobs %>% 
                  filter(Category                      == 'informationSupport' &
                         Typical.Entry.Level.Education == "Associate's degree" |
                         Typical.Entry.Level.Education == 'Some college no degree')             
      
      ##### PROGRAMMING & SOFTWARE DEV
      progBa <- techJobs %>%
                filter(Category                      == 'programmingSoftware' &
                       Typical.Entry.Level.Education == "Bachelor's degree")
      
      progAs <- techJobs %>% 
                filter(Category                      == 'programmingSoftware' &
                       Typical.Entry.Level.Education == "Associate's degree" |
                       Typical.Entry.Level.Education == 'Some college no degree')             
      
      
      ##### NETWORK SYSTEMS 
      netBa <- techJobs %>%
                filter(Category                       == 'networkSystems' &
                       Typical.Entry.Level.Education  == "Bachelor's degree")
      
      netAs <- techJobs %>% 
                filter(Category                       == 'networkSystems' &
                       Typical.Entry.Level.Education  == "Associate's degree" |
                       Typical.Entry.Level.Education  == 'Some college no degree')             
      
      #### WEB AND DIGITAL COMMUNICATIONS COLUMN
      webBa <- techJobs %>%
                filter(Category                      == 'webCommunications' &
                       Typical.Entry.Level.Education == "Bachelor's degree")
      webAs <- techJobs %>% 
                filter(Category                      == 'webCommunications' &
                       Typical.Entry.Level.Education == "Associate's degree")             

##################################################################################################################################################
##################################################################################################################################################
      degreeName1   <- "High School or GED"
      degreeName2   <- "Certificate (1-2 yrs)"
      degreeName2.5 <- "Associate Degree (2 yrs) or Some College, no degree"
      degreeName3   <- "Associate Degree (2 yrs)"
      degreeName4   <- "BA/BS"
      degreeName5   <- "Master's Degree"
      degreeName6   <- "Doctoral or Professional Degree"
      
########################################### COLUMN ENTRY ###########################################       
################ JOB NAMES 
      ########## BACHELORS
      itBachelorsInfo    <- " Information Technology Managers; 
                              Systems Analysts; 
                              Operations Analysts"
      itBachelorsProg    <- " Software Engineers, 
                              Systems Engineers"
      itBachelorsNet     <- " Network Specialists;
                              Database Administrators;
                              Information Security Analysts;
                              Systems Administrators"
      itBachelorsWeb     <- " Computer Programmers;
                              Graphic Designers"
      ###### ASSOCIATES
      itAssociatesInfo   <- " Technical Support Analysts;
                              Information Technology Support Technicians"
      
      itAssociatesProg   <- noJobsMessage
      itAssociatesNet    <- " Computer Network Support Specialists"
      itAssociatesWeb    <- " Web Developers"
      
      itSomeCollegeInfo  <- noJobsMessage
      itSomeCollegeProg  <- noJobsMessage
      itSomeCollegeNet   <- noJobsMessage
      itSomeCollegeWeb   <- noJobsMessage
      
      itCertificateInfo  <- noJobsMessage
      itCertificateProg  <- noJobsMessage
      itCertificateNet   <- noJobsMessage
      itCertificateWeb   <- noJobsMessage
      
      itHighSchoolInfo   <- noJobsMessage
      itHighSchoolProg   <- noJobsMessage
      itHighSchoolNet    <- noJobsMessage
      itHighSchoolWeb    <- noJobsMessage

######################## JOB POSTINGS 

      ##### TOTALS             
      totalJobsIt           <-  sum(techJobs$Number.of.Job.Postings)
      totalJobPostingsItBa  <-  sum(techJobsBa$Number.of.Job.Postings)
      totalJobPostingsItAs  <-  sum(techJobsAs$Number.of.Job.Postings)
      totalJobPostingsItSo  <-  sum(techJobsSo$Number.of.Job.Postings)

      ##### BACHELORS               
      itPostingsBaInfo      <-  sum(informationSupportBa$Number.of.Job.Postings)
      itPostingsBaProg      <-  sum(programmingBa$Number.of.Job.Postings) 
      itPostingsBaNet       <-  sum(networkBa$Number.of.Job.Postings)
      itPostingsBaWeb       <-  sum(webBa$Number.of.Job.Postings)
      
      ##### ASSOCIATES/SOME   
      itPostingsAsInfo      <-  sum(informationSupportAs$Number.of.Job.Postings)
      itPostingsAsProg      <-  sum(programmingAs$Number.of.Job.Postings) 
      itPostingsAsNet       <-  sum(networkAs$Number.of.Job.Postings)
      itPostingsAsWeb       <-  sum(weAs$Number.of.Job.Postings)



## IT Wage Ranges
## 25th Percentile      
itWagesBaInfo <- round(mean(infoBa$Pct..25.Hourly.Earnings), digits = 2)
itWagesBaProg <- round(mean(progBa$Pct..25.Hourly.Earnings), digits = 2)
itWagesBaNet  <- round(mean(progBa$Pct..25.Hourly.Earnings), digits = 2)
itWagesBaWeb  <- round(mean(progBa$Pct..25.Hourly.Earnings), digits = 2)

itWagesAsInfo <- round(mean(infoAs$Pct..25.Hourly.Earnings), digits = 2)
itWagesAsProg <- round(mean(progAs$Pct..25.Hourly.Earnings), digits = 2)
itWagesAsNet  <- round(mean(progAs$Pct..25.Hourly.Earnings), digits = 2)
itWagesAsWeb  <- round(mean(progAs$Pct..25.Hourly.Earnings), digits = 2)

## 75th Percentile
itWagesBaInfo <- round(mean(infoBa$Pct..75.Hourly.Earnings), digits = 2)
itWagesBaProg <- round(mean(progBa$Pct..75.Hourly.Earnings), digits = 2)
itWagesBaNet  <- round(mean(progBa$Pct..75.Hourly.Earnings), digits = 2)
itWagesBaWeb  <- round(mean(progBa$Pct..75.Hourly.Earnings), digits = 2)

itWagesAsInfo <- round(mean(infoAs$Pct..75.Hourly.Earnings), digits = 2)
itWagesAsProg <- round(mean(progAs$Pct..75.Hourly.Earnings), digits = 2)
itWagesAsNet  <- round(mean(progAs$Pct..75.Hourly.Earnings), digits = 2)
itWagesAsWeb  <- round(mean(progAs$Pct..75.Hourly.Earnings), digits = 2)


########### STOPPED HERE, NEED TO CHANGE NAMES ON SERVER OUTPUTS AND ON THE ITTEMPLATE.HTML

shinyServer(function(input, output) {
  #     output$healthcare <- renderUI(
  #    htmlTemplate('template.html', 
  #                 totalJobs = totalJobsHealthcare, 
  #                 column1Name = 'Direct Patient Care')
  #  )
     
  #  output$manufacturing <- renderUI(
  #     htmlTemplate('template.html', 
  #                 totalJobs = totalJobsManufacturing, 
  #                 column1Name = 'Production')
  #  )
     
  #  output$logistics <- renderUI(
  #    htmlTemplate('template.html', 
  #                 totalJobs = totalJobsManufacturing, 
  #                 column1Name = 'Production')
  #  )
     
     output$it <- renderUI(
       htmlTemplate('itTemplate.html', 
                    totalJobs = totalJobsIt, 
                    degreeName4        = degreeName4, 
                    degreeName3        = degreeName3, 
                    degreeName2.5      = degreeName2.5, 
                    degreeName2        = degreeName2, 
                    degreeName1        = degreeName1,
                    
                    itBachelorsColumn1 = itBachelorsColumn1, 
                    itBachelorsColumn2 = itBachelorsColumn2,
                    itBachelorsColumn3 = itBachelorsColumn3, 
                    itBachelorsColumn4 = itBachelorsColumn4, 
                    
                    itAssociatesColumn1 = itAssociatesColumn1, 
                    itAssociatesColumn2 = itAssociatesColumn2, 
                    itAssociatesColumn3 = itAssociatesColumn3, 
                    itAssociatesColumn4 = itAssociatesColumn4, 
                    
                    itSomeCollegeColumn1 = itSomeCollegeColumn1, 
                    itSomeCollegeColumn2 = itSomeCollegeColumn2, 
                    itSomeCollegeColumn3 = itSomeCollegeColumn3, 
                    itSomeCollegeColumn4 = itSomeCollegeColumn4, 
                    
                    itCertificateColumn1 = itCertificateColumn1, 
                    itCertificateColumn2 = itCertificateColumn2, 
                    itCertificateColumn3 = itCertificateColumn3, 
                    itCertificateColumn4 = itCertificateColumn4, 
                    
                    itHighSchoolColumn1  = itHighSchoolColumn1, 
                    itHighSchoolColumn2  = itHighSchoolColumn2, 
                    itHighSchoolColumn3  = itHighSchoolColumn3, 
                    itHighSchoolColumn4  = itHighSchoolColumn4)
     )
     
     #     output$foodAndBeverage <- renderUI(
     # htmlTemplate('template.html', 
     #              totalJobs = totalJobsManufacturing, 
     #              column1Name = 'Production')
     #)
     
#     output$business <- renderUI(
#       htmlTemplate('template.html', 
     #                    totalJobs = totalJobsManufacturing, 
     #              column1Name = 'Production')
     # )
     
     
     
     
     
     
     
     

  })

