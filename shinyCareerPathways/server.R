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

noJobsMessage <- "No high-demand jobs at this level within the pathway"

##################################################################################################################################################
##################################################################################################################################################
degreeName1   <- "High School or GED"
degreeName2   <- "Certificate (1-2 yrs)"
degreeName2.5 <- "Associate Degree (2 yrs) or Some College, no degree"
degreeName3   <- "Associate Degree (2 yrs)"
degreeName4   <- "BA/BS"
degreeName5   <- "Master's Degree"
degreeName6   <- "Doctoral or Professional Degree"



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
                  filter(Category                      == 'informationSupport') %>%
                  filter(Typical.Entry.Level.Education == "Bachelor's degree")
      
      infoAs <- techJobs %>% 
                  filter(Category                      == 'informationSupport') %>%
                  filter(Typical.Entry.Level.Education == "Associate's degree" |
                         Typical.Entry.Level.Education == 'Some college no degree')             
      
      ##### PROGRAMMING & SOFTWARE DEV
      progBa <- techJobs %>%
                filter(Category                      == 'programmingSoftware') %>%
                filter(Typical.Entry.Level.Education == "Bachelor's degree")
      
      progAs <- ""             
      
      
      ##### NETWORK SYSTEMS 
      netBa <- techJobs %>%
                filter(Category                       == 'networkSystems') %>%
                filter(Typical.Entry.Level.Education  == "Bachelor's degree")
      
      netAs <- techJobs %>% 
                filter(Category == 'networkSystems') %>%
                filter(Typical.Entry.Level.Education  == "Associate's degree" |
                       Typical.Entry.Level.Education  == 'Some college no degree')             
      
      #### WEB AND DIGITAL COMMUNICATIONS COLUMN
      webBa <- techJobs %>%
                filter(Category                      == 'webCommunications') %>%
                filter(Typical.Entry.Level.Education == "Bachelor's degree")
                
      webAs <- techJobs %>% 
                filter(Category                      == 'webCommunications') %>%
                filter(Typical.Entry.Level.Education == "Associate's degree" |
                       Typical.Entry.Level.Education == 'Some college no degree')             


      
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


      ##### BACHELORS               
      itPostingsBaInfo      <-  sum(infoBa$Number.of.Job.Postings)
      itPostingsBaProg      <-  sum(progBa$Number.of.Job.Postings) 
      itPostingsBaNet       <-  sum(netBa$Number.of.Job.Postings)
      itPostingsBaWeb       <-  sum(webBa$Number.of.Job.Postings)
      
      ##### ASSOCIATES/SOME   
      itPostingsAsInfo      <-  sum(infoAs$Number.of.Job.Postings)
      itPostingsAsProg      <-  "" 
      itPostingsAsNet       <-  sum(netAs$Number.of.Job.Postings)
      itPostingsAsWeb       <-  sum(webAs$Number.of.Job.Postings)



## IT Wage Ranges
## 25th Percentile      
itWagesBaInfoLOW <- round(mean(infoBa$Pct..25.Hourly.Earnings), digits = 2)
itWagesBaProgLOW <- round(mean(progBa$Pct..25.Hourly.Earnings), digits = 2)
itWagesBaNetLOW  <- round(mean(netBa$Pct..25.Hourly.Earnings), digits = 2)
itWagesBaWebLOW  <- round(mean(webBa$Pct..25.Hourly.Earnings), digits = 2)

itWagesAsInfoLOW <- round(mean(infoAs$Pct..25.Hourly.Earnings), digits = 2)
itWagesAsProgLOW <- ""
itWagesAsNetLOW  <- round(mean(netAs$Pct..25.Hourly.Earnings), digits = 2)
itWagesAsWebLOW  <- round(mean(webAs$Pct..25.Hourly.Earnings), digits = 2)

## 75th Percentile
itWagesBaInfoHIGH <- round(mean(infoBa$Pct..75.Hourly.Earnings), digits = 2)
itWagesBaProgHIGH <- round(mean(progBa$Pct..75.Hourly.Earnings), digits = 2)
itWagesBaNetHIGH  <- round(mean(netBa$Pct..75.Hourly.Earnings), digits = 2)
itWagesBaWebHIGH  <- round(mean(webBa$Pct..75.Hourly.Earnings), digits = 2)

itWagesAsInfoHIGH <- round(mean(infoAs$Pct..75.Hourly.Earnings), digits = 2)
itWagesAsProgHIGH <- ""
itWagesAsNetHIGH  <- round(mean(netAs$Pct..75.Hourly.Earnings), digits = 2)
itWagesAsWebHIGH  <- round(mean(webAs$Pct..75.Hourly.Earnings), digits = 2)


########### STOPPED HERE, NEED TO CHANGE NAMES ON SERVER OUTPUTS AND ON THE ITTEMPLATE.HTML

shinyServer(function(input, output) {
  #     output$healthcare <- renderUI(
  #    htmlTemplate('template.html', 
  #                 totalJobs = totalJobsHealthcare, 
  #                 InfoName = 'Direct Patient Care')
  #  )
     
  #  output$manufacturing <- renderUI(
  #     htmlTemplate('template.html', 
  #                 totalJobs = totalJobsManufacturing, 
  #                 InfoName = 'Production')
  #  )
     
  #  output$logistics <- renderUI(
  #    htmlTemplate('template.html', 
  #                 totalJobs = totalJobsManufacturing, 
  #                 InfoName = 'Production')
  #  )
     
     output$it <- renderUI(
       htmlTemplate('itTemplate.html', 
                    totalJobs = totalJobsIt, 
                    degreeName4        = degreeName4, 
                    degreeName3        = degreeName3, 
                    degreeName2.5      = degreeName2.5, 
                    degreeName2        = degreeName2, 
                    degreeName1        = degreeName1,
                    
                    # Bachelors
                    itBachelorsInfo = itBachelorsInfo,
                    itBachelorsProg = itBachelorsProg,
                    itBachelorsNet  = itBachelorsNet, 
                    itBachelorsWeb  = itBachelorsWeb,
                    
                    totalJobPostingsItBa = totalJobPostingsItBa,
                    itPostingsBaInfo = itPostingsBaInfo, 
                    itPostingsBaProg = itPostingsBaProg, 
                    itPostingsBaNet  = itPostingsBaNet, 
                    itPostingsBaWeb  = itPostingsBaWeb, 
                    
                    itWagesBaInfoLOW  = itWagesBaInfoLOW,
                    itWagesBaInfoHIGH = itWagesBaInfoHIGH,
                    itWagesBaProgLOW  = itWagesBaProgLOW, 
                    itWagesBaProgHIGH = itWagesBaProgHIGH,
                    itWagesBaNetLOW   = itWagesBaNetLOW,
                    itWagesBaNetHIGH  = itWagesBaNetHIGH,
                    itWagesBaWebLOW   = itWagesBaWebLOW,
                    itWagesBaWebHIGH  = itWagesBaWebHIGH,
                    
                    # ASSOCIATES
                    itAssociatesInfo = itAssociatesInfo, 
                    itAssociatesProg = itAssociatesProg, 
                    itAssociatesNet  = itAssociatesNet, 
                    itAssociatesWeb  = itAssociatesWeb, 
                    
                    totalJobPostingsItAs = totalJobPostingsItAs,
                    itPostingsAsInfo = itPostingsAsInfo, 
                    itPostingsAsProg = itPostingsAsProg, 
                    itPostingsAsNet  = itPostingsAsNet, 
                    itPostingsAsWeb  = itPostingsAsWeb, 
                    
                    itWagesAsInfoLOW  = itWagesAsInfoLOW,
                    itWagesAsInfoHIGH = itWagesAsInfoHIGH,
                    itWagesAsProgLOW  = itWagesAsProgLOW, 
                    itWagesAsProgHIGH = itWagesAsProgHIGH,
                    itWagesAsNetLOW   = itWagesAsNetLOW,
                    itWagesAsNetHIGH  = itWagesAsNetHIGH,
                    itWagesAsWebLOW   = itWagesAsWebLOW,
                    itWagesAsWebHIGH  = itWagesAsWebHIGH,
                    
                    # Certificate
                    itCertificateInfo = itCertificateInfo, 
                    itCertificateProg = itCertificateProg, 
                    itCertificateNet  = itCertificateNet, 
                    itCertificateWeb  = itCertificateWeb, 
                    
                    #High School
                    itHighSchoolInfo  = itHighSchoolInfo, 
                    itHighSchoolProg  = itHighSchoolProg, 
                    itHighSchoolNet   = itHighSchoolNet, 
                    itHighSchoolWeb   = itHighSchoolWeb)
     )
     
     #     output$foodAndBeverage <- renderUI(
     # htmlTemplate('template.html', 
     #              totalJobs = totalJobsManufacturing, 
     #              InfoName = 'Production')
     #)
     
#     output$business <- renderUI(
#       htmlTemplate('template.html', 
     #                    totalJobs = totalJobsManufacturing, 
     #              InfoName = 'Production')
     # )
     
          output$logistics <- renderUI(
            htmlTemplate('logisticsTemplate.html', 
                         totalJobs = totalJobsIt, 
                         degreeName4        = degreeName4, 
                         degreeName3        = degreeName3, 
                         degreeName2.5      = degreeName2.5, 
                         degreeName2        = degreeName2, 
                         degreeName1        = degreeName1,
                         
                         # Bachelors
                         logBachelorsPro = logBachelorsPro,
                         logBachelorsTran = logBachelorsTran,
                         logBachelorsWare  = logBachelorsWare, 
                         
                         totalJobPostingslogBa = totalJobPostingslogBa,
                         
                         logPostingsBaPro = logPostingsBaPro, 
                         logPostingsBaTran = logPostingsBaTran, 
                         logPostingsBaWare  = logPostingsBaWare, 

                         
                         logWagesBaProLOW  = logWagesBaProLOW,
                         logWagesBaProHIGH = logWagesBaProHIGH,
                         logWagesBaTranLOW  = logWagesBaTranLOW, 
                         logWagesBaTranHIGH = logWagesBaTranHIGH,
                         logWagesBaWareLOW   = logWagesBaWareLOW,
                         logWagesBaWareHIGH  = logWagesBaWareHIGH,
                         
                         
                         # ASSOCIATES
                         logAssociatesPro = logAssociatesPro, 
                         logAssociatesTran = logAssociatesTran, 
                         logAssociatesWare  = logAssociatesWare, 
                         
                         totalJobPostingslogAs = totalJobPostingslogAs,
                         
                         logPostingsAsPro = logPostingsAsPro, 
                         logPostingsAsTran = logPostingsAsTran, 
                         logPostingsAsWare  = logPostingsAsWare, 

                         
                         logWagesAsProLOW  = logWagesAsProLOW,
                         logWagesAsProHIGH = logWagesAsProHIGH,
                         logWagesAsTranLOW  = logWagesAsTranLOW, 
                         logWagesAsTranHIGH = logWagesAsTranHIGH,
                         logWagesAsWareLOW   = logWagesAsWareLOW,
                         logWagesAsWareHIGH  = logWagesAsWareHIGH,

                         
                         # Certificate
                         logCertificatePro = logCertificatePro, 
                         logCertificateTran = logCertificateTran, 
                         logCertificateWare  = logCertificateWare, 

                         
                         #High School
                         logHighSchoolPro  = logHighSchoolPro, 
                         logHighSchoolTran  = logHighSchoolTran, 
                         logHighSchoolWare   = logHighSchoolWare 

     )
     
     
     
     
     
     
     
     

  })

