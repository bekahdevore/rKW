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
mainDataFile                    <- full_join(mainDataFile, socCrosswalk,    by = 'SOC')
mainDataFile                    <- left_join(mainDataFile, sectors,         by = "SOC")

rm(burningGlassQuarter,
   emsiData, 
   sectors,
   socCrosswalk)

#Select necessary variables
mainDataFile                    <- mainDataFile %>%
                                      select(1, 12, 3, 10:11, 7, 14:15)

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

mainDataFile[is.na(mainDataFile)] <- 0
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

degreeName6   <- "Doctoral or Professional Degree"
degreeName5   <- "Master's Degree"
degreeName4   <- "BA/BS"
degreeName3   <- "Associate Degree (2 yrs)"
degreeName2.5 <- "Associate Degree (2 yrs) or Some College, no degree"
degreeName2   <- "Certificate (1-2 yrs)"
degreeName1   <- "High School or GED"

########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
# Functions
roundMean <- function(dataEntry) {
                sprintf("%.2f", round(mean(dataEntry), digits = 2))
}

formatCommas <- function(dataEntry) {
                  format(dataEntry, big.mark = ",", trim = TRUE)
}

########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################       IT          #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
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
      
      # CHANGE to roundMean function 
      itWagesBaInfoLOW <- roundMean(infoBa$Pct..25.Hourly.Earnings)
      itWagesBaProgLOW <- roundMean(progBa$Pct..25.Hourly.Earnings)
      itWagesBaNetLOW  <- roundMean(netBa$Pct..25.Hourly.Earnings)
      itWagesBaWebLOW  <- roundMean(webBa$Pct..25.Hourly.Earnings)
      
      itWagesAsInfoLOW <- roundMean(infoAs$Pct..25.Hourly.Earnings)
      itWagesAsProgLOW <- ""
      itWagesAsNetLOW  <- roundMean(netAs$Pct..25.Hourly.Earnings)
      itWagesAsWebLOW  <- roundMean(webAs$Pct..25.Hourly.Earnings)
      
      ## 75th Percentile
      itWagesBaInfoHIGH <- roundMean(infoBa$Pct..75.Hourly.Earnings)
      itWagesBaProgHIGH <- roundMean(progBa$Pct..75.Hourly.Earnings)
      itWagesBaNetHIGH  <- roundMean(netBa$Pct..75.Hourly.Earnings)
      itWagesBaWebHIGH  <- roundMean(webBa$Pct..75.Hourly.Earnings)
      
      itWagesAsInfoHIGH <- roundMean(infoAs$Pct..75.Hourly.Earnings)
      itWagesAsProgHIGH <- ""
      itWagesAsNetHIGH  <- roundMean(netAs$Pct..75.Hourly.Earnings)
      itWagesAsWebHIGH  <- roundMean(webAs$Pct..75.Hourly.Earnings)

      

########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################       LOGISTICS          #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################

      ######################## SECTOR JOBS ########################################
      logJobs <- mainDataFile %>%
        filter(Sector == "Logistics")
      
      
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      ## BACHELORS
      logJobsBa <- logJobs %>% 
                      filter(Typical.Entry.Level.Education == "Bachelor's degree")
      
      ## ASSOCIATES     
      logJobsAs <- ""

      ## CERTIFICATES    
      logJobsCe <- logJobs %>% 
                      filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      
      ## HIGH SCHOOL
      logJobsHi <- logJobs %>% 
                      filter(Typical.Entry.Level.Education == "High school diploma or equivalent")
      
      
      
      ######################## SECTOR JOBS BY COLUMN ##############################
      ################# COLUMN ONE
      ####### EDUCATION LEVEL
        ## BACHELORS
        proBa <- logJobs %>%
          filter(Category                      == 'pro') %>%
          filter(Typical.Entry.Level.Education == "Bachelor's degree")
        ## ASSOCIATES
        proAs <- ""
        ## CERTIFICATES
        proCe <- logJobs %>%
          filter(Category                      == 'pro') %>%
          filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
        ## HIGH SCHOOL
        proHi <- logJobs %>% 
          filter(Category                      == 'pro') %>%
          filter(Typical.Entry.Level.Education == "High school diploma or equivalent")  
      ################# COLUMN TWO
        ## BACHELORS
        tranBa <- logJobs %>%
          filter(Category                      == 'tran') %>%
          filter(Typical.Entry.Level.Education == "Bachelor's degree")
        ## ASSOCIATES
        tranAs <- ""     
        ## CERTIFICATE
        tranCe <- logJobs %>%
          filter(Category                      == 'tran') %>%
          filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
        ## HIGH SCHOOL 
        tranHi <- logJobs %>% 
          filter(Category                      == 'tran') %>%
          filter(Typical.Entry.Level.Education == "High school diploma or equivalent")
      ################# COLUMN THREE
        ## BACHELORS
        wareBa <- logJobs %>%
          filter(Category                       == 'ware') %>%
          filter(Typical.Entry.Level.Education  == "Bachelor's degree")
        ## ASSOCIATES
        wareAs <- ""
        ## CERTIFICATES
        wareCe <- ""
        ## HIGH SCHOOL 
        wareHi <- logJobs %>% 
          filter(Category                      == 'ware') %>%
          filter(Typical.Entry.Level.Education == "High school diploma or equivalent")  
        
      
      ######################## COLUMN ENTRY #######################################    
      ################# NAMES 
      ####### EDUCATION LEVEL
      ## BACHELORS
      logBachelorsPro    <- "Purchasing Managers; Logistics Specialists"
      logBachelorsTran   <- "Sales Managers; Industrial & Aerospace Engineers; Airline Pilots, Copilots, & Flight Engineers"
      logBachelorsWare   <- "Sales Representatives; Mechanical Engineers"
      ## ASSOCIATES
      logAssociatesPro   <- noJobsMessage
      logAssociatesTran  <- noJobsMessage
      logAssociatesWare  <- noJobsMessage
      ## CERTIFICATE
      logCertificatePro  <- noJobsMessage
      logCertificateTran <- "Tractor Trailer Truck Drivers; Aircraft Mechanics & Service Technicians"
      logCertificateWare <- noJobsMessage
      ## HIGH SCHOOL
      logHighSchoolPro   <- "Receptionists; File Clerks; Office Clerks; Customs Brokers; Purchasing Assistants; Wholesale Buyers"
      logHighSchoolTran  <- "Delivery Drivers; Import & Export Coordinators; Transportation Managers"
      logHighSchoolWare  <- "Production Supervisors; Warehouse Workers; Inventory Clerks; Warehouse Managers; Forklift Operators"

      
      ################# JOB POSTINGS
      ####### TOTALS           
      totalJobsLog           <-  sum(logJobs$Number.of.Job.Postings)
      totalJobPostingsLogBa  <-  sum(logJobsBa$Number.of.Job.Postings)
      totalJobPostingsLogAs  <-  ""
      totalJobPostingsLogCe  <-  sum(logJobsCe$Number.of.Job.Postings)
      totalJobPostingsLogHi  <-  sum(logJobsHi$Number.of.Job.Postings)
      ####### EDUCATION LEVEL
      ## BACHELORS      
      logPostingsBaPro      <-  sum(proBa$Number.of.Job.Postings)
      logPostingsBaTran     <-  sum(tranBa$Number.of.Job.Postings) 
      logPostingsBaWare     <-  sum(wareBa$Number.of.Job.Postings)
      ## ASSOCIATES  
      logPostingsAsPro      <-  ""
      logPostingsAsTran     <-  "" 
      logPostingsAsWare     <-  ""
      ## CERTIFICATES
      logPostingsCePro      <-  ""
      logPostingsCeTran     <-  sum(tranCe$Number.of.Job.Postings) 
      logPostingsCeWare     <-  ""
      ## HIGH SCHOOL
      logPostingsHiPro      <-  sum(proHi$Number.of.Job.Postings)
      logPostingsHiTran     <-  sum(tranHi$Number.of.Job.Postings) 
      logPostingsHiWare     <-  sum(wareHi$Number.of.Job.Postings)
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      logWagesBaProLOW  <- roundMean(proBa$Pct..25.Hourly.Earnings)
      logWagesBaTranLOW <- roundMean(tranBa$Pct..25.Hourly.Earnings)
      logWagesBaWareLOW <- roundMean(wareBa$Pct..25.Hourly.Earnings)
      # ASSOCIATES
      logWagesAsProLOW  <- ""
      logWagesAsTranLOW <- ""
      logWagesAsWareLOW <- ""
      # CERTIFICATES 
      logWagesCeProLOW  <- ""
      logWagesCeTranLOW <- roundMean(tranCe$Pct..25.Hourly.Earnings)
      logWagesCeWareLOW <- ""
      # HIGH SCHOOL 
      logWagesHiProLOW  <- roundMean(proHi$Pct..25.Hourly.Earnings)
      logWagesHiTranLOW <- roundMean(tranHi$Pct..25.Hourly.Earnings)
      logWagesHiWareLOW <- roundMean(wareHi$Pct..25.Hourly.Earnings)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      logWagesBaProHIGH  <- roundMean(proBa$Pct..75.Hourly.Earnings)
      logWagesBaTranHIGH <- roundMean(tranBa$Pct..75.Hourly.Earnings)
      logWagesBaWareHIGH <- roundMean(wareBa$Pct..75.Hourly.Earnings)
      ## ASSOCIATES
      logWagesAsProHIGH  <- ""
      logWagesAsTranHIGH <- ""
      logWagesAsWareHIGH <- ""
      ## CERTIFICATES
      logWagesCeProHIGH  <- ""
      logWagesCeTranHIGH <- roundMean(tranCe$Pct..75.Hourly.Earnings)
      logWagesCeWareHIGH <- ""
      ## HIGH SCHOOL 
      logWagesHiProHIGH  <- roundMean(proHi$Pct..75.Hourly.Earnings)
      logWagesHiTranHIGH <- roundMean(tranHi$Pct..75.Hourly.Earnings)
      logWagesHiWareHIGH <- roundMean(wareHi$Pct..75.Hourly.Earnings)
       

########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################       MANUFACTURING      #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################

      ######################## SECTOR JOBS ########################################
      manJobs <- mainDataFile %>%
        filter(Sector == "Manufacturing")
      
      
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      ## BACHELORS
      manJobsBa <- manJobs %>% 
        filter(Typical.Entry.Level.Education == "Bachelor's degree")
      
      ## ASSOCIATES     
      manJobsAs <- manJobs %>%
        filter(Typical.Entry.Level.Education == "Associate's degree")
      
      ## CERTIFICATES    
      manJobsCe <- manJobs %>% 
        filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      
      ## HIGH SCHOOL
      manJobsHi <- manJobs %>% 
        filter(Typical.Entry.Level.Education == "High school diploma or equivalent")
      
      
      
      ######################## SECTOR JOBS BY COLUMN ##############################
      ################# COLUMN ONE
      ####### EDUCATION LEVEL
      ## BACHELORS
      prodBa <- manJobs %>%
        filter(Category                      == 'prod') %>%
        filter(Typical.Entry.Level.Education == "Bachelor's degree")
      ## ASSOCIATES
      prodAs <- manJobs %>%
        filter(Category                      == 'prod') %>%
        filter(Typical.Entry.Level.Education == "Associate's degree")
      ## CERTIFICATES
      prodCe <- manJobs %>%
        filter(Category                      == 'prod') %>%
        filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      ## HIGH SCHOOL
      prodHi <- manJobs %>% 
        filter(Category                      == 'prod') %>%
        filter(Typical.Entry.Level.Education == "High school diploma or equivalent")  
      ################# COLUMN TWO
      ## BACHELORS
      procBa <- manJobs %>%
        filter(Category                      == 'proc') %>%
        filter(Typical.Entry.Level.Education == "Bachelor's degree")
      ## ASSOCIATES
      procAs <- manJobs %>%
        filter(Category                      == 'proc') %>%
        filter(Typical.Entry.Level.Education == "Associate's degree")    
      ## CERTIFICATE
      procCe <- manJobs %>%
        filter(Category                      == 'proc') %>%
        filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      ## HIGH SCHOOL 
      procHi <- manJobs %>% 
        filter(Category                      == 'proc') %>%
        filter(Typical.Entry.Level.Education == "High school diploma or equivalent")
      ################# COLUMN THREE
      ## BACHELORS
      qualBa <- manJobs %>%
        filter(Category                       == 'qual') %>%
        filter(Typical.Entry.Level.Education  == "Bachelor's degree")
      ## ASSOCIATES
      qualAs <- manJobs %>%
        filter(Category                      == 'qual') %>%
        filter(Typical.Entry.Level.Education == "Associate's degree")
      ## CERTIFICATES
      qualCe <- manJobs %>%
        filter(Category                      == 'qual') %>%
        filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      ## HIGH SCHOOL 
      qualHi <- manJobs %>% 
        filter(Category                      == 'qual') %>%
        filter(Typical.Entry.Level.Education == "High school diploma or equivalent")  
      ################# COLUMN FOUR
      ## BACHELORS
      mainBa <- manJobs %>%
        filter(Category                       == 'main') %>%
        filter(Typical.Entry.Level.Education  == "Bachelor's degree")
      ## ASSOCIATES
      mainAs <- manJobs %>%
        filter(Category                      == 'main') %>%
        filter(Typical.Entry.Level.Education == "Associate's degree")
      ## CERTIFICATES
      mainCe <- manJobs %>%
        filter(Category                      == 'main') %>%
        filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      ## HIGH SCHOOL 
      mainHi <- manJobs %>% 
        filter(Category                      == 'main') %>%
        filter(Typical.Entry.Level.Education == "High school diploma or equivalent")  
      
      
      ######################## COLUMN ENTRY #######################################    
      ################# NAMES 
      ####### EDUCATION LEVEL
      ## BACHELORS
      manBachelorsProd   <- "Manufacturing Engineers"
      manBachelorsProc   <- "Estimators; Sales Engineers"
      manBachelorsQual   <- "Quality Engineers; Production Mangers"
      manBachelorsMain   <- "Electrical and Mechanical Engineers" 
      
      ## ASSOCIATES
      manAssociatesProd  <- noJobsMessage
      manAssociatesProc  <- "Mechanical Drafters (Computer Aided Designers)"
      manAssociatesQual  <- "Engineering and Manufacturing Technicians"
      manAssociatesMain  <- noJobsMessage
      
      ## CERTIFICATE
      manCertificateProd <- "Production Supervisors; CNC Machine Tool Operators"
      manCertificateProc <- noJobsMessage
      manCertificateQual <- "Quality Coordinators"
      manCertificateMain <- "Welders; Industrial Machinary Mechanics; Industrial Maintenance Technicians"
      
      ## HIGH SCHOOL
      manHighSchoolProd  <- "Assembly Technicians; Industrial Tool Operators"
      manHighSchoolProc  <- noJobsMessage
      manHighSchoolQual  <- "Quality Assurance Specialist"
      manHighSchoolMain  <- "Repair Technician"
      
      ## STOPPED HERE
      ################# JOB POSTINGS
      ####### TOTALS           
      totalJobsMan           <-  sum(manJobs$Number.of.Job.Postings)
      totalJobPostingsManBa  <-  sum(manJobsBa$Number.of.Job.Postings)
      totalJobPostingsManAs  <-  ""
      totalJobPostingsManCe  <-  sum(manJobsCe$Number.of.Job.Postings)
      totalJobPostingsManHi  <-  sum(manJobsHi$Number.of.Job.Postings)
      ####### EDUCATION LEVEL
      ## BACHELORS      
      manPostingsBaProd     <-  sum(prodBa$Number.of.Job.Postings)
      manPostingsBaProc     <-  sum(tranBa$Number.of.Job.Postings) 
      manPostingsBaQual     <-  sum(wareBa$Number.of.Job.Postings)
      manPostingsBaMain     <-  sum(mainBa$Number.of.Job.Postings)
      ## ASSOCIATES  
      manPostingsAsProd     <-  ""
      manPostingsAsProc     <-  "" 
      manPostingsAsQual     <-  ""
      manPostingsAsMain     <-  ""
      ## CERTIFICATES
      manPostingsCeProd     <-  ""
      manPostingsCeProc     <-  sum(tranCe$Number.of.Job.Postings) 
      manPostingsCeQual     <-  ""
      manPostingsCeMain     <-  ""
      ## HIGH SCHOOL
      manPostingsHiProd     <-  sum(prodHi$Number.of.Job.Postings)
      manPostingsHiProc     <-  sum(tranHi$Number.of.Job.Postings) 
      manPostingsHiQual     <-  sum(wareHi$Number.of.Job.Postings)
      manPostingsHiMain     <-  sum(wareHi$Number.of.Job.Postings)
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      manWagesBaProdLOW <- roundMean(proBa$Pct..25.Hourly.Earnings)
      manWagesBaProcLOW <- roundMean(tranBa$Pct..25.Hourly.Earnings)
      manWagesBaQualLOW <- roundMean(wareBa$Pct..25.Hourly.Earnings)
      manWagesBaMainLOW <- roundMean(mainBa$Pct..25.Hourly.Earnings)
      # ASSOCIATES
      manWagesAsProdLOW <- ""
      manWagesAsProcLOW <- ""
      manWagesAsQualLOW <- ""
      manWagesAsMainLOW <- ""
      # CERTIFICATES 
      manWagesCeProdLOW <- ""
      manWagesCeProcLOW <- roundMean(tranCe$Pct..25.Hourly.Earnings)
      manWagesCeQualLOW <- ""
      manWagesCeMainLOW <- ""
      # HIGH SCHOOL 
      manWagesHiProdLOW <- roundMean(proHi$Pct..25.Hourly.Earnings)
      manWagesHiProcLOW <- roundMean(tranHi$Pct..25.Hourly.Earnings)
      manWagesHiQualLOW <- roundMean(wareHi$Pct..25.Hourly.Earnings)
      manWagesHiMainLOW <- roundMean(mainHi$Pct..25.Hourly.Earnings)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      manWagesBaProdHIGH <- roundMean(proBa$Pct..75.Hourly.Earnings)
      manWagesBaProcHIGH <- roundMean(tranBa$Pct..75.Hourly.Earnings)
      manWagesBaQualHIGH <- roundMean(wareBa$Pct..75.Hourly.Earnings)
      manWagesBaMainHIGH <- roundMean(mainBa$Pct..75.Hourly.Earnings)
      ## ASSOCIATES
      manWagesAsProdHIGH <- ""
      manWagesAsProcHIGH <- ""
      manWagesAsQualHIGH <- ""
      manWagesAsMainHIGH <- ""
      ## CERTIFICATES
      manWagesCeProdHIGH  <- ""
      manWagesCeProcHIGH <- roundMean(tranCe$Pct..75.Hourly.Earnings)
      manWagesCeQualHIGH <- ""
      manWagesCeMainHIGH <- ""
      ## HIGH SCHOOL 
      manWagesHiProdHIGH  <- roundMean(proHi$Pct..75.Hourly.Earnings)
      manWagesHiProcHIGH <- roundMean(tranHi$Pct..75.Hourly.Earnings)
      manWagesHiQualHIGH <- roundMean(wareHi$Pct..75.Hourly.Earnings)
      manWagesHiMainHIGH <- roundMean(mainHi$Pct..75.Hourly.Earnings)
            

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
                    totalJobs = formatCommas(totalJobsIt), 
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
                    
                    totalJobPostingsItBa = formatCommas(totalJobPostingsItBa),
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
                         totalJobs = formatCommas(totalJobsLog), 
                         degreeName4        = degreeName4, 
                         degreeName3        = degreeName3, 
                         degreeName2        = degreeName2, 
                         degreeName1        = degreeName1,
                         
                         # Bachelors
                         logBachelorsPro =  logBachelorsPro,
                         logBachelorsTran = logBachelorsTran,
                         logBachelorsWare  = logBachelorsWare, 
                         
                         totalJobPostingsLogBa = totalJobPostingsLogBa,
                         
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

                         
                         # Certificate
                         logCertificatePro = logCertificatePro, 
                         logCertificateTran = logCertificateTran, 
                         logCertificateWare  = logCertificateWare, 
                         
                         totalJobPostingsLogCe = formatCommas(totalJobPostingsLogCe),
                         
                         logPostingsCePro = logPostingsCePro, 
                         logPostingsCeTran = formatCommas(logPostingsCeTran), 
                         logPostingsCeWare  = logPostingsCeWare, 
                         
                         
                         logWagesCeProLOW  = logWagesCeProLOW,
                         logWagesCeProHIGH = logWagesCeProHIGH,
                         logWagesCeTranLOW  = logWagesCeTranLOW, 
                         logWagesCeTranHIGH = logWagesCeTranHIGH,
                         logWagesCeWareLOW   = logWagesCeWareLOW,
                         logWagesCeWareHIGH  = logWagesCeWareHIGH,
                         

                         
                         #High School
                         logHighSchoolPro    = logHighSchoolPro, 
                         logHighSchoolTran   = logHighSchoolTran, 
                         logHighSchoolWare   = logHighSchoolWare,
                         
                         totalJobPostingsLogHi = totalJobPostingsLogHi,
                         
                         logPostingsHiPro   = logPostingsHiPro, 
                         logPostingsHiTran  = logPostingsHiTran, 
                         logPostingsHiWare  = logPostingsHiWare, 
                         
                         
                         logWagesHiProLOW   = logWagesHiProLOW,
                         logWagesHiProHIGH  = logWagesHiProHIGH,
                         logWagesHiTranLOW  = logWagesHiTranLOW, 
                         logWagesHiTranHIGH = logWagesHiTranHIGH,
                         logWagesHiWareLOW  = logWagesHiWareLOW,
                         logWagesHiWareHIGH = logWagesHiWareHIGH
            ))
          
          output$manufacturing <- renderUI(
            htmlTemplate('manufacturingTemplate.html', 
                         totalJobs = formatCommas(totalJobsMan), 
                         degreeName4        = degreeName4, 
                         degreeName3        = degreeName3, 
                         degreeName2        = degreeName2, 
                         degreeName1        = degreeName1,
                         
                         # Bachelors
                         manBachelorsProd  = manBachelorsProd,
                         manBachelorsProc  = manBachelorsProc,
                         manBachelorsQual  = manBachelorsQual, 
                         manBachelorsMain  = manBachelorsMain, 
                         
                         totalJobPostingsManBa = totalJobPostingsManBa,
                         
                         manPostingsBaProd = manPostingsBaProd, 
                         manPostingsBaProc = manPostingsBaProc, 
                         manPostingsBaQual = manPostingsBaQual, 
                         manPostingsBaMain = manPostingsBaMain, 
                         
                         
                         manWagesBaProdLOW  = manWagesBaProdLOW,
                         manWagesBaProdHIGH = manWagesBaProdHIGH,
                         manWagesBaProcLOW  = manWagesBaProcLOW, 
                         manWagesBaProcHIGH = manWagesBaProcHIGH,
                         manWagesBaQualLOW   = manWagesBaQualLOW,
                         manWagesBaQualHIGH  = manWagesBaQualHIGH,
                         manWagesBaMainLOW   = manWagesBaMainLOW,
                         manWagesBaMainHIGH  = manWagesBaMainHIGH,                         
                         
                         # ASSOCIATES
                         manAssociatesProd = manAssociatesProd, 
                         manAssociatesProc = manAssociatesProc, 
                         manAssociatesQual  = manAssociatesQual,
                         manAssociatesMain  = manAssociatesMain,
                         
                         totalJobPostingsManAs = totalJobPostingsManAs,
                         
                         manPostingsAsProd = manPostingsAsProd, 
                         manPostingsAsProc = manPostingsAsProc, 
                         manPostingsAsQual = manPostingsAsQual, 
                         manPostingsAsMain = manPostingsAsMain, 
                         
                         
                         manWagesAsProdLOW  = manWagesAsProdLOW,
                         manWagesAsProdHIGH = manWagesAsProdHIGH,
                         manWagesAsProcLOW  = manWagesAsProcLOW, 
                         manWagesAsProcHIGH = manWagesAsProcHIGH,
                         manWagesAsQualLOW  = manWagesAsQualLOW,
                         manWagesAsQualHIGH = manWagesAsQualHIGH,
                         manWagesAsMainLOW  = manWagesAsMainLOW,
                         manWagesAsMainHIGH = manWagesAsMainHIGH,           
                         
                         
                         # Certificate
                         manCertificateProd = manCertificateProd, 
                         manCertificateProc = manCertificateProc, 
                         manCertificateQual = manCertificateQual, 
                         manCertificateMain = manCertificateMain,
                         
                         totalJobPostingsManCe = formatCommas(totalJobPostingsManCe),
                         
                         manPostingsCeProd = manPostingsCeProd, 
                         manPostingsCeProc = formatCommas(manPostingsCeProc), 
                         manPostingsCeQual = manPostingsCeQual, 
                         manPostingsCeMain = manPostingsCeMain, 
                         
                         
                         manWagesCeProdLOW  = manWagesCeProdLOW,
                         manWagesCeProdHIGH = manWagesCeProdHIGH,
                         manWagesCeProcLOW  = manWagesCeProcLOW, 
                         manWagesCeProcHIGH = manWagesCeProcHIGH,
                         manWagesCeQualLOW   = manWagesCeQualLOW,
                         manWagesCeQualHIGH  = manWagesCeQualHIGH,
                         manWagesCeMainLOW   = manWagesCeMainLOW,
                         manWagesCeMainHIGH  = manWagesCeMainHIGH,
 
                         #High School
                         manHighSchoolProd   = manHighSchoolProd, 
                         manHighSchoolProc   = manHighSchoolProc, 
                         manHighSchoolQual   = manHighSchoolQual,
                         manHighSchoolMain   = manHighSchoolMain,
                         
                         totalJobPostingsManHi = totalJobPostingsManHi,
                         
                         manPostingsHiProd  = manPostingsHiProd, 
                         manPostingsHiProc  = manPostingsHiProc, 
                         manPostingsHiQual  = manPostingsHiQual,
                         manPostingsHiMain  = manPostingsHiMain,
                         
                         manWagesHiProdLOW  = manWagesHiProdLOW,
                         manWagesHiProdHIGH = manWagesHiProdHIGH,
                         manWagesHiProcLOW  = manWagesHiProcLOW, 
                         manWagesHiProcHIGH = manWagesHiProcHIGH,
                         manWagesHiQualLOW  = manWagesHiQualLOW,
                         manWagesHiQualHIGH = manWagesHiQualHIGH, 
                         manWagesHiMainLOW  = manWagesHiMainLOW,
                         manWagesHiMainHIGH = manWagesHiMainHIGH
            ))
  })

