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
degreeName0   <- "Less than High School"

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

categoryFilter <- function(dataNameEd, category){dataNameEd %>%
                    filter(Category  == category)
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
      infoBa <- categoryFilter(techJobsBa, 'informationSupport')
      infoAs <- categoryFilter(techJobsAs, 'informationSupport')
      
      ##### PROGRAMMING & SOFTWARE DEV
      progBa <- categoryFilter(techJobsBa, 'programmingSoftware')
      progAs <- ""             
      ##### NETWORK SYSTEMS 
      #netBa <- techJobs %>%
       #         filter(Category                       == 'networkSystems') %>%
        #        filter(Typical.Entry.Level.Education  == "Bachelor's degree")
      netBa <- categoryFilter(techJobsBa, 'networkSystems')
      netAs <- categoryFilter(techJobsAs, 'networkSystems')
      #### WEB AND DIGITAL COMMUNICATIONS COLUMN
      webBa <- categoryFilter(techJobsBa, 'webCommunications')
      webAs <- categoryFilter(techJobsAs, 'webCommunications')
 
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
      ################# COLUMN 
      ####### EDUCATION LEVEL
      ## ONE
        proBa <- categoryFilter(logJobsBa, 'pro')
        proAs <- ""
        proCe <- categoryFilter(logJobsCe, 'pro')
        proHi <- categoryFilter(logJobsHi, 'pro')
      ## TWO
        tranBa <- categoryFilter(logJobsBa, 'tran')
        tranAs <- ""     
        tranCe <- categoryFilter(logJobsCe, 'tran')
        tranHi <- categoryFilter(logJobsHi, 'tran')
      ## THREE
        wareBa <- categoryFilter(logJobsBa, 'ware')
        wareAs <- ""
        wareCe <- ""
        wareHi <- categoryFilter(logJobsHi, 'ware')
        
      
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
      ################# COLUMN 
      ####### EDUCATION LEVEL
      
      ## ONE
      prodBa <- categoryFilter(manJobsBa, 'prod')
      prodAs <- categoryFilter(manJobsAs, 'prod')
      prodCe <- categoryFilter(manJobsCe, 'prod')
      prodHi <- categoryFilter(manJobsHi, 'prod')
      ## TWO
      procBa <- categoryFilter(manJobsBa, 'proc')
      procAs <- categoryFilter(manJobsAs, 'proc')
      procCe <- categoryFilter(manJobsCe, 'proc')
      procHi <- categoryFilter(manJobsHi, 'proc')
      ## THREE
      qualBa <- categoryFilter(manJobsBa, 'qual')
      qualAs <- categoryFilter(manJobsAs, 'qual')
      qualCe <- categoryFilter(manJobsCe, 'qual')
      qualHi <- categoryFilter(manJobsHi, 'qual')
      ## FOUR
      mainBa <- categoryFilter(manJobsBa, 'main')
      mainAs <- categoryFilter(manJobsAs, 'main')
      mainCe <- categoryFilter(manJobsCe, 'main')
      mainHi <- categoryFilter(manJobsHi, 'main')
      
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
      

  ########################################################################################################################################################################################################################
  ########################################################################################################################################################################################################################
  #####################################       BUSINESS     #################################################################################################################################################################
  ########################################################################################################################################################################################################################
  ########################################################################################################################################################################################################################
  
      ######################## SECTOR JOBS ########################################
      busJobs <- mainDataFile %>%
        filter(Sector == "Business")
      
      
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      ## MASTERS/DOCTORAL/PROFESSIONAL
      busJobsMa <- busJobs %>% 
        filter(Typical.Entry.Level.Education == "Master's degree"                 |
               Typical.Entry.Level.Education == "Doctoral or professional degree") 
      ## BACHELORS
      busJobsBa <- busJobs %>% 
        filter(Typical.Entry.Level.Education == "Bachelor's degree")
      
      ## ASSOCIATES     
      busJobsAs <- busJobs %>%
        filter(Typical.Entry.Level.Education == "Associate's degree")
      
      ## CERTIFICATES    
      busJobsCe <- busJobs %>% 
        filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      
      ## HIGH SCHOOL
      busJobsHi <- busJobs %>% 
        filter(Typical.Entry.Level.Education == "High school diploma or equivalent")
      
      
      
      ######################## SECTOR JOBS BY COLUMN ##############################
      ################# COLUMN
      ####### EDUCATION LEVEL
      ## ONE
      finMa <- categoryFilter(busJobsMa, 'fin')
      finBa <- categoryFilter(busJobsBa, 'fin')
      finAs <- categoryFilter(busJobsAs, 'fin')
      finCe <- categoryFilter(busJobsCe, 'fin')
      finHi <- categoryFilter(busJobsHi, 'fin')
      ## TWO
      legMa <- categoryFilter(busJobsMa, 'leg')
      legBa <- categoryFilter(busJobsBa, 'leg')
      legAs <- categoryFilter(busJobsAs, 'leg')   
      legCe <- categoryFilter(busJobsCe, 'leg')
      legHi <- categoryFilter(busJobsHi, 'leg')
      ## THREE
      advMa <- categoryFilter(busJobsMa, 'adv')
      advBa <- categoryFilter(busJobsBa, 'adv')
      advAs <- categoryFilter(busJobsAs, 'adv')
      advCe <- categoryFilter(busJobsCe, 'adv')
      advHi <- categoryFilter(busJobsHi, 'adv')


      ######################## COLUMN ENTRY #######################################    
      ################# NAMES 
      ####### EDUCATION LEVEL
      ## MASTERS
      busMastersFin    <- noJobsMessage
      busMastersLeg    <- "Judges; Lawyers"
      busMastersAdv    <- noJobsMessage

      ## BACHELORS
      busBachelorsFin   <- "Financial Managers; Accountants; Budget Analysts; Credit Analysts; Personal Financial Advisors; Business Bankers"
      busBachelorsLeg   <- "Management Consultant"
      busBachelorsAdv   <- "Marketing, Sales, and Public Relations Managers; Fundraising and Marketing Coordinators; Videographers; Editors; Graphic Designers; Animators; Communications Specialists; Technical Writers; Copywriters"
      
      ## ASSOCIATES
      busAssociatesFin  <- noJobsMessage
      busAssociatesLeg  <- "Paralegals and Legal Assistants"
      busAssociatesAdv  <- "Executive Assistants"
      
      ## CERTIFICATE
      busCertificateFin <- noJobsMessage
      busCertificateLeg <- noJobsMessage
      busCertificateAdv <- "Audio and Video Equiptment Technicians"
      
      ## HIGH SCHOOL
      busHighSchoolFin  <- "Collections and Billing Specialists; Bookkeepers; Correspondence Clerks; Credit Assistants"
      busHighSchoolLeg  <- "Legal Administrative Assistants; Receptionists"
      busHighSchoolAdv  <- "Office Assistants; Artists; Media and Communication Assistants; Photographers; Secretaries and Administrative Assistants"
      
      ################# JOB POSTINGS
      ####### TOTALS           
      totalJobsBus           <-  sum(busJobs$Number.of.Job.Postings)
      totalJobPostingsBusMa  <-  sum(busJobsMa$Number.of.Job.Postings)
      totalJobPostingsBusBa  <-  sum(busJobsBa$Number.of.Job.Postings)
      totalJobPostingsBusAs  <-  ""
      totalJobPostingsBusCe  <-  sum(busJobsCe$Number.of.Job.Postings)
      totalJobPostingsBusHi  <-  sum(busJobsHi$Number.of.Job.Postings)
      ####### EDUCATION LEVEL
      ## MASTERS
      busPostingsMaFin     <-  sum(finMa$Number.of.Job.Postings)
      busPostingsMaLeg     <-  sum(legMa$Number.of.Job.Postings)
      busPostingsMaAdv     <-  sum(advMa$Number.of.Job.Postings)
      ## BACHELORS      
      busPostingsBaFin     <-  sum(finBa$Number.of.Job.Postings)
      busPostingsBaLeg     <-  sum(legBa$Number.of.Job.Postings) 
      busPostingsBaAdv     <-  sum(advBa$Number.of.Job.Postings)
      ## ASSOCIATES  
      busPostingsAsFin     <-  ""
      busPostingsAsLeg     <-  "" 
      busPostingsAsAdv     <-  ""
      ## CERTIFICATES
      busPostingsCeFin     <-  sum(finCe$Number.of.Job.Postings)
      busPostingsCeLeg     <-  sum(legCe$Number.of.Job.Postings) 
      busPostingsCeAdv     <-  ""
      ## HIGH SCHOOL
      busPostingsHiFin     <-  sum(finHi$Number.of.Job.Postings)
      busPostingsHiLeg     <-  sum(legHi$Number.of.Job.Postings) 
      busPostingsHiAdv     <-  sum(advHi$Number.of.Job.Postings)
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # MASTERS
      busWagesMaFinLOW <- roundMean(finMa$Pct..25.Hourly.Earnings)
      busWagesMaLegLOW <- roundMean(legMa$Pct..25.Hourly.Earnings)
      busWagesMaAdvLOW <- roundMean(advMa$Pct..25.Hourly.Earnings)
      # BACHELORS
      busWagesBaFinLOW <- roundMean(finBa$Pct..25.Hourly.Earnings)
      busWagesBaLegLOW <- roundMean(legBa$Pct..25.Hourly.Earnings)
      busWagesBaAdvLOW <- roundMean(advBa$Pct..25.Hourly.Earnings)
      # ASSOCIATES
      busWagesAsFinLOW <- ""
      busWagesAsLegLOW <- ""
      busWagesAsAdvLOW <- ""
      # CERTIFICATES 
      busWagesCeFinLOW <- ""
      busWagesCeLegLOW <- roundMean(legCe$Pct..25.Hourly.Earnings)
      busWagesCeAdvLOW <- ""
      # HIGH SCHOOL 
      busWagesHiFinLOW <- roundMean(finHi$Pct..25.Hourly.Earnings)
      busWagesHiLegLOW <- roundMean(legHi$Pct..25.Hourly.Earnings)
      busWagesHiAdvLOW <- roundMean(advHi$Pct..25.Hourly.Earnings)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # MASTERS
      busWagesMaFinHIGH <- roundMean(finMa$Pct..75.Hourly.Earnings)
      busWagesMaLegHIGH <- roundMean(legMa$Pct..75.Hourly.Earnings)
      busWagesMaAdvHIGH <- roundMean(advMa$Pct..75.Hourly.Earnings)
      # BACHELORS
      busWagesBaFinHIGH <- roundMean(finBa$Pct..75.Hourly.Earnings)
      busWagesBaLegHIGH <- roundMean(legBa$Pct..75.Hourly.Earnings)
      busWagesBaAdvHIGH <- roundMean(advBa$Pct..75.Hourly.Earnings)
      ## ASSOCIATES
      busWagesAsFinHIGH <- ""
      busWagesAsLegHIGH <- ""
      busWagesAsAdvHIGH <- ""
      ## CERTIFICATES
      busWagesCeFinHIGH  <- ""
      busWagesCeLegHIGH <- roundMean(legCe$Pct..75.Hourly.Earnings)
      busWagesCeAdvHIGH <- ""
      ## HIGH SCHOOL 
      busWagesHiFinHIGH  <- roundMean(finHi$Pct..75.Hourly.Earnings)
      busWagesHiLegHIGH <- roundMean(legHi$Pct..75.Hourly.Earnings)
      busWagesHiAdvHIGH <- roundMean(advHi$Pct..75.Hourly.Earnings)
      
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################       FOOD AND BEVERAGE      #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################

      ######################## SECTOR JOBS ########################################
      foodJobs <- mainDataFile %>%
        filter(Sector == "Food")
      
      
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      ## BACHELORS
      foodJobsBa <- foodJobs %>% 
        filter(Typical.Entry.Level.Education == "Bachelor's degree")
      
      ## ASSOCIATES     
      foodJobsAs <- foodJobs %>%
        filter(Typical.Entry.Level.Education == "Associate's degree")
      
      ## CERTIFICATES    
      foodJobsCe <- foodJobs %>% 
        filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      
      ## HIGH SCHOOL
      foodJobsHi <- foodJobs %>% 
        filter(Typical.Entry.Level.Education == "High school diploma or equivalent")
      
      ## LESS THAN HIGH SCHOOL
      foodJobsNo <- foodJobs %>% 
        filter(Typical.Entry.Level.Education == "No formal educational credential")
      
      ######################## SECTOR JOBS BY COLUMN ##############################
      ################# COLUMN 
      ####### EDUCATION LEVEL
      ## ONE
      prodBa <- categoryFilter(foodJobsBa, 'prod')
      prodAs <- categoryFilter(foodJobsAs, 'prod')
      prodCe <- categoryFilter(foodJobsCe, 'prod')
      prodHi <- categoryFilter(foodJobsHi, 'prod')
      prodNo <- categoryFilter(foodJobsNo, 'prod')
      ## TWO
      restBa <- categoryFilter(foodJobsBa, 'rest')
      restAs <- categoryFilter(foodJobsAs, 'rest')
      restCe <- categoryFilter(foodJobsCe, 'rest')
      restHi <- categoryFilter(foodJobsHi, 'rest')
      restNo <- categoryFilter(foodJobsNo, 'rest')
      ## THREE
      hospBa <- categoryFilter(foodJobsBa, 'hosp')
      hospAs <- categoryFilter(foodJobsAs, 'hosp')
      hospCe <- categoryFilter(foodJobsCe, 'hosp')
      hospHi <- categoryFilter(foodJobsHi, 'hosp')
      hospNo <- categoryFilter(foodJobsNo, 'hosp')
      ## FOUR
      corpBa <- categoryFilter(foodJobsBa, 'corp')
      corpAs <- categoryFilter(foodJobsAs, 'corp')
      corpCe <- categoryFilter(foodJobsCe, 'corp')
      corpHi <- categoryFilter(foodJobsHi, 'corp')
      corpNo <- categoryFilter(foodJobsNo, 'corp')
      
      ######################## COLUMN ENTRY #######################################    
      ################# NAMES 
      ####### EDUCATION LEVEL
      ## BACHELORS
      foodBachelorsProd   <- noJobsMessage
      foodBachelorsRest   <- "General Managers"
      foodBachelorsHosp   <- "Sales Managers; Marketing Coordinators"
      foodBachelorsCorp   <- "Human Resources Specialists; Financial Analysts; Quality Assurance Analysts; Application Developers; Marketing Managers; Accountants; Network Engineers" 
      
      ## ASSOCIATES
      foodAssociatesProd  <- noJobsMessage
      foodAssociatesRest  <- noJobsMessage
      foodAssociatesHosp  <- noJobsMessage
      foodAssociatesCorp  <- noJobsMessage
      
      ## CERTIFICATE
      foodCertificateProd <- "Shift Coordinators; Tractor-Trailer Truck Drivers"
      foodCertificateRest <- noJobsMessage
      foodCertificateHosp <- noJobsMessage
      foodCertificateCorp <- noJobsMessage
      
      ## HIGH SCHOOL
      foodHighSchoolProd  <- "Maintenance Technicians; Deliverary Drivers; Production Associates"
      foodHighSchoolRest  <- "Shift Mangers; Assistant Managers; Chefs"
      foodHighSchoolHosp  <- "Front Desk Agents; Housekeeping Supervisors; Security Officers"
      foodHighSchoolCorp  <- "Accounting Clerks; Front Office Supervisors; Customer Service Reps; Telephone Operators"
      
      ## HIGH SCHOOL
      foodNoSchoolProd  <- "Order Fillers; Bakers"
      foodNoSchoolRest  <- "Restaurant Team Members; Servers; Grill, Line and Prep Cooks; Cashiers; Bartenders; Hosts"
      foodNoSchoolHosp  <- "Housekeepers; Janitors; Valet Attendants"
      foodNoSchoolCorp  <- noJobsMessage
      
      
      ################# JOB POSTINGS
      ####### TOTALS           
      totalJobsFood           <-  sum(foodJobs$Number.of.Job.Postings)
      totalJobPostingsFoodBa  <-  sum(foodJobsBa$Number.of.Job.Postings)
      totalJobPostingsFoodAs  <-  ""
      totalJobPostingsFoodCe  <-  sum(foodJobsCe$Number.of.Job.Postings)
      totalJobPostingsFoodHi  <-  sum(foodJobsHi$Number.of.Job.Postings)
      totalJobPostingsFoodNo  <-  sum(foodJobsNo$Number.of.Job.Postings)
      ####### EDUCATION LEVEL
      ## BACHELORS      
      foodPostingsBaProd     <-  sum(prodBa$Number.of.Job.Postings)
      foodPostingsBaRest     <-  sum(restBa$Number.of.Job.Postings) 
      foodPostingsBaHosp     <-  sum(hospBa$Number.of.Job.Postings)
      foodPostingsBaCorp     <-  sum(corpBa$Number.of.Job.Postings)
      ## ASSOCIATES  
      foodPostingsAsProd     <-  ""
      foodPostingsAsRest     <-  "" 
      foodPostingsAsHosp     <-  ""
      foodPostingsAsCorp     <-  ""
      ## CERTIFICATES
      foodPostingsCeProd     <-  sum(prodCe$Number.of.Job.Postings) 
      foodPostingsCeRest     <-  ""
      foodPostingsCeHosp     <-  ""
      foodPostingsCeCorp     <-  ""
      ## HIGH SCHOOL
      foodPostingsHiProd     <-  sum(prodHi$Number.of.Job.Postings)
      foodPostingsHiRest     <-  sum(restHi$Number.of.Job.Postings) 
      foodPostingsHiHosp     <-  sum(hospHi$Number.of.Job.Postings)
      foodPostingsHiCorp     <-  sum(corpHi$Number.of.Job.Postings)
      ## LESS THAN HIGH SCHOOL
      foodPostingsNoProd     <-  sum(prodNo$Number.of.Job.Postings)
      foodPostingsNoRest     <-  sum(restNo$Number.of.Job.Postings) 
      foodPostingsNoHosp     <-  sum(hospNo$Number.of.Job.Postings)
      foodPostingsNoCorp     <-  ""
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      foodWagesBaProdLOW <- roundMean(proBa$Pct..25.Hourly.Earnings)
      foodWagesBaRestLOW <- roundMean(restBa$Pct..25.Hourly.Earnings)
      foodWagesBaHospLOW <- roundMean(hospBa$Pct..25.Hourly.Earnings)
      foodWagesBaCorpLOW <- roundMean(corpBa$Pct..25.Hourly.Earnings)
      # ASSOCIATES
      foodWagesAsProdLOW <- ""
      foodWagesAsRestLOW <- ""
      foodWagesAsHospLOW <- ""
      foodWagesAsCorpLOW <- ""
      # CERTIFICATES 
      foodWagesCeProdLOW <- roundMean(prodCe$Pct..25.Hourly.Earnings)
      foodWagesCeRestLOW <- ""
      foodWagesCeHospLOW <- ""
      foodWagesCeCorpLOW <- ""
      # HIGH SCHOOL 
      foodWagesHiProdLOW <- roundMean(proHi$Pct..25.Hourly.Earnings)
      foodWagesHiRestLOW <- roundMean(restHi$Pct..25.Hourly.Earnings)
      foodWagesHiHospLOW <- roundMean(hospHi$Pct..25.Hourly.Earnings)
      foodWagesHiCorpLOW <- roundMean(corpHi$Pct..25.Hourly.Earnings)
      # LESS THAN HIGH SCHOOL 
      foodWagesNoProdLOW <- roundMean(prodNo$Pct..25.Hourly.Earnings)
      foodWagesNoRestLOW <- roundMean(restNo$Pct..25.Hourly.Earnings)
      foodWagesNoHospLOW <- roundMean(hospNo$Pct..25.Hourly.Earnings)
      foodWagesNoCorpLOW <- ""
      
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      foodWagesBaProdHIGH <- roundMean(proBa$Pct..75.Hourly.Earnings)
      foodWagesBaRestHIGH <- roundMean(restBa$Pct..75.Hourly.Earnings)
      foodWagesBaHospHIGH <- roundMean(hospBa$Pct..75.Hourly.Earnings)
      foodWagesBaCorpHIGH <- roundMean(corpBa$Pct..75.Hourly.Earnings)
      ## ASSOCIATES
      foodWagesAsProdHIGH <- ""
      foodWagesAsRestHIGH <- ""
      foodWagesAsHospHIGH <- ""
      foodWagesAsCorpHIGH <- ""
      ## CERTIFICATES
      foodWagesCeProdHIGH <- roundMean(prodCe$Pct..75.Hourly.Earnings)
      foodWagesCeRestHIGH <- ""
      foodWagesCeHospHIGH <- ""
      foodWagesCeCorpHIGH <- ""
      ## HIGH SCHOOL 
      foodWagesHiProdHIGH <- roundMean(prodHi$Pct..75.Hourly.Earnings)
      foodWagesHiRestHIGH <- roundMean(restHi$Pct..75.Hourly.Earnings)
      foodWagesHiHospHIGH <- roundMean(hospHi$Pct..75.Hourly.Earnings)
      foodWagesHiCorpHIGH <- roundMean(corpHi$Pct..75.Hourly.Earnings)
      ## HIGH SCHOOL 
      foodWagesNoProdHIGH <- roundMean(prodNo$Pct..75.Hourly.Earnings)
      foodWagesNoRestHIGH <- roundMean(restNo$Pct..75.Hourly.Earnings)
      foodWagesNoHospHIGH <- roundMean(hospNo$Pct..75.Hourly.Earnings)
      foodWagesNoCorpHIGH <- ""


      ########################################################################################################################################################################################################################
      ########################################################################################################################################################################################################################
      #####################################       Healthcare    #################################################################################################################################################################
      ########################################################################################################################################################################################################################
      ########################################################################################################################################################################################################################
      
      ######################## SECTOR JOBS ########################################
      healthJobs <- mainDataFile %>%
        filter(Sector == "Healthcare")
      
      
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      ## DOCTORAL/PROFESSIONAL
      healthJobsDo <- healthJobs %>% filter(Typical.Entry.Level.Education == "Doctoral or professional degree") 
      ## MASTERS
      healthJobsMa <- healthJobs %>% filter(Typical.Entry.Level.Education == "Master's degree") 
      ## BACHELORS
      healthJobsBa <- healthJobs %>% filter(Typical.Entry.Level.Education == "Bachelor's degree")
      ## ASSOCIATES     
      healthJobsAs <- healthJobs %>% filter(Typical.Entry.Level.Education == "Associate's degree")
      ## CERTIFICATES    
      healthJobsCe <- healthJobs %>% filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")
      ## HIGH SCHOOL
      healthJobsHi <- healthJobs %>% filter(Typical.Entry.Level.Education == "High school diploma or equivalent")

      
      ######################## SECTOR JOBS BY COLUMN ##############################
      ################# COLUMN
      ####### EDUCATION LEVEL
      ## ONE
      direDo <- categoryFilter(healthJobsDo, 'dire')
      direMa <- categoryFilter(healthJobsMa, 'dire')
      direBa <- categoryFilter(healthJobsBa, 'dire')
      direAs <- categoryFilter(healthJobsAs, 'dire')
      direCe <- categoryFilter(healthJobsCe, 'dire')
      direHi <- categoryFilter(healthJobsHi, 'dire')
      ## TWO
      diagDo <- categoryFilter(healthJobsDo, 'diag')
      diagMa <- categoryFilter(healthJobsMa, 'diag')
      diagBa <- categoryFilter(healthJobsBa, 'diag')
      diagAs <- categoryFilter(healthJobsAs, 'diag')   
      diagCe <- categoryFilter(healthJobsCe, 'diag')
      diagHi <- categoryFilter(healthJobsHi, 'diag')
      ## THREE
      admiDo <- categoryFilter(healthJobsDo, 'admi')
      admiMa <- categoryFilter(healthJobsMa, 'admi')
      admiBa <- categoryFilter(healthJobsBa, 'admi')
      admiAs <- categoryFilter(healthJobsAs, 'admi')
      admiCe <- categoryFilter(healthJobsCe, 'admi')
      admiHi <- categoryFilter(healthJobsHi, 'admi')
      
      
      ######################## COLUMN ENTRY #######################################    
      ################# NAMES 
      ####### EDUCATION LEVEL
      ## DOCTORAL 
      healthDoctoralDire    <- "Physicians and Surgeons; Physical Therapists; Audiologists; Family and General Practitioners"
      healthDoctoralDiag    <- "Dentists; Orthodontists; Pharmacists; Optometrists"
      healthDoctoralAdmi    <- "Healthcare Lawyers"
      
      ## MASTERS
      healthMastersDire    <- "Nurse Practitioners; Occupational Therapists; Physicians Assistants; Nurse Anesthetists; Speech-Language Pathologists"
      healthMastersDiag    <- "Mental Health Counselors"
      healthMastersAdmi    <- "Healthcare Social Workers; Statisticians"
      
      ## BACHELORS
      healthBachelorsDire   <- "Directors of Nursing"
      healthBachelorsDiag   <- "Medical and Clinical Laboratory Technicians; Dieticians and Nutritionists"
      healthBachelorsAdmi   <- "Systems and Accounting Analysts; Business Office Managers; Human Resources Specialists; Human Resources and Marketing Managers; Marketing Coordinators; Information Technology Managers; Network Engineer; Risk and Financial Analysts"
      ## ASSOCIATES
      healthAssociatesDire  <- "Registered Nurses; Physical and Occupational Therapist Assistants; Respiratory Therapists; Sonographers; Cardiovascular Technicians"
      healthAssociatesDiag  <- "Medical and Clinical Laboratory Technicians; Dental Hygientists; Radiologic Technologists"
      healthAssociatesAdmi  <- noJobsMessage
      
      ## CERTIFICATE
      healthCertificateDire <- "Nursing Assistants; Emergency Technicians and Paramedics; Medical Assistants; Licensed Nurses"
      healthCertificateDiag <- "Dental Assistants; Surgical Technologists; Phlebotomists; Ophthamlmic Medical Technicians"
      healthCertificateAdmi <- "Medical Transcriptionists; Medical Records Clerks"
      
      ## HIGH SCHOOL
      healthHighSchoolDire  <- "Physical Therapists Aides; Patient Transporters; Sterile Processing Technicians; Endoscopy Technicians; Emergency Room Technicians"
      healthHighSchoolDiag  <- "Pharmacy Clerks; Pharmacy Technicians; Opticians; Social Workers; Home Health Aides; Dietary Cooks; Caregivers"
      healthHighSchoolAdmi  <- "Medical Receptionists; Executive Assistants; Bookkeepers; Telephone Operators; Office Managers; Customer Service Reps"
      
      ################# JOB POSTINGS
      ####### TOTALS           
      totalJobsHealth           <-  sum(healthJobs$Number.of.Job.Postings)
      totalJobPostingsHealthDo  <-  sum(healthJobsDo$Number.of.Job.Postings)
      totalJobPostingsHealthMa  <-  sum(healthJobsMa$Number.of.Job.Postings)
      totalJobPostingsHealthBa  <-  sum(healthJobsBa$Number.of.Job.Postings)
      totalJobPostingsHealthAs  <-  sum(healthJobsAs$Number.of.Job.Postings)
      totalJobPostingsHealthCe  <-  sum(healthJobsCe$Number.of.Job.Postings)
      totalJobPostingsHealthHi  <-  sum(healthJobsHi$Number.of.Job.Postings)
      ####### EDUCATION LEVEL
      ## DOCTORAL 
      healthPostingsDoDire     <-  sum(direDo$Number.of.Job.Postings)
      healthPostingsDoDiag     <-  sum(diagDo$Number.of.Job.Postings)
      healthPostingsDoAdmi     <-  sum(admiDo$Number.of.Job.Postings)
      ## MASTERS
      healthPostingsMaDire     <-  sum(direMa$Number.of.Job.Postings)
      healthPostingsMaDiag     <-  sum(diagMa$Number.of.Job.Postings)
      healthPostingsMaAdmi     <-  sum(admiMa$Number.of.Job.Postings)
      ## BACHELORS      
      healthPostingsBaDire     <-  sum(direBa$Number.of.Job.Postings)
      healthPostingsBaDiag     <-  sum(diagBa$Number.of.Job.Postings) 
      healthPostingsBaAdmi     <-  sum(admiBa$Number.of.Job.Postings)
      ## ASSOCIATES  
      healthPostingsAsDire     <-  sum(direAs$Number.of.Job.Postings)
      healthPostingsAsDiag     <-  sum(diagAs$Number.of.Job.Postings) 
      healthPostingsAsAdmi     <-  ""
      ## CERTIFICATES
      healthPostingsCeDire     <-  sum(direCe$Number.of.Job.Postings)
      healthPostingsCeDiag     <-  sum(diagCe$Number.of.Job.Postings) 
      healthPostingsCeAdmi     <-  sum(admiCe$Number.of.Job.Postings)
      ## HIGH SCHOOL
      healthPostingsHiDire     <-  sum(direHi$Number.of.Job.Postings)
      healthPostingsHiDiag     <-  sum(diagHi$Number.of.Job.Postings) 
      healthPostingsHiAdmi     <-  sum(admiHi$Number.of.Job.Postings)
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # MASTERS
      healthWagesDoDireLOW <- roundMean(direDo$Pct..25.Hourly.Earnings)
      healthWagesDoDiagLOW <- roundMean(diagDo$Pct..25.Hourly.Earnings)
      healthWagesDoAdmiLOW <- roundMean(admiDo$Pct..25.Hourly.Earnings)
      # MASTERS
      healthWagesMaDireLOW <- roundMean(direMa$Pct..25.Hourly.Earnings)
      healthWagesMaDiagLOW <- roundMean(diagMa$Pct..25.Hourly.Earnings)
      healthWagesMaAdmiLOW <- roundMean(admiMa$Pct..25.Hourly.Earnings)
      # BACHELORS
      healthWagesBaDireLOW <- roundMean(direBa$Pct..25.Hourly.Earnings)
      healthWagesBaDiagLOW <- roundMean(diagBa$Pct..25.Hourly.Earnings)
      healthWagesBaAdmiLOW <- roundMean(admiBa$Pct..25.Hourly.Earnings)
      # ASSOCIATES
      healthWagesAsDireLOW <- roundMean(direAs$Pct..25.Hourly.Earnings)
      healthWagesAsDiagLOW <- roundMean(diagAs$Pct..25.Hourly.Earnings)
      healthWagesAsAdmiLOW <- ""
      # CERTIFICATES 
      healthWagesCeDireLOW <- roundMean(direCe$Pct..25.Hourly.Earnings)
      healthWagesCeDiagLOW <- roundMean(diagCe$Pct..25.Hourly.Earnings)
      healthWagesCeAdmiLOW <- roundMean(admiCe$Pct..25.Hourly.Earnings)
      # HIGH SCHOOL 
      healthWagesHiDireLOW <- roundMean(direHi$Pct..25.Hourly.Earnings)
      healthWagesHiDiagLOW <- roundMean(diagHi$Pct..25.Hourly.Earnings)
      healthWagesHiAdmiLOW <- roundMean(admiHi$Pct..25.Hourly.Earnings)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # MASTERS
      healthWagesDoDireHIGH <- roundMean(direDo$Pct..75.Hourly.Earnings)
      healthWagesDoDiagHIGH <- roundMean(diagDo$Pct..75.Hourly.Earnings)
      healthWagesDoAdmiHIGH <- roundMean(admiDo$Pct..75.Hourly.Earnings)
      # MASTERS
      healthWagesMaDireHIGH <- roundMean(direMa$Pct..75.Hourly.Earnings)
      healthWagesMaDiagHIGH <- roundMean(diagMa$Pct..75.Hourly.Earnings)
      healthWagesMaAdmiHIGH <- roundMean(admiMa$Pct..75.Hourly.Earnings)
      # BACHELORS
      healthWagesBaDireHIGH <- roundMean(direBa$Pct..75.Hourly.Earnings)
      healthWagesBaDiagHIGH <- roundMean(diagBa$Pct..75.Hourly.Earnings)
      healthWagesBaAdmiHIGH <- roundMean(admiBa$Pct..75.Hourly.Earnings)
      ## ASSOCIATES
      healthWagesAsDireHIGH <- roundMean(direAs$Pct..75.Hourly.Earnings)
      healthWagesAsDiagHIGH <- roundMean(diagAs$Pct..75.Hourly.Earnings)
      healthWagesAsAdmiHIGH <- ""
      ## CERTIFICATES
      healthWagesCeDireHIGH <- roundMean(direCe$Pct..75.Hourly.Earnings)
      healthWagesCeDiagHIGH <- roundMean(diagCe$Pct..75.Hourly.Earnings)
      healthWagesCeAdmiHIGH <- roundMean(admiCe$Pct..75.Hourly.Earnings)
      ## HIGH SCHOOL 
      healthWagesHiDireHIGH <- roundMean(direHi$Pct..75.Hourly.Earnings)
      healthWagesHiDiagHIGH <- roundMean(diagHi$Pct..75.Hourly.Earnings)
      healthWagesHiAdmiHIGH <- roundMean(admiHi$Pct..75.Hourly.Earnings)
      


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
          
          output$business <- renderUI(
            htmlTemplate('businessTemplate.html', 
                         totalJobs = formatCommas(totalJobsBus), 
                         degreeName4        = degreeName4, 
                         degreeName3        = degreeName3, 
                         degreeName2        = degreeName2, 
                         degreeName1        = degreeName1,
                         
                         # MASTERS
                         busMastersFin  = busMastersFin,
                         busMastersLeg  = busMastersLeg,
                         busMastersAdv  = busMastersAdv, 
                         
                         totalJobPostingsBusMa = totalJobPostingsBusMa,
                         
                         busPostingsMaFin = busPostingsMaFin, 
                         busPostingsMaLeg = busPostingsMaLeg, 
                         busPostingsMaAdv = busPostingsMaAdv, 
                         
                         
                         busWagesMaFinLOW  = busWagesMaFinLOW,
                         busWagesMaFinHIGH = busWagesMaFinHIGH,
                         busWagesMaLegLOW  = busWagesMaLegLOW, 
                         busWagesMaLegHIGH = busWagesMaLegHIGH,
                         busWagesMaAdvLOW  = busWagesMaAdvLOW,
                         busWagesMaAdvHIGH = busWagesMaAdvHIGH,
                         
                         # Bachelors
                         busBachelorsFin  = busBachelorsFin,
                         busBachelorsLeg  = busBachelorsLeg,
                         busBachelorsAdv  = busBachelorsAdv, 
                         
                         totalJobPostingsBusBa = totalJobPostingsBusBa,
                         
                         busPostingsBaFin = busPostingsBaFin, 
                         busPostingsBaLeg = busPostingsBaLeg, 
                         busPostingsBaAdv = busPostingsBaAdv, 
                         
                         
                         busWagesBaFinLOW  = busWagesBaFinLOW,
                         busWagesBaFinHIGH = busWagesBaFinHIGH,
                         busWagesBaLegLOW  = busWagesBaLegLOW, 
                         busWagesBaLegHIGH = busWagesBaLegHIGH,
                         busWagesBaAdvLOW   = busWagesBaAdvLOW,
                         busWagesBaAdvHIGH  = busWagesBaAdvHIGH,
                         # ASSOCIATES
                         busAssociatesFin = busAssociatesFin, 
                         busAssociatesLeg = busAssociatesLeg, 
                         busAssociatesAdv  = busAssociatesAdv,
                         
                         totalJobPostingsBusAs = totalJobPostingsBusAs,
                         
                         busPostingsAsFin = busPostingsAsFin, 
                         busPostingsAsLeg = busPostingsAsLeg, 
                         busPostingsAsAdv = busPostingsAsAdv, 
                         
                         
                         busWagesAsFinLOW  = busWagesAsFinLOW,
                         busWagesAsFinHIGH = busWagesAsFinHIGH,
                         busWagesAsLegLOW  = busWagesAsLegLOW, 
                         busWagesAsLegHIGH = busWagesAsLegHIGH,
                         busWagesAsAdvLOW  = busWagesAsAdvLOW,
                         busWagesAsAdvHIGH = busWagesAsAdvHIGH,
                         
                         
                         # Certificate
                         busCertificateFin = busCertificateFin, 
                         busCertificateLeg = busCertificateLeg, 
                         busCertificateAdv = busCertificateAdv, 
                         
                         totalJobPostingsBusCe = formatCommas(totalJobPostingsBusCe),
                         
                         busPostingsCeFin = busPostingsCeFin, 
                         busPostingsCeLeg = formatCommas(busPostingsCeLeg), 
                         busPostingsCeAdv = busPostingsCeAdv,
                         
                         busWagesCeFinLOW  = busWagesCeFinLOW,
                         busWagesCeFinHIGH = busWagesCeFinHIGH,
                         busWagesCeLegLOW  = busWagesCeLegLOW, 
                         busWagesCeLegHIGH = busWagesCeLegHIGH,
                         busWagesCeAdvLOW  = busWagesCeAdvLOW,
                         busWagesCeAdvHIGH = busWagesCeAdvHIGH,

                         
                         #High School
                         busHighSchoolFin  = busHighSchoolFin, 
                         busHighSchoolLeg  = busHighSchoolLeg, 
                         busHighSchoolAdv  = busHighSchoolAdv,
                         
                         totalJobPostingsBusHi = totalJobPostingsBusHi,
                         
                         busPostingsHiFin  = busPostingsHiFin, 
                         busPostingsHiLeg  = busPostingsHiLeg, 
                         busPostingsHiAdv  = busPostingsHiAdv,
                         
                         busWagesHiFinLOW  = busWagesHiFinLOW,
                         busWagesHiFinHIGH = busWagesHiFinHIGH,
                         busWagesHiLegLOW  = busWagesHiLegLOW, 
                         busWagesHiLegHIGH = busWagesHiLegHIGH,
                         busWagesHiAdvLOW  = busWagesHiAdvLOW,
                         busWagesHiAdvHIGH = busWagesHiAdvHIGH
            ))

          output$food <- renderUI(
            htmlTemplate('foodTemplate.html', 
                         totalJobs = formatCommas(totalJobsFood), 
                         degreeName4        = degreeName4, 
                         degreeName3        = degreeName3, 
                         degreeName2        = degreeName2, 
                         degreeName1        = degreeName1,
                         degreeName0        = degreeName0,
                         
                         # Bachelors
                         foodBachelorsProd  = foodBachelorsProd,
                         foodBachelorsRest  = foodBachelorsRest,
                         foodBachelorsHosp  = foodBachelorsHosp, 
                         foodBachelorsCorp  = foodBachelorsCorp, 
                         
                         totalJobPostingsFoodBa = totalJobPostingsFoodBa,
                         
                         foodPostingsBaProd = foodPostingsBaProd, 
                         foodPostingsBaRest = foodPostingsBaRest, 
                         foodPostingsBaHosp = foodPostingsBaHosp, 
                         foodPostingsBaCorp = foodPostingsBaCorp, 
                         
                         
                         foodWagesBaProdLOW  = foodWagesBaProdLOW,
                         foodWagesBaProdHIGH = foodWagesBaProdHIGH,
                         foodWagesBaRestLOW  = foodWagesBaRestLOW, 
                         foodWagesBaRestHIGH = foodWagesBaRestHIGH,
                         foodWagesBaHospLOW   = foodWagesBaHospLOW,
                         foodWagesBaHospHIGH  = foodWagesBaHospHIGH,
                         foodWagesBaCorpLOW   = foodWagesBaCorpLOW,
                         foodWagesBaCorpHIGH  = foodWagesBaCorpHIGH,                         
                         
                         # ASSOCIATES
                         foodAssociatesProd = foodAssociatesProd, 
                         foodAssociatesRest = foodAssociatesRest, 
                         foodAssociatesHosp  = foodAssociatesHosp,
                         foodAssociatesCorp  = foodAssociatesCorp,
                         
                         totalJobPostingsFoodAs = totalJobPostingsFoodAs,
                         
                         foodPostingsAsProd = foodPostingsAsProd, 
                         foodPostingsAsRest = foodPostingsAsRest, 
                         foodPostingsAsHosp = foodPostingsAsHosp, 
                         foodPostingsAsCorp = foodPostingsAsCorp, 
                         
                         
                         foodWagesAsProdLOW  = foodWagesAsProdLOW,
                         foodWagesAsProdHIGH = foodWagesAsProdHIGH,
                         foodWagesAsRestLOW  = foodWagesAsRestLOW, 
                         foodWagesAsRestHIGH = foodWagesAsRestHIGH,
                         foodWagesAsHospLOW  = foodWagesAsHospLOW,
                         foodWagesAsHospHIGH = foodWagesAsHospHIGH,
                         foodWagesAsCorpLOW  = foodWagesAsCorpLOW,
                         foodWagesAsCorpHIGH = foodWagesAsCorpHIGH,           
                         
                         
                         # Certificate
                         foodCertificateProd = foodCertificateProd, 
                         foodCertificateRest = foodCertificateRest, 
                         foodCertificateHosp = foodCertificateHosp, 
                         foodCertificateCorp = foodCertificateCorp,
                         
                         totalJobPostingsFoodCe = formatCommas(totalJobPostingsFoodCe),
                         
                         foodPostingsCeProd = foodPostingsCeProd, 
                         foodPostingsCeRest = formatCommas(foodPostingsCeRest), 
                         foodPostingsCeHosp = foodPostingsCeHosp, 
                         foodPostingsCeCorp = foodPostingsCeCorp, 
                         
                         
                         foodWagesCeProdLOW  = foodWagesCeProdLOW,
                         foodWagesCeProdHIGH = foodWagesCeProdHIGH,
                         foodWagesCeRestLOW  = foodWagesCeRestLOW, 
                         foodWagesCeRestHIGH = foodWagesCeRestHIGH,
                         foodWagesCeHospLOW   = foodWagesCeHospLOW,
                         foodWagesCeHospHIGH  = foodWagesCeHospHIGH,
                         foodWagesCeCorpLOW   = foodWagesCeCorpLOW,
                         foodWagesCeCorpHIGH  = foodWagesCeCorpHIGH,
                         
                         #High School
                         foodHighSchoolProd   = foodHighSchoolProd, 
                         foodHighSchoolRest   = foodHighSchoolRest, 
                         foodHighSchoolHosp   = foodHighSchoolHosp,
                         foodHighSchoolCorp   = foodHighSchoolCorp,
                         
                         totalJobPostingsFoodHi = totalJobPostingsFoodHi,
                         
                         foodPostingsHiProd  = foodPostingsHiProd, 
                         foodPostingsHiRest  = foodPostingsHiRest, 
                         foodPostingsHiHosp  = foodPostingsHiHosp,
                         foodPostingsHiCorp  = foodPostingsHiCorp,
                         
                         foodWagesHiProdLOW  = foodWagesHiProdLOW,
                         foodWagesHiProdHIGH = foodWagesHiProdHIGH,
                         foodWagesHiRestLOW  = foodWagesHiRestLOW, 
                         foodWagesHiRestHIGH = foodWagesHiRestHIGH,
                         foodWagesHiHospLOW  = foodWagesHiHospLOW,
                         foodWagesHiHospHIGH = foodWagesHiHospHIGH, 
                         foodWagesHiCorpLOW  = foodWagesHiCorpLOW,
                         foodWagesHiCorpHIGH = foodWagesHiCorpHIGH,
                         
                         # LESS THAN HIGH SCHOOL
                         foodNoSchoolProd   = foodNoSchoolProd, 
                         foodNoSchoolRest   = foodNoSchoolRest, 
                         foodNoSchoolHosp   = foodNoSchoolHosp,
                         foodNoSchoolCorp   = foodNoSchoolCorp,
                         
                         totalJobPostingsFoodNo = totalJobPostingsFoodNo,
                         
                         foodPostingsNoProd  = foodPostingsNoProd, 
                         foodPostingsNoRest  = foodPostingsNoRest, 
                         foodPostingsNoHosp  = foodPostingsNoHosp,
                         foodPostingsNoCorp  = foodPostingsNoCorp,
                         
                         foodWagesNoProdLOW  = foodWagesNoProdLOW,
                         foodWagesNoProdHIGH = foodWagesNoProdHIGH,
                         foodWagesNoRestLOW  = foodWagesNoRestLOW, 
                         foodWagesNoRestHIGH = foodWagesNoRestHIGH,
                         foodWagesNoHospLOW  = foodWagesNoHospLOW,
                         foodWagesNoHospHIGH = foodWagesNoHospHIGH, 
                         foodWagesNoCorpLOW  = foodWagesNoCorpLOW,
                         foodWagesNoCorpHIGH = foodWagesNoCorpHIGH
            ))
          
          output$healthcare <- renderUI(
            htmlTemplate('healthcareTemplate.html', 
                         totalJobs = formatCommas(totalJobsHealth),
                         degreeName6        = degreeName6,
                         degreeName5        = degreeName5,
                         degreeName4        = degreeName4, 
                         degreeName3        = degreeName3, 
                         degreeName2        = degreeName2, 
                         degreeName1        = degreeName1,
                         
                         # DOCTORAL 
                         healthDoctoralDire  = healthDoctoralDire,
                         healthDoctoralDiag  = healthDoctoralDiag,
                         healthDoctoralAdmi  = healthDoctoralAdmi, 
                         
                         totalJobPostingsHealthDo = totalJobPostingsHealthDo,
                         
                         healthPostingsDoDire = healthPostingsDoDire, 
                         healthPostingsDoDiag = healthPostingsDoDiag, 
                         healthPostingsDoAdmi = healthPostingsDoAdmi, 
                         
                         
                         healthWagesDoDireLOW  = healthWagesDoDireLOW,
                         healthWagesDoDireHIGH = healthWagesDoDireHIGH,
                         healthWagesDoDiagLOW  = healthWagesDoDiagLOW, 
                         healthWagesDoDiagHIGH = healthWagesDoDiagHIGH,
                         healthWagesDoAdmiLOW  = healthWagesDoAdmiLOW,
                         healthWagesDoAdmiHIGH = healthWagesDoAdmiHIGH,
                         
                         # MASTERS
                         healthMastersDire  = healthMastersDire,
                         healthMastersDiag  = healthMastersDiag,
                         healthMastersAdmi  = healthMastersAdmi, 
                         
                         totalJobPostingsHealthMa = totalJobPostingsHealthMa,
                         
                         healthPostingsMaDire = healthPostingsMaDire, 
                         healthPostingsMaDiag = healthPostingsMaDiag, 
                         healthPostingsMaAdmi = healthPostingsMaAdmi, 
                         
                         
                         healthWagesMaDireLOW  = healthWagesMaDireLOW,
                         healthWagesMaDireHIGH = healthWagesMaDireHIGH,
                         healthWagesMaDiagLOW  = healthWagesMaDiagLOW, 
                         healthWagesMaDiagHIGH = healthWagesMaDiagHIGH,
                         healthWagesMaAdmiLOW  = healthWagesMaAdmiLOW,
                         healthWagesMaAdmiHIGH = healthWagesMaAdmiHIGH,
                         
                         # Bachelors
                         healthBachelorsDire  = healthBachelorsDire,
                         healthBachelorsDiag  = healthBachelorsDiag,
                         healthBachelorsAdmi  = healthBachelorsAdmi, 
                         
                         totalJobPostingsHealthBa = totalJobPostingsHealthBa,
                         
                         healthPostingsBaDire = healthPostingsBaDire, 
                         healthPostingsBaDiag = healthPostingsBaDiag, 
                         healthPostingsBaAdmi = healthPostingsBaAdmi, 
                         
                         
                         healthWagesBaDireLOW  = healthWagesBaDireLOW,
                         healthWagesBaDireHIGH = healthWagesBaDireHIGH,
                         healthWagesBaDiagLOW  = healthWagesBaDiagLOW, 
                         healthWagesBaDiagHIGH = healthWagesBaDiagHIGH,
                         healthWagesBaAdmiLOW   = healthWagesBaAdmiLOW,
                         healthWagesBaAdmiHIGH  = healthWagesBaAdmiHIGH,
                         # ASSOCIATES
                         healthAssociatesDire = healthAssociatesDire, 
                         healthAssociatesDiag = healthAssociatesDiag, 
                         healthAssociatesAdmi  = healthAssociatesAdmi,
                         
                         totalJobPostingsHealthAs = totalJobPostingsHealthAs,
                         
                         healthPostingsAsDire = healthPostingsAsDire, 
                         healthPostingsAsDiag = healthPostingsAsDiag, 
                         healthPostingsAsAdmi = healthPostingsAsAdmi, 
                         
                         
                         healthWagesAsDireLOW  = healthWagesAsDireLOW,
                         healthWagesAsDireHIGH = healthWagesAsDireHIGH,
                         healthWagesAsDiagLOW  = healthWagesAsDiagLOW, 
                         healthWagesAsDiagHIGH = healthWagesAsDiagHIGH,
                         healthWagesAsAdmiLOW  = healthWagesAsAdmiLOW,
                         healthWagesAsAdmiHIGH = healthWagesAsAdmiHIGH,
                         
                         
                         # Certificate
                         healthCertificateDire = healthCertificateDire, 
                         healthCertificateDiag = healthCertificateDiag, 
                         healthCertificateAdmi = healthCertificateAdmi, 
                         
                         totalJobPostingsHealthCe = formatCommas(totalJobPostingsHealthCe),
                         
                         healthPostingsCeDire = healthPostingsCeDire, 
                         healthPostingsCeDiag = formatCommas(healthPostingsCeDiag), 
                         healthPostingsCeAdmi = healthPostingsCeAdmi,
                         
                         healthWagesCeDireLOW  = healthWagesCeDireLOW,
                         healthWagesCeDireHIGH = healthWagesCeDireHIGH,
                         healthWagesCeDiagLOW  = healthWagesCeDiagLOW, 
                         healthWagesCeDiagHIGH = healthWagesCeDiagHIGH,
                         healthWagesCeAdmiLOW  = healthWagesCeAdmiLOW,
                         healthWagesCeAdmiHIGH = healthWagesCeAdmiHIGH,
                         
                         #High School
                         healthHighSchoolDire  = healthHighSchoolDire, 
                         healthHighSchoolDiag  = healthHighSchoolDiag, 
                         healthHighSchoolAdmi  = healthHighSchoolAdmi,
                         
                         totalJobPostingsHealthHi = totalJobPostingsHealthHi,
                         
                         healthPostingsHiDire  = healthPostingsHiDire, 
                         healthPostingsHiDiag  = healthPostingsHiDiag, 
                         healthPostingsHiAdmi  = healthPostingsHiAdmi,
                         
                         healthWagesHiDireLOW  = healthWagesHiDireLOW,
                         healthWagesHiDireHIGH = healthWagesHiDireHIGH,
                         healthWagesHiDiagLOW  = healthWagesHiDiagLOW, 
                         healthWagesHiDiagHIGH = healthWagesHiDiagHIGH,
                         healthWagesHiAdmiLOW  = healthWagesHiAdmiLOW,
                         healthWagesHiAdmiHIGH = healthWagesHiAdmiHIGH
            ))
  })

