#Load packages
library(dplyr)
library(shiny)
library(RCurl)
library(stringr)
library(googlesheets)
library(shinythemes)
library(DT)

#Load data
burningGlassQuarterConnection   <- getURL('https://docs.google.com/spreadsheets/d/1gk0cOCFlAfZCN_Yh86qB0ob7N7PJVzXIRWsUVl81Lks/pub?gid=0&single=true&output=csv') #updated
emsiDataConnection              <- getURL('https://docs.google.com/spreadsheets/d/1gk0cOCFlAfZCN_Yh86qB0ob7N7PJVzXIRWsUVl81Lks/pub?gid=396778125&single=true&output=csv') #updated  
sectorsConnection               <- getURL('https://docs.google.com/spreadsheets/d/1rL0sCtUSzBbhlZYSGvUgx3fXip55o2OpMWUMK_6TKaA/pub?gid=487558132&single=true&output=csv')
socCrosswalkConnection          <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1551915918&single=true&output=csv')
splitsConnection                <- getURL("https://docs.google.com/spreadsheets/d/1LZ96VKkawzvybK9s5FQNX0S5sCuuiTJO0EPNAWajecU/pub?gid=253150309&single=true&output=csv")
#socConnection                  <- getURL('https://docs.google.com/spreadsheets/d/1wWVpXkU7OG2dGjCEEOK4Z4sS02tgK9_zee9cl0MdQRE/pub?gid=0&single=true&output=csv')
#burningGlassQuarter             <- read.csv('burningGlassQuarter.csv', check.names = FALSE)
#emsiData                        <- read.csv('emsiData.csv',            check.names = FALSE)
#burningGlassQuarter             <- burningGlassQuarter %>% select(-1)
#emsiData                        <- emsiData            %>% select(-1)
burningGlassQuarter             <- read.csv(textConnection(burningGlassQuarterConnection), check.names = FALSE)
emsiData                        <- read.csv(textConnection(emsiDataConnection),            check.names = FALSE)
sectors                         <- read.csv(textConnection(sectorsConnection))
socCrosswalk                    <- read.csv(textConnection(socCrosswalkConnection),        check.names = FALSE)
splits                          <- read.csv(textConnection(splitsConnection),              check.names = FALSE)
#majorSocCodeNames               <- read.csv(textConnection(socConnection))

rm(burningGlassQuarterConnection, 
   emsiDataConnection,
   sectorsConnection,
   socCrosswalkConnection)

#Merge Data
mainDataFile                    <- full_join(burningGlassQuarter, emsiData, by = 'SOC')
mainDataFile                    <- full_join(mainDataFile, socCrosswalk,    by = 'SOC')
mainDataFile                    <- left_join(mainDataFile, sectors,         by = "SOC")


#Make new columns with SOC + SECTOR for Main Data File and Splits file
splits$socSector           <- paste(splits$SOC, splits$Sector)
mainDataFile$socSector     <- paste(mainDataFile$SOC, mainDataFile$Sector)

splits           <- splits %>% select(4, 3)

mainDataFile     <- left_join(mainDataFile, splits, by = "socSector")    
mainDataFile     <- mainDataFile %>% select(-16, -17)
mainDataFile$SPLIT[is.na(mainDataFile$SPLIT)] <- 1

rm(burningGlassQuarter,
   emsiData, 
   sectors,
   socCrosswalk,
   splits)

#Select necessary variables
mainDataFile                    <- mainDataFile %>%
                                      select(1, 12, 3, 10:11, 7, 14:16)

replaceRemove <- function(dataToEnter, replaceThis, withThis){
                            dataToEnter <- as.data.frame(lapply(dataToEnter, function(x) {gsub(replaceThis, withThis, x) }))
}
mainDataFile <- replaceRemove(mainDataFile, ",", "")
mainDataFile <- replaceRemove(mainDataFile, "\\$", "")

variables <- c('Number.of.Job.Postings' ,
               'Pct..25.Hourly.Earnings',
               'Pct..75.Hourly.Earnings', 
               'SPLIT')

mainDataFile[,variables] <- lapply(mainDataFile[,variables] , as.character)
mainDataFile[,variables] <- lapply(mainDataFile[,variables] , as.numeric)

mainDataFile$deduplicatedPostings <- (mainDataFile$Number.of.Job.Postings)* .8
mainDataFile$deduplicatedPostings <- round(mainDataFile$deduplicatedPostings, digits = 0)
mainDataFile$deduplicatedPostings <- round(mainDataFile$deduplicatedPostings * mainDataFile$SPLIT)

mainDataFile <- mainDataFile %>% select(-9)


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
roundMean               <- function(dataEntry) {sprintf("%.2f", round(mean(dataEntry), digits = 2))}
formatCommas            <- function(dataEntry) {format(dataEntry, big.mark = ",", trim = TRUE)}
categoryFilter          <- function(dataNameEd, category){dataNameEd %>% filter(Category  == category)}
filterPostingsByPercent <- function(dataToEnter, percentMultiply) {dataToEnter[,'deduplicatedPostings'] <- dataToEnter[,'deduplicatedPostings']* percentMultiply}
wages25                 <- function(dataToEnter) {roundMean(dataToEnter[,'Pct..25.Hourly.Earnings'])}
wages75                 <- function(dataToEnter) {roundMean(dataToEnter[,'Pct..75.Hourly.Earnings'])}
sumDeduplicatedPostings <- function(dataToEnter){sum(dataToEnter[,"deduplicatedPostings"])}
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################       IT          #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
    ######################## DATA FILTERS ########################################
techJobs <- mainDataFile %>% filter(Sector == "IT")

###### EDUCATION LEVEL DATA 
  ###### TOTALS BY EDUCATION LEVEL 
  techJobsBa <- techJobs %>% filter(Typical.Entry.Level.Education == "Bachelor's degree")
  techJobsAs <- techJobs %>% filter(Typical.Entry.Level.Education == "Associate's degree"  |
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
      totalJobsIt           <-  sumDeduplicatedPostings(techJobs)     ##### TOTALS             
      totalJobPostingsItBa  <-  sumDeduplicatedPostings(techJobsBa)
      totalJobPostingsItAs  <-  sumDeduplicatedPostings(techJobsAs)
      itPostingsBaInfo      <-  sumDeduplicatedPostings(infoBa)       ##### BACHELORS               
      itPostingsBaProg      <-  sumDeduplicatedPostings(progBa) 
      itPostingsBaNet       <-  sumDeduplicatedPostings(netBa)
      itPostingsBaWeb       <-  sumDeduplicatedPostings(webBa)
      itPostingsAsInfo      <-  sumDeduplicatedPostings(infoAs)       ##### ASSOCIATES/SOME   
      itPostingsAsProg      <-  "" 
      itPostingsAsNet       <-  sumDeduplicatedPostings(netAs)
      itPostingsAsWeb       <-  sumDeduplicatedPostings(webAs)

######################## WAGE RANGES
      itWagesBaInfoLOW <- wages25(infoBa)       ## 25th Percentile 
      itWagesBaProgLOW <- wages25(progBa)
      itWagesBaNetLOW  <- wages25(netBa)
      itWagesBaWebLOW  <- wages25(webBa)
      itWagesAsInfoLOW <- wages25(infoAs)
      itWagesAsProgLOW <- ""
      itWagesAsNetLOW  <- wages25(netAs)
      itWagesAsWebLOW  <- wages25(webAs)
                                              ## 75th Percentile      
      itWagesBaInfoHIGH <- wages75(infoBa)
      itWagesBaProgHIGH <- wages75(progBa)
      itWagesBaNetHIGH  <- wages75(netBa)
      itWagesBaWebHIGH  <- wages75(webBa)
      itWagesAsInfoHIGH <- wages75(infoAs)
      itWagesAsProgHIGH <- ""
      itWagesAsNetHIGH  <- wages75(netAs)
      itWagesAsWebHIGH  <- wages75(webAs)

      

########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################       LOGISTICS          #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################

      ######################## SECTOR JOBS ########################################
      logJobs <- mainDataFile %>% filter(Sector == "Logistics")
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      logJobsBa <- logJobs %>% filter(Typical.Entry.Level.Education == "Bachelor's degree")                 ## BACHELORS
      logJobsAs <- ""                                                                                       ## ASSOCIATES  
      logJobsCe <- logJobs %>% filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")     ## CERTIFICATES    
      logJobsHi <- logJobs %>% filter(Typical.Entry.Level.Education == "High school diploma or equivalent") ## HIGH SCHOOL
      ######################## SECTOR JOBS BY COLUMN ##############################
      ################# COLUMN 
      ####### EDUCATION LEVEL
      ## ONE
        proBa  <- categoryFilter(logJobsBa, 'pro')
        proAs  <- ""
        proCe  <- categoryFilter(logJobsCe, 'pro')
        proHi  <- categoryFilter(logJobsHi, 'pro')
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

      ####### EDUCATION LEVEL
      ## BACHELORS      
      logPostingsBaPro      <-  sumDeduplicatedPostings(proBa)
      logPostingsBaTran     <-  sumDeduplicatedPostings(tranBa) 
      logPostingsBaWare     <-  sumDeduplicatedPostings(wareBa)
      ## ASSOCIATES  
      logPostingsAsPro      <-  ""
      logPostingsAsTran     <-  "" 
      logPostingsAsWare     <-  ""
      ## CERTIFICATES
      logPostingsCePro      <-  ""
      logPostingsCeTran     <-  sumDeduplicatedPostings(tranCe) 
      logPostingsCeWare     <-  ""
      ## HIGH SCHOOL
      logPostingsHiPro      <-  sumDeduplicatedPostings(proHi)
      logPostingsHiTran     <-  sumDeduplicatedPostings(tranHi) 
      logPostingsHiWare     <-  sumDeduplicatedPostings(wareHi)
      ####### TOTALS           
      totalJobPostingsLogBa  <-  sumDeduplicatedPostings(logJobsBa)
      totalJobPostingsLogAs  <-  ""
      totalJobPostingsLogCe  <-  sumDeduplicatedPostings(logJobsCe)
      totalJobPostingsLogHi  <-  sumDeduplicatedPostings(logJobsHi)
      
      totalJobsLog           <-  sum(totalJobPostingsLogBa, totalJobPostingsLogCe, totalJobPostingsLogHi)
    
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      logWagesBaProLOW  <- wages25(proBa)
      logWagesBaTranLOW <- wages25(tranBa)
      logWagesBaWareLOW <- wages25(wareBa)
      # ASSOCIATES
      logWagesAsProLOW  <- ""
      logWagesAsTranLOW <- ""
      logWagesAsWareLOW <- ""
      # CERTIFICATES 
      logWagesCeProLOW  <- ""
      logWagesCeTranLOW <- wages25(tranCe)
      logWagesCeWareLOW <- ""
      # HIGH SCHOOL 
      logWagesHiProLOW  <- wages25(proHi)
      logWagesHiTranLOW <- wages25(tranHi)
      logWagesHiWareLOW <- wages25(wareHi)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      logWagesBaProHIGH  <- wages75(proBa)
      logWagesBaTranHIGH <- wages75(tranBa)
      logWagesBaWareHIGH <- wages75(wareBa)
      ## ASSOCIATES
      logWagesAsProHIGH  <- ""
      logWagesAsTranHIGH <- ""
      logWagesAsWareHIGH <- ""
      ## CERTIFICATES
      logWagesCeProHIGH  <- ""
      logWagesCeTranHIGH <- wages75(tranCe)
      logWagesCeWareHIGH <- ""
      ## HIGH SCHOOL 
      logWagesHiProHIGH  <- wages75(proHi)
      logWagesHiTranHIGH <- wages75(tranHi)
      logWagesHiWareHIGH <- wages75(wareHi)
       

########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################       MANUFACTURING      #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################

      ######################## SECTOR JOBS ########################################
      manJobs <- mainDataFile %>% filter(Sector == "Manufacturing")
      certSoc <- c("51-4121", "49-9041", "51-4011", "51-1011")
      manJobsCe <- manJobs %>% filter(SOC %in% certSoc) 
      manJobs <- manJobs %>% filter(!SOC %in% certSoc)
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      manJobsBa <- manJobs %>% filter(Typical.Entry.Level.Education == "Bachelor's degree")                 ## BACHELORS
      manJobsAs <- manJobs %>% filter(Typical.Entry.Level.Education == "Associate's degree")                ## ASSOCIATES    
                                                                                                            ## CERTIFICATES (See above, special situation)
      manJobsHi <- manJobs %>% filter(Typical.Entry.Level.Education == "High school diploma or equivalent") ## HIGH SCHOOL
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
      manCertificateQual <- noJobsMessage
      manCertificateMain <- "Welders; Industrial Machinery Mechanics; Industrial Maintenance Technicians"
      
      ## HIGH SCHOOL
      manHighSchoolProd  <- "Assembly Technicians; Industrial Tool Operators"
      manHighSchoolProc  <- noJobsMessage
      manHighSchoolQual  <- "Quality Assurance Specialist"
      manHighSchoolMain  <- "Repair Technician"
      
      ## STOPPED HERE
      ################# JOB POSTINGS

      ####### EDUCATION LEVEL
      ## BACHELORS      
      manPostingsBaProd     <-  sumDeduplicatedPostings(prodBa)
      manPostingsBaProc     <-  sumDeduplicatedPostings(procBa) 
      manPostingsBaQual     <-  sumDeduplicatedPostings(qualBa)
      manPostingsBaMain     <-  sumDeduplicatedPostings(mainBa)
      ## ASSOCIATES  
      manPostingsAsProd     <-  sumDeduplicatedPostings(prodAs) 
      manPostingsAsProc     <-  sumDeduplicatedPostings(procAs) 
      manPostingsAsQual     <-  sumDeduplicatedPostings(qualAs) 
      manPostingsAsMain     <-  sumDeduplicatedPostings(mainAs) 
      ## CERTIFICATES
      manPostingsCeProd     <-  sumDeduplicatedPostings(prodCe) 
      manPostingsCeProc     <-  sumDeduplicatedPostings(procCe) 
      manPostingsCeQual     <-  sumDeduplicatedPostings(qualCe) 
      manPostingsCeMain     <-  sumDeduplicatedPostings(mainCe) 
      ## HIGH SCHOOL
      manPostingsHiProd     <-  sumDeduplicatedPostings(prodHi)
      manPostingsHiProc     <-  sumDeduplicatedPostings(procHi) 
      manPostingsHiQual     <-  sumDeduplicatedPostings(qualHi)
      manPostingsHiMain     <-  sumDeduplicatedPostings(mainHi)
      ####### TOTALS           
      totalJobPostingsManBa  <-  sumDeduplicatedPostings(manJobsBa)
      totalJobPostingsManAs  <-  sumDeduplicatedPostings(manJobsAs)
      totalJobPostingsManCe  <-  sumDeduplicatedPostings(manJobsCe)
      totalJobPostingsManHi  <-  sumDeduplicatedPostings(manJobsHi)
      
      totalJobsMan           <-  sum(totalJobPostingsManBa, totalJobPostingsManAs, totalJobPostingsManCe, totalJobPostingsManHi)
      
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      manWagesBaProdLOW <- wages25(prodBa)
      manWagesBaProcLOW <- wages25(procBa)
      manWagesBaQualLOW <- wages25(qualBa)
      manWagesBaMainLOW <- wages25(mainBa)
      # ASSOCIATES
      manWagesAsProdLOW <- wages25(prodAs)
      manWagesAsProcLOW <- wages25(procAs)
      manWagesAsQualLOW <- wages25(qualAs)
      manWagesAsMainLOW <- wages25(mainAs)
      # CERTIFICATES 
      manWagesCeProdLOW <- wages25(prodCe)
      manWagesCeProcLOW <- wages25(procCe)
      manWagesCeQualLOW <- wages25(qualCe)
      manWagesCeMainLOW <- wages25(mainCe)
      # HIGH SCHOOL 
      manWagesHiProdLOW <- wages25(prodHi)
      manWagesHiProcLOW <- wages25(procHi)
      manWagesHiQualLOW <- wages25(wareHi)
      manWagesHiMainLOW <- wages25(mainHi)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      manWagesBaProdHIGH <- wages75(prodBa)
      manWagesBaProcHIGH <- wages75(procBa)
      manWagesBaQualHIGH <- wages75(qualBa)
      manWagesBaMainHIGH <- wages75(mainBa)
      ## ASSOCIATES
      manWagesAsProdHIGH <- wages75(prodAs)
      manWagesAsProcHIGH <- wages75(procAs)
      manWagesAsQualHIGH <- wages75(qualAs)
      manWagesAsMainHIGH <- wages75(mainAs)
      ## CERTIFICATES
      manWagesCeProdHIGH <- wages75(prodCe)
      manWagesCeProcHIGH <- wages75(procCe)
      manWagesCeQualHIGH <- wages75(qualCe)
      manWagesCeMainHIGH <- wages75(mainCe)
      ## HIGH SCHOOL 
      manWagesHiProdHIGH <- wages75(prodHi)
      manWagesHiProcHIGH <- wages75(procHi)
      manWagesHiQualHIGH <- wages75(qualHi)
      manWagesHiMainHIGH <- wages75(mainHi)
      

  ########################################################################################################################################################################################################################
  ########################################################################################################################################################################################################################
  #####################################       BUSINESS     #################################################################################################################################################################
  ########################################################################################################################################################################################################################
  ########################################################################################################################################################################################################################
  
      ######################## SECTOR JOBS ########################################
      busJobs <- mainDataFile %>% filter(Sector == "Business")
      
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      busJobsMa <- busJobs %>% filter(Typical.Entry.Level.Education == "Master's degree"                 |   ## MASTERS/DOCTORAL/PROFESSIONAL
                                      Typical.Entry.Level.Education == "Doctoral or professional degree") 
      busJobsBa <- busJobs %>% filter(Typical.Entry.Level.Education == "Bachelor's degree")                  ## BACHELORS
      busJobsAs <- busJobs %>% filter(Typical.Entry.Level.Education == "Associate's degree")                 ## ASSOCIATES     
      busJobsCe <- busJobs %>% filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")      ## CERTIFICATES    
      busJobsHi <- busJobs %>% filter(Typical.Entry.Level.Education == "High school diploma or equivalent" | ## HIGH SCHOOL
                                      Typical.Entry.Level.Education == "Some college no degree")
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
      busAssociatesAdv  <- "Human Resource Assistants"
      ## CERTIFICATE
      busCertificateFin <- noJobsMessage
      busCertificateLeg <- noJobsMessage
      busCertificateAdv <- "Audio and Video Equipment Technicians"
      ## HIGH SCHOOL
      busHighSchoolFin  <- "Collections and Billing Specialists; Bookkeepers; Correspondence Clerks; Credit Assistants"
      busHighSchoolLeg  <- "Legal Administrative Assistants; Receptionists"
      busHighSchoolAdv  <- "Office Assistants; Artists; Media and Communication Assistants; Photographers; Secretaries and Administrative Assistants"
      
      ################# JOB POSTINGS
      ####### TOTALS           
      totalJobPostingsBusMa  <-  sumDeduplicatedPostings(busJobsMa)
      totalJobPostingsBusBa  <-  sumDeduplicatedPostings(busJobsBa)
      totalJobPostingsBusAs  <-  sumDeduplicatedPostings(busJobsAs)
      totalJobPostingsBusCe  <-  sumDeduplicatedPostings(busJobsCe)
      totalJobPostingsBusHi  <-  sumDeduplicatedPostings(busJobsHi)
      totalJobsBus           <-  sum(totalJobPostingsBusMa, totalJobPostingsBusBa, totalJobPostingsBusAs, totalJobPostingsBusCe, totalJobPostingsBusHi)
      ####### EDUCATION LEVEL
      ## MASTERS
      busPostingsMaFin     <-  sumDeduplicatedPostings(finMa)
      busPostingsMaLeg     <-  sumDeduplicatedPostings(legMa)
      busPostingsMaAdv     <-  sumDeduplicatedPostings(advMa)
      ## BACHELORS      
      busPostingsBaFin     <-  sumDeduplicatedPostings(finBa)
      busPostingsBaLeg     <-  sumDeduplicatedPostings(legBa) 
      busPostingsBaAdv     <-  sumDeduplicatedPostings(advBa)
      ## ASSOCIATES  
      busPostingsAsFin     <-  ""
      busPostingsAsLeg     <-  sumDeduplicatedPostings(legAs) 
      busPostingsAsAdv     <-  sumDeduplicatedPostings(advAs) 
      ## CERTIFICATES
      busPostingsCeFin     <-  sumDeduplicatedPostings(finCe)
      busPostingsCeLeg     <-  sumDeduplicatedPostings(legCe) 
      busPostingsCeAdv     <-  sumDeduplicatedPostings(advCe)
      ## HIGH SCHOOL
      busPostingsHiFin     <-  sumDeduplicatedPostings(finHi)
      busPostingsHiLeg     <-  sumDeduplicatedPostings(legHi) 
      busPostingsHiAdv     <-  sumDeduplicatedPostings(advHi)
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # MASTERS
      busWagesMaFinLOW <- wages25(finMa)
      busWagesMaLegLOW <- wages25(legMa)
      busWagesMaAdvLOW <- wages25(advMa)
      # BACHELORS
      busWagesBaFinLOW <- wages25(finBa)
      busWagesBaLegLOW <- wages25(legBa)
      busWagesBaAdvLOW <- wages25(advBa)
      # ASSOCIATES
      busWagesAsFinLOW <- ""
      busWagesAsLegLOW <- wages25(legAs)
      busWagesAsAdvLOW <- wages25(advAs)
      # CERTIFICATES 
      busWagesCeFinLOW <- ""
      busWagesCeLegLOW <- wages25(legCe)
      busWagesCeAdvLOW <- wages25(advCe)
      # HIGH SCHOOL 
      busWagesHiFinLOW <- wages25(finHi)
      busWagesHiLegLOW <- wages25(legHi)
      busWagesHiAdvLOW <- wages25(advHi)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # MASTERS
      busWagesMaFinHIGH <- wages75(finMa)
      busWagesMaLegHIGH <- wages75(legMa)
      busWagesMaAdvHIGH <- wages75(advMa)
      # BACHELORS
      busWagesBaFinHIGH <- wages75(finBa)
      busWagesBaLegHIGH <- wages75(legBa)
      busWagesBaAdvHIGH <- wages75(advBa)
      ## ASSOCIATES
      busWagesAsFinHIGH <- ""
      busWagesAsLegHIGH <- wages75(legAs)
      busWagesAsAdvHIGH <- wages75(advAs)
      ## CERTIFICATES
      busWagesCeFinHIGH <- ""
      busWagesCeLegHIGH <- wages75(legCe)
      busWagesCeAdvHIGH <- wages75(advCe)
      ## HIGH SCHOOL 
      busWagesHiFinHIGH <- wages75(finHi)
      busWagesHiLegHIGH <- wages75(legHi)
      busWagesHiAdvHIGH <- wages75(advHi)

########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################     CONSTRUCTION       #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
      
      ######################## SECTOR JOBS ########################################
      conJobs <- mainDataFile %>% filter(Sector == "Construction")
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      conJobsCe <- conJobs %>% filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")        ## Credential
      conJobsHi <- conJobs %>% filter(Typical.Entry.Level.Education == "High school diploma or equivalent")    ## High School  
      conJobsNo <- conJobs %>% filter(Typical.Entry.Level.Education == "No formal educational credential")     ## No formal education credential   
      
      variables       <- c("47-2111", "47-2152", "41-9022", "41-9021")
      apprenticeships <- conJobs %>% filter(SOC %in% variables) 
      conJobsCe       <- rbind(conJobsCe, apprenticeships)
      conJobsHi       <- conJobsHi %>% filter(!SOC %in% variables)
      ######################## SECTOR JOBS BY COLUMN ##############################
      ################# COLUMN 
      ####### EDUCATION LEVEL
      ## ONE
      consCe  <- categoryFilter(conJobsCe, 'con')
      consHi  <- categoryFilter(conJobsHi, 'con')
      consNo  <- categoryFilter(conJobsNo, 'con')
      ## TWO
      propCe <- categoryFilter(conJobsCe, 'prop')
      propHi <- categoryFilter(conJobsHi, 'prop')
      propNo <- categoryFilter(conJobsNo, 'prop')
      ## THREE
      realCe <- categoryFilter(conJobsCe, 'real')
      realHi <- categoryFilter(conJobsHi, 'real')
      realNo <- categoryFilter(conJobsNo, 'real')      
      
      ######################## COLUMN ENTRY #######################################    
      ################# NAMES 
      ####### EDUCATION LEVEL
      ## CERTIFICATE
      conCertificateCons  <- "Electricians; Plumbers; HVAC & Refrigeration Mechanics"
      conCertificateProp  <- "Electronic Equiptment Installers and Repairers"
      conCertificateReal  <- "Real Estate Agents and Brokers"
      ## HIGH SCHOOL
      conHighSchoolCons   <- "Supervisors; Inspectors; Helpers"
      conHighSchoolProp   <- "Repair & Installation Maintenance; Supervisors of Housekeeping & Janitorial Workers"
      conHighSchoolReal   <- "Property Real Estate & Community Association Managers"
      ## NO FORMAL EDUCATION CREDENTIAL 
      conNoSchoolCons    <- "Construction Laborers, Painters, Drywall Installers, Roofers"
      conNoSchoolProp    <- "Janitors; Housekeeping Cleaners; Landscaping & Groundskeeping Workers"
      conNoSchoolReal    <- noJobsMessage 
      
      ################# JOB POSTINGS
      ####### TOTALS           
      totalJobsCon           <-  sumDeduplicatedPostings(conJobs)
      totalJobPostingsConCe  <-  sumDeduplicatedPostings(conJobsCe)
      totalJobPostingsConHi  <-  sumDeduplicatedPostings(conJobsHi)
      totalJobPostingsConNo  <-  sumDeduplicatedPostings(conJobsNo)
      ####### EDUCATION LEVEL
      ## CERTIFICATES
      conPostingsCeCons     <-  sumDeduplicatedPostings(consCe)
      conPostingsCeProp     <-  sumDeduplicatedPostings(propCe) 
      conPostingsCeReal     <-  sumDeduplicatedPostings(realCe)
      ## HIGH SCHOOL
      conPostingsHiCons     <-  sumDeduplicatedPostings(consHi)
      conPostingsHiProp     <-  sumDeduplicatedPostings(propHi) 
      conPostingsHiReal     <-  sumDeduplicatedPostings(realHi)
      ## NO FORMAL EDUCATION CREDENTIAL
      conPostingsNoCons     <-  sumDeduplicatedPostings(consNo)
      conPostingsNoProp     <-  sumDeduplicatedPostings(propNo) 
      conPostingsNoReal     <-  sumDeduplicatedPostings(realNo)
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      conWagesCeConsLOW  <- wages25(consCe)  # CERTIFICATES 
      conWagesCePropLOW  <- wages25(propCe)
      conWagesCeRealLOW  <- wages25(realCe)
      conWagesHiConsLOW  <- wages25(consHi)  # HIGH SCHOOL 
      conWagesHiPropLOW  <- wages25(propHi)
      conWagesHiRealLOW  <- wages25(realHi)
      conWagesNoConsLOW  <- wages25(consNo)  # NO FORMAL EDUCATION CREDENTIAL
      conWagesNoPropLOW  <- wages25(propNo)
      conWagesNoRealLOW  <- wages25(realNo)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      conWagesCeConsHIGH <- wages75(consCe) ## CERTIFICATES
      conWagesCePropHIGH <- wages75(propCe)
      conWagesCeRealHIGH <- wages75(realCe)
      conWagesHiConsHIGH <- wages75(consHi) ## HIGH SCHOOL 
      conWagesHiPropHIGH <- wages75(propHi)
      conWagesHiRealHIGH <- wages75(realHi)    
      conWagesNoConsHIGH <- wages75(consNo) # NO FORMAL EDUCATION CREDENTIAL
      conWagesNoPropHIGH <- wages75(propNo)
      conWagesNoRealHIGH <- wages75(realNo)
      
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#####################################       FOOD AND BEVERAGE      #################################################################################################################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################

      ######################## SECTOR JOBS ########################################
      foodJobs <- mainDataFile %>% filter(Sector == "Food")
      
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      ## BACHELORS
      foodJobsBa <- foodJobs %>% filter(Typical.Entry.Level.Education == "Bachelor's degree")
      
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

      ####### EDUCATION LEVEL
      ## BACHELORS      
      foodPostingsBaProd     <-  sumDeduplicatedPostings(prodBa)
      foodPostingsBaRest     <-  sumDeduplicatedPostings(restBa) 
      foodPostingsBaHosp     <-  sumDeduplicatedPostings(hospBa)
      foodPostingsBaCorp     <-  sumDeduplicatedPostings(corpBa)
      ## ASSOCIATES  
      foodPostingsAsProd     <-  ""
      foodPostingsAsRest     <-  "" 
      foodPostingsAsHosp     <-  ""
      foodPostingsAsCorp     <-  ""
      ## CERTIFICATES
      foodPostingsCeProd     <-  sumDeduplicatedPostings(prodCe) 
      foodPostingsCeRest     <-  ""
      foodPostingsCeHosp     <-  ""
      foodPostingsCeCorp     <-  ""
      ## HIGH SCHOOL
      foodPostingsHiProd     <-  sumDeduplicatedPostings(prodHi)
      foodPostingsHiRest     <-  sumDeduplicatedPostings(restHi) 
      foodPostingsHiHosp     <-  sumDeduplicatedPostings(hospHi)
      foodPostingsHiCorp     <-  sumDeduplicatedPostings(corpHi)
      ## LESS THAN HIGH SCHOOL
      foodPostingsNoProd     <-  sumDeduplicatedPostings(prodNo)
      foodPostingsNoRest     <-  sumDeduplicatedPostings(restNo) 
      foodPostingsNoHosp     <-  sumDeduplicatedPostings(hospNo)
      foodPostingsNoCorp     <-  ""
      ####### TOTALS           
      totalJobPostingsFoodBa  <-  sumDeduplicatedPostings(foodJobsBa)
      totalJobPostingsFoodAs  <-  ""
      totalJobPostingsFoodCe  <-  sumDeduplicatedPostings(foodJobsCe)
      totalJobPostingsFoodHi  <-  sumDeduplicatedPostings(foodJobsHi)
      totalJobPostingsFoodNo  <-  sumDeduplicatedPostings(foodJobsNo)
      
      totalJobsFood           <-  sum(totalJobPostingsFoodBa, totalJobPostingsFoodCe, totalJobPostingsFoodHi, totalJobPostingsFoodNo)
      
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      foodWagesBaProdLOW <- wages25(proBa)
      foodWagesBaRestLOW <- wages25(restBa)
      foodWagesBaHospLOW <- wages25(hospBa)
      foodWagesBaCorpLOW <- wages25(corpBa)
      # ASSOCIATES
      foodWagesAsProdLOW <- ""
      foodWagesAsRestLOW <- ""
      foodWagesAsHospLOW <- ""
      foodWagesAsCorpLOW <- ""
      # CERTIFICATES 
      foodWagesCeProdLOW <- wages25(prodCe)
      foodWagesCeRestLOW <- ""
      foodWagesCeHospLOW <- ""
      foodWagesCeCorpLOW <- ""
      # HIGH SCHOOL 
      foodWagesHiProdLOW <- wages25(proHi)
      foodWagesHiRestLOW <- wages25(restHi)
      foodWagesHiHospLOW <- wages25(hospHi)
      foodWagesHiCorpLOW <- wages25(corpHi)
      # LESS THAN HIGH SCHOOL 
      foodWagesNoProdLOW <- wages25(prodNo)
      foodWagesNoRestLOW <- wages25(restNo)
      foodWagesNoHospLOW <- wages25(hospNo)
      foodWagesNoCorpLOW <- ""
      
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # BACHELORS
      foodWagesBaProdHIGH <- wages75(proBa)
      foodWagesBaRestHIGH <- wages75(restBa)
      foodWagesBaHospHIGH <- wages75(hospBa)
      foodWagesBaCorpHIGH <- wages75(corpBa)
      ## ASSOCIATES
      foodWagesAsProdHIGH <- ""
      foodWagesAsRestHIGH <- ""
      foodWagesAsHospHIGH <- ""
      foodWagesAsCorpHIGH <- ""
      ## CERTIFICATES
      foodWagesCeProdHIGH <- wages75(prodCe)
      foodWagesCeRestHIGH <- ""
      foodWagesCeHospHIGH <- ""
      foodWagesCeCorpHIGH <- ""
      ## HIGH SCHOOL 
      foodWagesHiProdHIGH <- wages75(prodHi)
      foodWagesHiRestHIGH <- wages75(restHi)
      foodWagesHiHospHIGH <- wages75(hospHi)
      foodWagesHiCorpHIGH <- wages75(corpHi)
      ## HIGH SCHOOL 
      foodWagesNoProdHIGH <- wages75(prodNo)
      foodWagesNoRestHIGH <- wages75(restNo)
      foodWagesNoHospHIGH <- wages75(hospNo)
      foodWagesNoCorpHIGH <- ""


      ########################################################################################################################################################################################################################
      ########################################################################################################################################################################################################################
      #####################################       Healthcare    #################################################################################################################################################################
      ########################################################################################################################################################################################################################
      ########################################################################################################################################################################################################################
      
      ######################## SECTOR JOBS ########################################
      healthJobs <- mainDataFile %>% filter(Sector == "Healthcare")
      ######################## SECTOR JOBS BY EDUCATION LEVEL #####################
      healthJobsDo <- healthJobs %>% filter(Typical.Entry.Level.Education == "Doctoral or professional degree")   ## DOCTORAL/PROFESSIONAL
      healthJobsMa <- healthJobs %>% filter(Typical.Entry.Level.Education == "Master's degree")                   ## MASTERS
      healthJobsBa <- healthJobs %>% filter(Typical.Entry.Level.Education == "Bachelor's degree")                 ## BACHELORS
      healthJobsAs <- healthJobs %>% filter(Typical.Entry.Level.Education == "Associate's degree")                ## ASSOCIATES     
      healthJobsCe <- healthJobs %>% filter(Typical.Entry.Level.Education == "Postsecondary nondegree award")     ## CERTIFICATES    
      healthJobsHi <- healthJobs %>% filter(Typical.Entry.Level.Education == "High school diploma or equivalent") ## HIGH SCHOOL
      ######################## SECTOR JOBS BY COLUMN ##############################
      ################# COLUMN
      ####### EDUCATION LEVEL
      direDo <- categoryFilter(healthJobsDo, 'dire') ## ONE
      direMa <- categoryFilter(healthJobsMa, 'dire') 
      direBa <- categoryFilter(healthJobsBa, 'dire') 
      direAs <- categoryFilter(healthJobsAs, 'dire') 
      direCe <- categoryFilter(healthJobsCe, 'dire') 
      direHi <- categoryFilter(healthJobsHi, 'dire') 
      
      diagDo <- categoryFilter(healthJobsDo, 'diag') ## TWO
      diagMa <- categoryFilter(healthJobsMa, 'diag')
      diagBa <- categoryFilter(healthJobsBa, 'diag')
      diagAs <- categoryFilter(healthJobsAs, 'diag')   
      diagCe <- categoryFilter(healthJobsCe, 'diag')
      diagHi <- categoryFilter(healthJobsHi, 'diag')
      
      admiDo <- categoryFilter(healthJobsDo, 'admi') ## THREE
      admiMa <- categoryFilter(healthJobsMa, 'admi')
      admiBa <- categoryFilter(healthJobsBa, 'admi')
      admiAs <- categoryFilter(healthJobsAs, 'admi')
      admiCe <- categoryFilter(healthJobsCe, 'admi')
      admiHi <- categoryFilter(healthJobsHi, 'admi')
      
      ## SPECIFIC FILTERS
      variables  <- c("15-1121", "15-1143", "15-1199") 
      healthComp <- admiBa %>% filter(SOC %in% variables)
      healthHr   <- admiBa %>% filter(SOC == "13-1071")
      admiBa     <- admiBa %>% filter(!SOC %in% variables & SOC != "13-1071")
      
      healthComp$deduplicatedPostings <- filterPostingsByPercent(healthComp, .1) 
      healthHr$deduplicatedPostings   <- filterPostingsByPercent(healthHr,   .5)
      healthCompAndHr                 <- rbind(healthHr, healthComp)
      
      admiBa <- rbind(admiBa, healthCompAndHr)
      
      ######################## COLUMN ENTRY #######################################    
      ################# NAMES 
      ####### EDUCATION LEVEL
      ## DOCTORAL 
      healthDoctoralDire    <- "Physicians and Surgeons; Physical Therapists; Audiologists; Family and General Practitioners"
      healthDoctoralDiag    <- "Dentists; Orthodontists; Pharmacists; Optometrists"
      healthDoctoralAdmi    <- "Healthcare Lawyers"
      ## MASTERS
      healthMastersDire     <- "Nurse Practitioners; Occupational Therapists; Physicians Assistants; Nurse Anesthetists; Speech-Language Pathologists"
      healthMastersDiag     <- "Mental Health Counselors"
      healthMastersAdmi     <- "Healthcare Social Workers; Statisticians"
      ## BACHELORS
      healthBachelorsDire   <- "Directors of Nursing, Registered Nurses"
      healthBachelorsDiag   <- "Medical and Clinical Laboratory Technicians; Dieticians and Nutritionists"
      healthBachelorsAdmi   <- "Systems and Accounting Analysts; Business Office Managers; Human Resources Specialists; Human Resources and Marketing Managers; Marketing Coordinators; Information Technology Managers; Network Engineer; Risk and Financial Analysts"
      ## ASSOCIATES
      healthAssociatesDire  <- "Physical and Occupational Therapist Assistants; Respiratory Therapists; Sonographers; Cardiovascular Technicians"
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
  
      ####### EDUCATION LEVEL
      ## DOCTORAL 
      healthPostingsDoDire      <-  sumDeduplicatedPostings(direDo)
      healthPostingsDoDiag      <-  sumDeduplicatedPostings(diagDo)
      healthPostingsDoAdmi      <-  round((sumDeduplicatedPostings(admiDo)*.2), 0)
      ## MASTERS
      healthPostingsMaDire      <-  sumDeduplicatedPostings(direMa)
      healthPostingsMaDiag      <-  sumDeduplicatedPostings(diagMa)
      healthPostingsMaAdmi      <-  sumDeduplicatedPostings(admiMa)
      ## BACHELORS      
      healthPostingsBaDire      <-  sumDeduplicatedPostings(direBa)
      healthPostingsBaDiag      <-  sumDeduplicatedPostings(diagBa) 
      healthPostingsBaAdmi      <-  round(sumDeduplicatedPostings(admiBa), 0)
      ## ASSOCIATES  
      healthPostingsAsDire      <-  sumDeduplicatedPostings(direAs)
      healthPostingsAsDiag      <-  sumDeduplicatedPostings(diagAs) 
      healthPostingsAsAdmi      <-  ""
      ## CERTIFICATES
      healthPostingsCeDire     <-  sumDeduplicatedPostings(direCe)
      healthPostingsCeDiag     <-  sumDeduplicatedPostings(diagCe) 
      healthPostingsCeAdmi     <-  sumDeduplicatedPostings(admiCe)
      ## HIGH SCHOOL
      healthPostingsHiDire     <-  sumDeduplicatedPostings(direHi)
      healthPostingsHiDiag     <-  sumDeduplicatedPostings(diagHi) 
      healthPostingsHiAdmi     <-  sumDeduplicatedPostings(admiHi)
     
      ####### TOTALS           
      totalJobPostingsHealthDo  <-  sum(healthPostingsDoDire, healthPostingsDoDiag, healthPostingsDoAdmi)
      totalJobPostingsHealthMa  <-  sumDeduplicatedPostings(healthJobsMa)
      totalJobPostingsHealthBa <-  healthPostingsBaAdmi + healthPostingsBaDiag + healthPostingsBaDire
      totalJobPostingsHealthAs  <-  sumDeduplicatedPostings(healthJobsAs)
      totalJobPostingsHealthCe  <-  sumDeduplicatedPostings(healthJobsCe)
      totalJobPostingsHealthHi  <-  sumDeduplicatedPostings(healthJobsHi)
      
      totalJobsHealth           <-  sum(totalJobPostingsHealthDo, totalJobPostingsHealthMa, totalJobPostingsHealthBa, totalJobPostingsHealthAs, totalJobPostingsHealthCe, totalJobPostingsHealthHi)
      
      
      ################# WAGES
      ####### 25th PERCENTILE 
      ## EDUCATION LEVEL
      # MASTERS
      healthWagesDoDireLOW <- wages25(direDo)
      healthWagesDoDiagLOW <- wages25(diagDo)
      healthWagesDoAdmiLOW <- wages25(admiDo)
      # MASTERS
      healthWagesMaDireLOW <- wages25(direMa)
      healthWagesMaDiagLOW <- wages25(diagMa)
      healthWagesMaAdmiLOW <- wages25(admiMa)
      # BACHELORS
      healthWagesBaDireLOW <- wages25(direBa)
      healthWagesBaDiagLOW <- wages25(diagBa)
      healthWagesBaAdmiLOW <- wages25(admiBa)
      # ASSOCIATES
      healthWagesAsDireLOW <- wages25(direAs)
      healthWagesAsDiagLOW <- wages25(diagAs)
      healthWagesAsAdmiLOW <- ""
      # CERTIFICATES 
      healthWagesCeDireLOW <- wages25(direCe)
      healthWagesCeDiagLOW <- wages25(diagCe)
      healthWagesCeAdmiLOW <- wages25(admiCe)
      # HIGH SCHOOL 
      healthWagesHiDireLOW <- wages25(direHi)
      healthWagesHiDiagLOW <- wages25(diagHi)
      healthWagesHiAdmiLOW <- wages25(admiHi)
      ####### 75th PERCENTILE 
      ## EDUCATION LEVEL
      # MASTERS
      healthWagesDoDireHIGH <- wages75(direDo)
      healthWagesDoDiagHIGH <- wages75(diagDo)
      healthWagesDoAdmiHIGH <- wages75(admiDo)
      # MASTERS
      healthWagesMaDireHIGH <- wages75(direMa)
      healthWagesMaDiagHIGH <- wages75(diagMa)
      healthWagesMaAdmiHIGH <- wages75(admiMa)
      # BACHELORS
      healthWagesBaDireHIGH <- wages75(direBa)
      healthWagesBaDiagHIGH <- wages75(diagBa)
      healthWagesBaAdmiHIGH <- wages75(admiBa)
      ## ASSOCIATES
      healthWagesAsDireHIGH <- wages75(direAs)
      healthWagesAsDiagHIGH <- wages75(diagAs)
      healthWagesAsAdmiHIGH <- ""
      ## CERTIFICATES
      healthWagesCeDireHIGH <- wages75(direCe)
      healthWagesCeDiagHIGH <- wages75(diagCe)
      healthWagesCeAdmiHIGH <- wages75(admiCe)
      ## HIGH SCHOOL 
      healthWagesHiDireHIGH <- wages75(direHi)
      healthWagesHiDiagHIGH <- wages75(diagHi)
      healthWagesHiAdmiHIGH <- wages75(admiHi)
      
      mainData <- mainDataFile %>%
                  select(1:3, 6:8, 4:5, 9)
      colnames(mainData)[3] <- 'Job Postings'
      colnames(mainData)[4] <- 'Entry Education'
      colnames(mainData)[7] <- 'Pct. 25 Earnings'
      colnames(mainData)[8] <- 'Pct. 75 Earnings'
      colnames(mainData)[9] <- 'Deduplicated Postings'

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
                         logBachelorsPro   =  logBachelorsPro,
                         logBachelorsTran  = logBachelorsTran,
                         logBachelorsWare  = logBachelorsWare, 
                         
                         totalJobPostingsLogBa = totalJobPostingsLogBa,
                         
                         logPostingsBaPro   = logPostingsBaPro, 
                         logPostingsBaTran  = logPostingsBaTran, 
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
          
          output$construction <- renderUI(
            htmlTemplate('constructionTemplate.html', 
                         totalJobs = formatCommas(totalJobsCon), 
                         degreeName2        = degreeName2, 
                         degreeName1        = degreeName1,
                         degreeName0        = degreeName0, 
                         
                         # Certificate
                         conCertificateCons = conCertificateCons, 
                         conCertificateProp = conCertificateProp, 
                         conCertificateReal = conCertificateReal, 
                         
                         totalJobPostingsConCe = formatCommas(totalJobPostingsConCe),
                         
                         conPostingsCeCons = conPostingsCeCons, 
                         conPostingsCeProp = formatCommas(conPostingsCeProp), 
                         conPostingsCeReal = conPostingsCeReal, 
                         
                         conWagesCeConsLOW  = conWagesCeConsLOW,
                         conWagesCeConsHIGH = conWagesCeConsHIGH,
                         conWagesCePropLOW  = conWagesCePropLOW, 
                         conWagesCePropHIGH = conWagesCePropHIGH,
                         conWagesCeRealLOW  = conWagesCeRealLOW,
                         conWagesCeRealHIGH = conWagesCeRealHIGH,

                         #High School
                         conHighSchoolCons   = conHighSchoolCons, 
                         conHighSchoolProp   = conHighSchoolProp, 
                         conHighSchoolReal   = conHighSchoolReal,
                         
                         totalJobPostingsConHi = totalJobPostingsConHi,
                         
                         conPostingsHiCons  = conPostingsHiCons, 
                         conPostingsHiProp  = conPostingsHiProp, 
                         conPostingsHiReal  = conPostingsHiReal, 
                         
                         conWagesHiConsLOW  = conWagesHiConsLOW,
                         conWagesHiConsHIGH = conWagesHiConsHIGH,
                         conWagesHiPropLOW  = conWagesHiPropLOW, 
                         conWagesHiPropHIGH = conWagesHiPropHIGH,
                         conWagesHiRealLOW  = conWagesHiRealLOW,
                         conWagesHiRealHIGH = conWagesHiRealHIGH,
                         
                         # NoSchool
                         conNoSchoolCons  =  conNoSchoolCons,
                         conNoSchoolProp  = conNoSchoolProp,
                         conNoSchoolReal  = conNoSchoolReal, 
                         
                         totalJobPostingsConNo = totalJobPostingsConNo,
                         
                         conPostingsNoCons  = conPostingsNoCons, 
                         conPostingsNoProp  = conPostingsNoProp, 
                         conPostingsNoReal  = conPostingsNoReal, 
                         
                         conWagesNoConsLOW  = conWagesNoConsLOW,
                         conWagesNoConsHIGH = conWagesNoConsHIGH,
                         conWagesNoPropLOW  = conWagesNoPropLOW, 
                         conWagesNoPropHIGH = conWagesNoPropHIGH,
                         conWagesNoRealLOW  = conWagesNoRealLOW,
                         conWagesNoRealHIGH = conWagesNoRealHIGH
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
                         
                         totalJobPostingsBusBa = formatCommas(totalJobPostingsBusBa),
                         
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
                         busAssociatesFin  = busAssociatesFin, 
                         busAssociatesLeg  = busAssociatesLeg, 
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
                         
                         busPostingsCeFin = formatCommas(busPostingsCeFin), 
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
                         
                         totalJobPostingsBusHi = formatCommas(totalJobPostingsBusHi),
                         
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
                         totalJobs   = formatCommas(totalJobsHealth),
                         degreeName6 = degreeName6,
                         degreeName5 = degreeName5,
                         degreeName4 = degreeName4, 
                         degreeName3 = degreeName3, 
                         degreeName2 = degreeName2, 
                         degreeName1 = degreeName1,
                         
                         # DOCTORAL 
                         healthDoctoralDire       = healthDoctoralDire,
                         healthDoctoralDiag       = healthDoctoralDiag,
                         healthDoctoralAdmi       = healthDoctoralAdmi, 
                         
                         totalJobPostingsHealthDo = totalJobPostingsHealthDo,
                         
                         healthPostingsDoDire     = healthPostingsDoDire, 
                         healthPostingsDoDiag     = healthPostingsDoDiag, 
                         healthPostingsDoAdmi     = healthPostingsDoAdmi, 
                         
                         
                         healthWagesDoDireLOW     = healthWagesDoDireLOW,
                         healthWagesDoDireHIGH    = healthWagesDoDireHIGH,
                         healthWagesDoDiagLOW     = healthWagesDoDiagLOW, 
                         healthWagesDoDiagHIGH    = healthWagesDoDiagHIGH,
                         healthWagesDoAdmiLOW     = healthWagesDoAdmiLOW,
                         healthWagesDoAdmiHIGH    = healthWagesDoAdmiHIGH,
                         
                         # MASTERS
                         healthMastersDire        = healthMastersDire,
                         healthMastersDiag        = healthMastersDiag,
                         healthMastersAdmi        = healthMastersAdmi, 
                         
                         totalJobPostingsHealthMa = totalJobPostingsHealthMa,
                         
                         healthPostingsMaDire     = healthPostingsMaDire, 
                         healthPostingsMaDiag     = healthPostingsMaDiag, 
                         healthPostingsMaAdmi     = healthPostingsMaAdmi, 
                         
                         
                         healthWagesMaDireLOW     = healthWagesMaDireLOW,
                         healthWagesMaDireHIGH    = healthWagesMaDireHIGH,
                         healthWagesMaDiagLOW     = healthWagesMaDiagLOW, 
                         healthWagesMaDiagHIGH    = healthWagesMaDiagHIGH,
                         healthWagesMaAdmiLOW     = healthWagesMaAdmiLOW,
                         healthWagesMaAdmiHIGH    = healthWagesMaAdmiHIGH,
                         
                         # Bachelors
                         healthBachelorsDire      = healthBachelorsDire,
                         healthBachelorsDiag      = healthBachelorsDiag,
                         healthBachelorsAdmi      = healthBachelorsAdmi, 
                         
                         totalJobPostingsHealthBa = formatCommas(totalJobPostingsHealthBa),
                         
                         healthPostingsBaDire     = formatCommas(healthPostingsBaDire), 
                         healthPostingsBaDiag     = formatCommas(healthPostingsBaDiag), 
                         healthPostingsBaAdmi     = formatCommas(healthPostingsBaAdmi), 
                         
                         
                         healthWagesBaDireLOW     = healthWagesBaDireLOW,
                         healthWagesBaDireHIGH    = healthWagesBaDireHIGH,
                         healthWagesBaDiagLOW     = healthWagesBaDiagLOW, 
                         healthWagesBaDiagHIGH    = healthWagesBaDiagHIGH,
                         healthWagesBaAdmiLOW     = healthWagesBaAdmiLOW,
                         healthWagesBaAdmiHIGH    = healthWagesBaAdmiHIGH,
                         
                         # ASSOCIATES
                         healthAssociatesDire     = healthAssociatesDire, 
                         healthAssociatesDiag     = healthAssociatesDiag, 
                         healthAssociatesAdmi     = healthAssociatesAdmi,
                         
                         totalJobPostingsHealthAs = totalJobPostingsHealthAs,
                         
                         healthPostingsAsDire     = healthPostingsAsDire, 
                         healthPostingsAsDiag     = healthPostingsAsDiag, 
                         healthPostingsAsAdmi     = healthPostingsAsAdmi, 
                         
                         healthWagesAsDireLOW     = healthWagesAsDireLOW,
                         healthWagesAsDireHIGH    = healthWagesAsDireHIGH,
                         healthWagesAsDiagLOW     = healthWagesAsDiagLOW, 
                         healthWagesAsDiagHIGH    = healthWagesAsDiagHIGH,
                         healthWagesAsAdmiLOW     = healthWagesAsAdmiLOW,
                         healthWagesAsAdmiHIGH    = healthWagesAsAdmiHIGH,
                         
                         # Certificate
                         healthCertificateDire    = healthCertificateDire, 
                         healthCertificateDiag    = healthCertificateDiag, 
                         healthCertificateAdmi    = healthCertificateAdmi, 
                         
                         totalJobPostingsHealthCe = formatCommas(totalJobPostingsHealthCe),
                         
                         healthPostingsCeDire     = healthPostingsCeDire, 
                         healthPostingsCeDiag     = formatCommas(healthPostingsCeDiag), 
                         healthPostingsCeAdmi     = healthPostingsCeAdmi,
                         
                         healthWagesCeDireLOW     = healthWagesCeDireLOW,
                         healthWagesCeDireHIGH    = healthWagesCeDireHIGH,
                         healthWagesCeDiagLOW     = healthWagesCeDiagLOW, 
                         healthWagesCeDiagHIGH    = healthWagesCeDiagHIGH,
                         healthWagesCeAdmiLOW     = healthWagesCeAdmiLOW,
                         healthWagesCeAdmiHIGH    = healthWagesCeAdmiHIGH,
                         
                         #High School
                         healthHighSchoolDire     = healthHighSchoolDire, 
                         healthHighSchoolDiag     = healthHighSchoolDiag, 
                         healthHighSchoolAdmi     = healthHighSchoolAdmi,
                         
                         totalJobPostingsHealthHi = formatCommas(totalJobPostingsHealthHi),
                         
                         healthPostingsHiDire     = formatCommas(healthPostingsHiDire), 
                         healthPostingsHiDiag     = formatCommas(healthPostingsHiDiag), 
                         healthPostingsHiAdmi     = formatCommas(healthPostingsHiAdmi),
                         
                         healthWagesHiDireLOW     = healthWagesHiDireLOW,
                         healthWagesHiDireHIGH    = healthWagesHiDireHIGH,
                         healthWagesHiDiagLOW     = healthWagesHiDiagLOW, 
                         healthWagesHiDiagHIGH    = healthWagesHiDiagHIGH,
                         healthWagesHiAdmiLOW     = healthWagesHiAdmiLOW,
                         healthWagesHiAdmiHIGH    = healthWagesHiAdmiHIGH
            ))
          
          output$mainData <- renderDataTable(
            DT::datatable(mainData, 
                          filter = 'bottom')

          )
          
          
  })

