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