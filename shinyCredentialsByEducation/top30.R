library(plyr)
#library(dplyr)
library(treemap)
library(googleVis)
library(RCurl)
library(ggplot2)



## LOAD DATA
credentialByEducationLevel   <- read.csv("credentialByEducation.csv")
sankey                       <- read.csv('sankey.csv')
majors                       <- read.csv('majors.csv')
employers                    <- read.csv('employers.csv')
louisville                   <- read.csv('louisvilleDataCerts.csv')

summary(louisville)
topCredentials               <- as.data.frame(summary(louisville$Certification))
topCredentials               <- na.omit(topCredentials)
colnames(topCredentials)     <- "Job Postings"


credentialsMedianWage <- read.csv("sankey.csv")
sixSigma <- c("LEAN SIX SIGMA", "SIX SIGMA BLACK BELT", "SIX SIGMA CERTIFICATION", "SIX SIGMA GREEN BELT", "SIX SIGMA MASTER BLACK BELT", "SIX SIGMA YELLOW BELT")
nursePracticionersAPRN <- c("NURSE PRACTITIONER", "ADVANCED PRACTICE NURSE")
calculateMedianGroup <- function(certificationName) {
  dataToCalculate <- credentialsMedianWage %>% 
    filter(Certification %in% certificationName)
  dataToCalculate <- na.omit(dataToCalculate)
  sum(dataToCalculate$Median.Hourly.Earnings)/nrow(dataToCalculate) }
calculateMedianGroup(sixSigma)

calculateMedian <- function(certificationName) {
                    dataToCalculate <- credentialsMedianWage %>% 
                      filter(Certification == certificationName)
                    dataToCalculate <- na.omit(dataToCalculate)
                    sum(dataToCalculate$Median.Hourly.Earnings)/nrow(dataToCalculate) }


calculateMedian("FIRST AID CPR AED")
calculateMedian("PROJECT MANAGEMENT CERTIFICATION (E.G. PMP)")
calculateMedian("ADVANCED CARDIAC LIFE SUPPORT (ACLS) CERTIFICATION")
calculateMedian("EMERGENCY MEDICAL TECHNICIAN (EMT)")
calculateMedian("BASIC CARDIAC LIFE SUPPORT CERTIFICATION")
calculateMedian("FORKLIFT OPERATOR CERTIFICATION")
calculateMedian("CERTIFIED PHARMACY TECHNICIAN")
calculateMedian("AUTOMOTIVE SERVICE EXCELLENCE (ASE) CERTIFICATION")
calculateMedianGroup(nursePracticionersAPRN)
calculateMedian("SERVSAFE")
calculateMedian("CERTIFIED CASE MANAGER")
calculateMedian("CERTIFIED A+ TECHNICIAN")
calculateMedian("REGISTERED HEALTH INFORMATION TECHNICIAN")
calculateMedian("CERTIFIED MANAGEMENT ACCOUNTANT (CMA)")
calculateMedian("CERTIFIED INFORMATION SYSTEMS SECURITY PROFESSIONAL (CISSP)")
calculateMedian("CISCO CERTIFIED NETWORK ASSOCIATE")
calculateMedian("LICENSED VOCATIONAL NURSE (LVN)")
calculateMedian("PHARMACIST")
calculateMedian("INSURANCE LICENSE")
calculateMedian("REGISTERED HEALTH INFORMATION ADMINISTRATOR")
calculateMedian("SOCIAL WORK LICENSE")
calculateMedian("CERTIFIED PROFESSIONAL CODER")
calculateMedian("PHLEBOTOMY CERTIFICATION")
calculateMedian("SERIES 6")
calculateMedian("CERTIFIED INFORMATION SYSTEMS AUDITOR (CISA)")
calculateMedian("CDL CLASS A")
calculateMedian("CERTIFIED PUBLIC ACCOUNTANT (CPA)")
calculateMedian("CERTIFIED NURSING ASSISTANT")


  test <- credentialsMedianWage %>% 
    filter(Certification == "PHLEBOTOMY CERTIFICATION")
  dataToCalculate <- na.omit(dataToCalculate)
  sum(dataToCalculate$Median.Hourly.Earnings)/nrow(dataToCalculate) }

employersList <- as.data.frame(unique(employers$Employer))
nrow(employersList)
