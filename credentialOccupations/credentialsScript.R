library(dplyr)
library(stringr)

#Load all csv files
advancedCardiacLifeSupportAclsCertification             <- read.csv("Credentials/advancedCardiacLifeSupportAclsCertification.csv")
advancedPracticeNurse                                   <- read.csv("Credentials/advancedPracticeNurse.csv")
americanHeartAssociationCertificate                     <- read.csv("Credentials/americanHeartAssociationCertificate.csv")
americanHeartAssociationCertification                   <- read.csv("Credentials/americanHeartAssociationCertification.csv")
americanRegistryOfRadiologicTechnologistsARRT           <- read.csv("Credentials/americanRegistryOfRadiologicTechnologistsARRT.csv")
automotiveServiceExcellenceAseCertification             <- read.csv("Credentials/automotiveServiceExcellenceAseCertification.csv")
basicCardiacLifeSupportCertification                    <- read.csv("Credentials/basicCardiacLifeSupportCertification.csv")
cdlClassA                                               <- read.csv("Credentials/cdlClassA.csv")
certifiedAplusTechnician                                <- read.csv("Credentials/certifiedAplusTechnician.csv")
certifiedCaseManager                                    <- read.csv("Credentials/certifiedCasemanager.csv")
certifiedInformationSystemsSecurityProfessionalCISS     <- read.csv("Credentials/certifiedInformationSystemsSecurityProfessionalCISSP.csv")
certifiedManagementAccountant                           <- read.csv("Credentials/certifiedManagementAccountant.csv")
certifiedMedicalAssistant                               <- read.csv("Credentials/certifiedMedicalAssistant.csv")
certifiedNursingAssistant                               <- read.csv("Credentials/certifiedNursingAssistant.csv")
certifiedPharmacyTechnician                             <- read.csv("Credentials/certifiedPharmacyTechnician.csv")
certifiedPublicAccountantCPA                            <- read.csv("Credentials/certifiedPublicAccountantCPA.csv")
commercialDriversLicense                                <- read.csv("Credentials/commercialDriversLicense.csv")
emergencyMedicalTechnician                              <- read.csv("Credentials/emergencyMedicalTechnician.csv")
firstAidCprAed                                          <- read.csv("Credentials/firstAidCprAed.csv")
forkliftOperatorCertification                           <- read.csv("Credentials/forkliftOperatorCertification.csv")
homeHealthAide                                          <- read.csv("Credentials/homeHealthAide.csv")
licensedVocationalNurseLVN                              <- read.csv("Credentials/licensedVocationalNurseLVN.csv")
nursePractitioner                                       <- read.csv("Credentials/nursePractitioner.csv")
pharmacist                                              <- read.csv("Credentials/pharmacist.csv")
projectManagementCertificationPMP                       <- read.csv("Credentials/projectManagementCertificationPMP.csv")
registeredHealthInformationTechnician                   <- read.csv("Credentials/registeredHealthInformationTechnician.csv")
registeredNurses                                        <- read.csv("Credentials/registeredNurses.csv")
securityCredentials                                     <- read.csv("Credentials/securityCredentials.csv")
servSafe                                                <- read.csv("Credentials/servSafe.csv")
sixSigmaCertification                                   <- read.csv("Credentials/sixSigmaCertification.csv")


##function to clean data 
cleanData <- function(dataSet, totalJobs, credentialName){
       dataSet$Job.Postings <- str_replace_all(dataSet$Job.Postings, '\\%','')
       dataSet$Job.Postings <-  as.numeric(dataSet$Job.Postings)
       credentialName <- dataSet %>%
              mutate(PercentOccupations = Job.Postings/100)  %>%
              mutate(NumberOccupations = PercentOccupations*totalJobs)%>%
              mutate(Credential = credentialName)%>%
              select(Credential, SOC.Code, Occupation, PercentOccupations, NumberOccupations)
       credentialName$NumberOccupations <- format(round(credentialName$NumberOccupations))
       as.data.frame(credentialName)
}


advancedCardiacLifeSupportAclsCertification             <- cleanData(advancedCardiacLifeSupportAclsCertification,624,"Advanced Cardiac Life Support (ACLS) Certification")
advancedPracticeNurse                                   <- cleanData(advancedPracticeNurse, 245, "Advanced Practice Nurse")
americanHeartAssociationCertificate                     <- cleanData(americanHeartAssociationCertificate, 535, "American Heart Association Certificate")
americanHeartAssociationCertification                   <- cleanData(americanHeartAssociationCertification, 515, "American Heart Association Certification")
americanRegistryOfRadiologicTechnologistsARRT           <- cleanData(americanRegistryOfRadiologicTechnologistsARRT, 467, "American Registry of Radiologic Technologists (ARRT)")
automotiveServiceExcellenceAseCertification             <- cleanData(automotiveServiceExcellenceAseCertification, 346, "Automotive Service Excellence (ASE) Certification") 
basicCardiacLifeSupportCertification                    <- cleanData(basicCardiacLifeSupportCertification, 394, "Basic Cardiac Life Support Certification") 
cdlClassA                                               <- cleanData(cdlClassA, 3682, "CDL Class A") 
certifiedAplusTechnician                                <- cleanData(certifiedAplusTechnician, 271, "Certified A+ Technician")
certifiedCaseManager                                    <- cleanData(certifiedCaseManager, 287, "Certified Case Manager") 
certifiedInformationSystemsSecurityProfessionalCISS     <- cleanData(certifiedInformationSystemsSecurityProfessionalCISS, 232, "Certified Information Systems Security Professional (CISS)")
certifiedManagementAccountant                           <- cleanData(certifiedManagementAccountant, 280, "Certified Management Accountant")
certifiedMedicalAssistant                               <- cleanData(certifiedMedicalAssistant, 872, "Certified Medical Assistant")
certifiedNursingAssistant                               <- cleanData(certifiedNursingAssistant, 1214, "Certified Nursing Assistant")
certifiedPharmacyTechnician                             <- cleanData(certifiedPharmacyTechnician, 311, "Certified Pharmacy Technician")
certifiedPublicAccountantCPA                            <- cleanData(certifiedPublicAccountantCPA, 961, "Certified Public Accountant")
commercialDriversLicense                                <- cleanData(commercialDriversLicense, 1352, "Commercial Driver's License")
emergencyMedicalTechnician                              <- cleanData(emergencyMedicalTechnician, 471, "Emergency Medical Technician")
firstAidCprAed                                          <- cleanData(firstAidCprAed, 2941, "First Aid CPR AED")
forkliftOperatorCertification                           <- cleanData(forkliftOperatorCertification, 366, "Forklift Operator")
homeHealthAide                                          <- cleanData(homeHealthAide, 242, "Home Health Aide")
licensedVocationalNurseLVN                              <- cleanData(licensedVocationalNurseLVN, 239, "Licensed Vocational Nurse (LVN)")
nursePractitioner                                       <- cleanData(nursePractitioner, 284, "Nurse Practitioner")
pharmacist                                              <- cleanData(pharmacist, 233,"Pharmacist")
projectManagementCertificationPMP                       <- cleanData(projectManagementCertificationPMP, 677, "Project Management Certification (PMP)")
registeredHealthInformationTechnician                   <- cleanData(registeredHealthInformationTechnician, 254,"Registered Health Information Technician")
registeredNurses                                        <- cleanData(registeredNurses, 6752,"Registered Nurses")
securityCredentials                                     <- cleanData(securityCredentials, 532, "Security Credentials")
servSafe                                                <- cleanData(servSafe, 311, "Servsafe")
sixSigmaCertification                                   <- cleanData(sixSigmaCertification, 486, "Six Sigma Certification")



allData <- do.call("rbind", list(advancedCardiacLifeSupportAclsCertification, 
                                 advancedPracticeNurse, 
                                 americanHeartAssociationCertificate,
                                 americanHeartAssociationCertification, 
                                 americanRegistryOfRadiologicTechnologistsARRT, 
                                 automotiveServiceExcellenceAseCertification, 
                                 basicCardiacLifeSupportCertification, 
                                 cdlClassA, 
                                 certifiedAplusTechnician, 
                                 certifiedCaseManager, 
                                 certifiedInformationSystemsSecurityProfessionalCISS, 
                                 certifiedManagementAccountant, 
                                 certifiedMedicalAssistant, 
                                 certifiedNursingAssistant, 
                                 certifiedPharmacyTechnician, 
                                 certifiedPublicAccountantCPA, 
                                 commercialDriversLicense, 
                                 emergencyMedicalTechnician, 
                                 firstAidCprAed, 
                                 forkliftOperatorCertification, 
                                 homeHealthAide, 
                                 licensedVocationalNurseLVN, 
                                 nursePractitioner, 
                                 pharmacist, 
                                 projectManagementCertificationPMP, 
                                 registeredNurses, 
                                 securityCredentials, 
                                 servSafe, 
                                 sixSigmaCertification))

write.csv(allData, file = "allCredentials.csv")
       