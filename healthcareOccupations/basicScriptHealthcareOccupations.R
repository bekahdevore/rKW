#load packages
library(dplyr)
library(stringr)

healthcareOccupations <- read.csv("healthcareOccupationsPart2.csv")
jobPostings           <- read.csv("jobPostingsAug2015-July2016.csv")

#change name of jobPostings to match healthcare occupations for SOC code
#colnames(jobPostings)[1] <- "SOC"

healthcareOccupations$SOC <- str_replace_all(healthcareOccupations$SOC, '-','')

healthcareOccupations$SOC <- as.character(healthcareOccupations$SOC)
jobPostings$SOC <- as.character(jobPostings$SOC)


#merge healthcare and job posting data
occupationsJobPostings <- left_join(healthcareOccupations, jobPostings, by="SOC")


write.csv(occupationsJobPostings, file = "occupationsJobPostings.csv")
