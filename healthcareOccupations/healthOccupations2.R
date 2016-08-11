#load packages
library(dplyr)
library(stringr)

indianaOccupations  <- read.csv("indianaOccupations.csv")
kentuckyOccupations <- read.csv("kentuckyOccupations.csv")

indianaJobs         <- read.csv("indianaJobs.csv")        
kentuckyJobs        <- read.csv("kentuckyJobs.csv")

healthcareOccupations <- indianaOccupations
jobPostings           <- indianaJobs

#change name of jobPostings to match healthcare occupations for SOC code
#colnames(jobPostings)[1] <- "SOC"

#healthcareOccupations$SOC <- str_replace_all(healthcareOccupations$SOC, '-','')

healthcareOccupations$SOC <- as.character(healthcareOccupations$SOC)
jobPostings$SOC           <- as.character(jobPostings$SOC)


#merge healthcare and job posting data
occupationsJobPostings <- left_join(healthcareOccupations, jobPostings, by="SOC")

#Remove dollar signs
occupationsJobPostings$Pct..25.Hourly.Earnings <- str_replace_all(occupationsJobPostings$Pct..25.Hourly.Earnings, '\\$','')

#replace all <10 with 5
occupationsJobPostings <- as.data.frame(lapply(occupationsJobPostings, function(x) {
       gsub("<10", "5", x)
}))

#remove all commas
occupationsJobPostings <- as.data.frame(lapply(occupationsJobPostings, function(x) {
       gsub(",", "", x)
}))

#occupationsJobPostings$X2016.Jobs <- str_replace_all(occupationsJobPostings$Pct..25.Hourly.Earnings, ',','')


#change specific variables to numeric 
variables <- c("Pct..25.Hourly.Earnings", "X2016.Jobs", "X2026.Jobs", "Age.55.64", "Age.65.", "Job.Postings") 
occupationsJobPostings[variables]  <- lapply(occupationsJobPostings[variables], as.character)
occupationsJobPostings[variables]  <- lapply(occupationsJobPostings[variables], as.numeric)



#occupationsJobPostings$Pct..25.Hourly.Earnings <-  as.numeric(occupationsJobPostings$Pct..25.Hourly.Earnings)


#manipulate
occupationsJobPostings <- occupationsJobPostings %>%
       mutate(potientialRetirements2016.2026 = c(Age.55.64 + Age.65.)) %>%
       mutate(jobsAdded = c(X2026.Jobs-X2016.Jobs))%>%
       filter(Typical.Entry.Level.Education != "Master's degree" & Typical.Entry.Level.Education != "Doctoral or professional degree") %>%
       select(-Age.55.64, -Age.65., -Occupation, -X2016...2026.Change)

occupationsJobPostings <- occupationsJobPostings %>%
       mutate(JobGrowth2016.2026 = c(jobsAdded + potientialRetirements2016.2026))

occupationsJobPostings <- occupationsJobPostings %>%
       mutate(JobPostingRank = rank(desc(Job.Postings)))%>%
       mutate(GrowthRank = rank(desc(JobGrowth2016.2026)))

occupationsJobPostings <- occupationsJobPostings %>%
       mutate(Rank = (JobPostingRank + GrowthRank)/2)

highGrowthAbove12.5 <- occupationsJobPostings %>%
       filter(Pct..25.Hourly.Earnings > 12.5 & JobGrowth2016.2026 >= 200)

lowGrowthBelow12.5 <- occupationsJobPostings %>%
       filter(Pct..25.Hourly.Earnings < 12.5 & JobGrowth2016.2026 < 200)



#Export
write.csv(occupationsJobPostings, file = "healthOccupations.csv")
write.csv(highGrowthAbove12.5, file = "highGrowthAndWageHealthOccupations.csv")
write.csv(lowGrowthBelow12.5, file = "lowGrowthAndWageHealthOccupations.csv")
