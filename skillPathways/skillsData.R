library(dplyr)
library(stringr)

skills         <- read.csv("skills.csv")
wagesEducation <- read.csv("skillsOccupationsWagesAndEducation.csv")


#Remove - from SOC in EMSI data
wagesEducation$SOC <- str_replace_all(wagesEducation$SOC, '-','')


skillsData <- merge(wagesEducation, skills, by="SOC")


write.csv(skillsData, file = "skillsData.csv")
