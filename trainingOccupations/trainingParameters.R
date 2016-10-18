library(dplyr)
library(stringr)

occupations <- read.csv("allOccupations.csv")

#Remove dollar signs
occupations$Pct..25.Hourly.Earnings <- str_replace_all(occupations$Pct..25.Hourly.Earnings, '\\$','')

#Get rid of na entries
dd <- is.na(occupations)
occupations[dd] <- 0 ##change NA to 0

#Change to character in order to change to numeric in next function
occupations[,"X2016...2026.Change"] <- (as.numeric(as.character(occupations[,"X2016...2026.Change"])))


#Change variables to numeric
x <- c("Pct..25.Hourly.Earnings", "Age.55.64", "Age.65." )
occupations[,x] <- sapply(occupations[,x], as.numeric)




occupationsGrowth <-
  occupations %>% 
    mutate(growthPlusRetirements = c(Age.55.64 + Age.65.+ X2016...2026.Change))%>% #Sum 55-64, 65 plus, Job change
    mutate(education = ifelse(
                          Typical.Entry.Level.Education == "Bachelor's degree" | 
                          Typical.Entry.Level.Education == "High school diploma or equivalent" |
                          Typical.Entry.Level.Education == "Associate's degree" | 
                          Typical.Entry.Level.Education == "No formal educational credential" | 
                          Typical.On.The.Job.Training =="Postsecondary nondegree award" | 
                          Typical.Entry.Level.Education == "Some college, no degree" |
                          Typical.On.The.Job.Training == "Apprenticeship", 1, 0
                      )) %>%
   filter(growthPlusRetirements >= 10) %>%
   filter(education > 0)

occupationsGrowth$Pct..25.Hourly.Earnings <-  sprintf("$ %2.2f", occupationsGrowth$Pct..25.Hourly.Earnings)
#Create code to chang variable names

save(occupationsGrowth, file = "data.Rda")



    


