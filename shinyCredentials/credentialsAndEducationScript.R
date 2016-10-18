library(dplyr)

credentials <- read.csv("allcredentials.csv")
education   <- read.csv("schoolLevelData.csv")

#change variable names
colnames(credentials)[2]    <- "source"
colnames(credentials)[4]    <- "target"
colnames(credentials)[6]    <- "weight"

colnames(education)[3]      <- "source"
colnames(education)[5]      <- "target"
colnames(education)[4]      <- "weight"

selectData <- function(dataName){
       dataName <- dataName %>%
                     select(source, target, weight)
}

credentials <- selectData(credentials)
education   <- selectData(education)
#merge data
credentialsAndEducation <- rbind(credentials, education)

write.csv(credentialsAndEducation, file = "credentialsAndEducation.csv")
