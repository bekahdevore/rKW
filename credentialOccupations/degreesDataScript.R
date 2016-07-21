library(dplyr)

#load data
highVocational     <- read.csv("../../rData/burningGlassCredentialsData/highSchoolVocational.csv", stringsAsFactors = FALSE)
associates         <- read.csv("../../rData/burningGlassCredentialsData/associates.csv", stringsAsFactors = FALSE)
bachelors          <- read.csv("../../rData/burningGlassCredentialsData/bachelors.csv", stringsAsFactors = FALSE)
graduate           <- read.csv("../../rData/burningGlassCredentialsData/graduateProfessional.csv", stringsAsFactors = FALSE)

#fucntion to add variable with name of education level 
educationLevel <- function(data, educationLevelName){
                     data <- data %>%
                            mutate(education = educationLevelName)
}

#add new variable to each dataset
highVocational       <- educationLevel(highVocational, "High School or Vocational")
associates           <- educationLevel(associates,     "Associate's Degree")
bachelors            <- educationLevel(bachelors,      "Bachelor's Degree")
graduate             <- educationLevel(graduate,       "Graduate or Professional Degree")


#merge data
#not currently working
schoolLevelData <- do.call("rbind", list(
                                          highVocational, 
                                          associates, 
                                          bachelors, 
                                          graduate
                                          )) 
                                             
#save as csv document
write.csv(schoolLevelData, file = "schoolLevelData.csv")

                                            