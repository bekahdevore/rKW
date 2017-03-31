library(RCurl)
library(dplyr)


dataConnection0 <- getURL("https://docs.google.com/spreadsheets/d/1hWZkSbvYFLtacYxejrT0S8ZeIIW3aoqK1sBSEeo7_mU/pub?gid=0&single=true&output=csv") #Institution List
dataConnection1 <- getURL("https://docs.google.com/spreadsheets/d/1OmzP29Seq_FWwDJM8y3AUEbVqjE4pWayK4D19ryxfJY/pub?gid=0&single=true&output=csv") #Accredidation L ist

institutions <- read.csv(textConnection(dataConnection0)) 
accredidations <- read.csv(textConnection(dataConnection1))

accredidations <- accredidations %>% select(1:2) 
accredidations <- accredidations[1:101,]

accreditedList <- institutions %>% filter(Institution %in% accredidations$Institution)


write.csv(accreditedList, file = "accreditedSchools.csv")
