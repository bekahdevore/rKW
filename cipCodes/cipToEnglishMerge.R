library(RCurl)
library(dplyr)

englishConnection <- getURL("https://docs.google.com/spreadsheets/d/1MTATzPuk6kTHNKMt4qJOHyMUO8LZ0kNZLADh2ezeMBk/pub?gid=0&single=true&output=csv")
spanishConnection <- getURL("https://docs.google.com/spreadsheets/d/1MTATzPuk6kTHNKMt4qJOHyMUO8LZ0kNZLADh2ezeMBk/pub?gid=564916124&single=true&output=csv")

english <- read.csv(textConnection(englishConnection))
spanish <- read.csv(textConnection(spanishConnection))

allData <- left_join(english, spanish, by = "CIP")

write.csv(allData, file = "cipCodesEnEs.csv")
