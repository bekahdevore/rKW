#load packages
library(stringr)

#import data 
laborForce         <- read.csv("laborForce.csv")
unemploymentRate   <- read.csv("unemploymentRate.csv")

#Function to remove unecessary characters: (R), (D). and (P)
replaceCharacters  <- function(dataName) {
                            as.data.frame(lapply(dataName, function(x) {
                            gsub("\\(R)|\\(D)|\\(P)", "", x)
                            }))
                     }

#Remove characters from datasets
laborForce       <- replaceCharacters(laborForce)
unemploymentRate <- replaceCharacters(unemploymentRate)