## Where is this folder?
getwd() # Get working directory

## Assigning variables
x <- 1
## using variables
x*2

# lists
words <- c("these", "are", "words")
words <- as.data.frame(words)
names(words)
row.names(words)

# Accessing variables
words$newColumn <- "words"
words$numbers <- 1:3

# Basic math
sum(words$numbers)
mean(words$numbers)
median(words$numbers)

## Upload data from .csv document
myData <- read.csv("myData.csv")

## Upload data from google sheet, 
library(RCurl) ## add packages

dataConnection <- getURL("")
myData <- read.csv(textConnection(dataConnection))

## Write a function
multiplyNumberBy5 <- function(number) {
      number <- number *  5
      return(number)
}





