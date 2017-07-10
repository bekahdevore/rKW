## FUNCTIONS
library(XML)

currentQuarter <- function(currentMonth){
  if (currentMonth %in% q1) 
    1
  else if (currentMonth %in% q2)
    2
  else if (currentMonth %in% q3)
    3
  else 
    4
}
count <- 1
q1 <- 1:3
q2 <- 4:6
q3 <- 7:9
q4 <- 10:12

#currentMonth <- as.numeric(format(Sys.Date(), "%m"))
currentYear <- as.numeric(format(Sys.Date(), "%Y"))
#quarter <- currentQuarter(currentMonth)
quarter <-  4
count <- 0

getQcewData <- function(quarter, currentYear) {
  count = count + 1
  dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", quarter, "/area/C3114.csv")
  dataHere <- read.csv(textConnection(getURL(dataURL)))
  if (ncol(dataHere) > 2) return (dataHere)
  else if (count > 4)  { 
      currentYear = currentYear - 1 
      for (i in 1:4) {
        getQcewData(i, currentYear)
        }
  }
    else {
      for (i in 1:4) {
        getQcewData(i, currentYear)
      }
    }
  }

qcewDataLouisville <- getQcewData(quarter, currentYear)


