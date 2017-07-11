## FUNCTIONS

getQcewData <- function(quarter, currentYear) {
  dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", quarter, "/area/C3114.csv")
  dataHere <- read.csv(textConnection(getURL(dataURL)))
  if (ncol(dataHere) > 2) return (dataHere)
  else { 
    for (i in 4:1) {
      dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", i, "/area/C3114.csv")
      dataHere <- read.csv(textConnection(getURL(dataURL)))
      if (ncol(dataHere) > 2) return (dataHere)
    }
    currentYear = currentYear - 1 
    for (i in 4:1) {
      dataURL <- paste0("https://data.bls.gov/cew/data/api/", currentYear, "/", i, "/area/C3114.csv")
      dataHere <- read.csv(textConnection(getURL(dataURL)))
      if (ncol(dataHere) > 2) return (dataHere)
    }
  }
}

getAcsData <- function()








