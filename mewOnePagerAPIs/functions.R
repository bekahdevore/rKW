## FUNCTIONS

# GET CURRENT BLS QCEW DATA
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

## BLS IMPORT
####  IMPORT FROM BLS FUNCTION ##
# https://download.bls.gov/pub/time.series/overview.txt

import.from.bls <- function(web.address, filenameInput) {
  
  # Import data
  temp <- tempfile()
  download.file(web.address, temp)
  data <- read.table(temp,
                     header=FALSE,
                     sep="\t",
                     skip=1,
                     stringsAsFactors=FALSE,
                     strip.white=TRUE,
                     quote = NULL)
  
  # Add column headers
  topline <- readLines(web.address)
  topline <- topline[1] # Select column headers
  topline <- as.list(strsplit(x = topline, split = "\t")[[1]]) # Split the string into a list
  colnames(data) <- topline
  
  unlink(temp)
  
  # Drop extra, unused column
  data <- data[,1:(ncol(data) - 1)]
  
  # This command plucks the text that appears after the pattern below,
  # and uses it to name the file.
  #filename <- gsub(pattern = "https://download.bls.gov/pub/time.series/","", x = web.address, ignore.case=T)
  
  # save the file to the global environment
  assign(filenameInput, data, envir = .GlobalEnv) 
}


## ACS DATA
getDataMetros <- function(tableCode, area, currentYear, dataPath) {
  dataHere <- getURL(paste("http://api.census.gov/data/", currentYear, "/acs1/subject?get=NAME,", tableCode, area, "&key=", apiKey))
  dataHere <- read.csv(textConnection(dataHere))
  if (nrow(dataHere) > 2) return (dataHere)
  for (i in currentYear:2015) {
    dataHere <- getURL(paste("http://api.census.gov/data/", i, "/acs1/", dataPath, "?get=NAME,", tableCode, area, "&key=", apiKey))
    dataHere <- read.csv(textConnection(dataHere))
    if (nrow(dataHere) > 2) return (dataHere)
  }
}

cleanAcsData <- function(dataHere, dataPointName){
  dataHere[,"X..NAME"] <- str_replace_all(dataHere[,"X..NAME"], "\\[", "")
  dataHere[,"metropolitan.statistical.area.micropolitan.statistical.area."] <- str_replace_all(dataHere[,"metropolitan.statistical.area.micropolitan.statistical.area."], "\\]", "")
  colnames(dataHere)[3] <- "msaCode"
  colnames(dataHere)[2] <- dataPointName
  dataHere[,"msaCode"] <- as.numeric(as.character(dataHere[,"msaCode"])) 
  dataHere <- dataHere %>% select(1:3)
  return (dataHere)
}





### ACS TESTING
# tableCode <- "DP04_0089E" 
# area <- acsMetros
# dataHere <- getURL(paste("http://api.census.gov/data/2015/acs1/profile?get=NAME,", tableCode, area, "&key=", apiKey))
# dataHere <- read.csv(textConnection(dataHere))



