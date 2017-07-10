library(dplyr)

#### BLS OES MEDIAN WAGE DATA ####
# list of all datasets 
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

import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.series")
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current")
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.area")

import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.60.Metro", "metros") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/la/la.series", "lausSeries") # BLS LAUS METROPOLITAN DATA


allData <- left_join(data.0.Current, series, by = "series_id")
allData <- left_join(allData, area, by = "area_code")

save(allData, file = "allData.RData")