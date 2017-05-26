library(dplyr)

# list of everything in directory
# http://download.bls.gov/pub/time.series/sm/

import.from.bls <- function(web.address) {
  
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
  filename <- gsub(pattern = "https://download.bls.gov/pub/time.series/oe/oe.","", x = web.address, ignore.case=T)
  
  # save the file to the global environment
  assign(filename, data, envir = .GlobalEnv) 
}

import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.series")
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current")
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.area")

allData <- left_join(data.0.Current, series, by = "series_id")
allData <- left_join(allData, area, by = "area_code")

save(allData, file = "allData.RData")
