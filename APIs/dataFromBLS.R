library(dplyr)

# list of all datasets 
# https://download.bls.gov/pub/time.series/overview.txt

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
  filename <- gsub(pattern = "https://download.bls.gov/pub/time.series/sm/sm.","", x = web.address, ignore.case=T)
  
  # save the file to the global environment
  assign(filename, data, envir = .GlobalEnv) 
}

import.from.bls("https://download.bls.gov/pub/time.series/sm/sm.area") # area
import.from.bls("https://download.bls.gov/pub/time.series/sm/sm.data_type") # weekly hours, earnings, etc.
import.from.bls("https://download.bls.gov/pub/time.series/sm/sm.industry") # construction, manufacturing, etc.
import.from.bls("https://download.bls.gov/pub/time.series/sm/sm.series") # industry, datatype, etc. for ea. series
import.from.bls("https://download.bls.gov/pub/time.series/sm/sm.data.18.Kentucky") #Data