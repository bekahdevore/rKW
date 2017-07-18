## BLS IMPORT
####  IMPORT FROM BLS FUNCTION ##
# https://download.bls.gov/pub/time.series/overview.txt

## Grab  MSA area codes from google sheets
areaCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
peerAreaCodes <- read.csv(textConnection(areaCodeConnection))

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

## BLS OES imports 
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current", "oesMainData")


################# DATA MANIPULTION ##################
##### BLS OES DATA MANIPULATION ####
## Create new variables (from series_id) that contains the area code and datapoint
oesMainData[,"area_code"] <- substr(oesMainData[,"series_id"], 7,11)
oesMainData[,"datapoint"] <- substr(oesMainData[,"series_id"], 24,25)
oesMainData[,"industry"] <- substr(oesMainData[,"series_id"], 15,23)

## Change vairables to numeric
oesMainData[,"area_code"] <- as.numeric(as.character(oesMainData[,"area_code"]))
#oesMainData[,"datapoint"] <- as.numeric(as.character(oesMainData[,"datapoint"]))

oesMainData <- oesMainData %>% 
  filter((datapoint == "13" & (area_code %in% peerAreaCodes$area_code) & industry == "000000000")) %>% 
  select(4:5)
oesMainData[,1] <- as.numeric(as.character(oesMainData[,1]))
colnames(oesMainData)[1] <- "Annual Median Wage (USD)"

write.csv(oesMainData, file = "medianWage.csv")

