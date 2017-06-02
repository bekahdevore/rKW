# library(devtools)
# install_github("mikeasilva/blsAPI")
library(blsAPI)
library(RCurl)
library(RJSONIO)
library(ggplot2)
library(dplyr)
library(scales)

####Load and clean data
#Start and End Years for BLS data and Month to Filter
startYear <- 2007
endYear   <- 2017
#blsMonth  <- 'March' Now automatic, code found at end of file under 'Filter for specified month' comment
apiKey    <- '2691a2506f514617823b4e653111fdc9'
#,'startyear'= startYear, 'endyear' = endYear
#Load data and parse it out with RJSONIO
#BLS series LNS14000000 returns the Seasonally Adjusted Unemployment Rate for individuals 16 and over.

###########################################################################################################
## FUNCTIONS ##
###########################################################################################################
#Convert Data from BLS API to DataFrame
apiDataConverter <- function(seriesId) {
  payload     <- list('seriesid'        = seriesId,
                      'startyear'       = startYear, 
                      'endyear'         = endYear, 
                      'registrationKey' = apiKey)
  bls.content <- blsAPI(payload)
  bls.json    <- fromJSON(bls.content, simplify=TRUE)
  tmp         <- bls.json$Results[[1]][[1]]
  bls.df      <- data.frame(year=sapply(tmp$data,"[[","year"),
                            period=sapply(tmp$data,"[[","period"),
                            periodName=sapply(tmp$data,"[[","periodName"),
                            value=as.numeric(sapply(tmp$data,"[[","value")),
                            stringsAsFactors=FALSE)
}

###########################################################################################################
## CREATE DATAFRAMES AND FORMAT ##
###########################################################################################################

#### Labor Force Series ID's ###
JeffersonCountyLaborForce <- apiDataConverter("LAUCN211110000000006")
KentuckyLaborForce        <- apiDataConverter("LAUST210000000000006")
LouisvilleLaborForce      <- apiDataConverter("LAUMT213114000000006")
UnitedStatesLaborForce    <- apiDataConverter("LNU01000000")