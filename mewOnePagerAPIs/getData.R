## OES imports 
#import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.series", "oesSeries")
import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current", "oesMainData")
# import.from.bls("https://download.bls.gov/pub/time.series/oe/oe.area", "oesArea")

## LAUS imports
import.from.bls("https://download.bls.gov/pub/time.series/la/la.data.60.Metro", "lausMetros") # BLS LAUS METROPOLITAN DATA
import.from.bls("https://download.bls.gov/pub/time.series/la/la.series", "lausSeries") # BLS LAUS METROPOLITAN DATA

## Grab  MSA area codes from google sheets
areaCodeConnection <- getURL("https://docs.google.com/spreadsheets/d/1MIgcX_LQBF2J2pzevXRPJzy9UKjaVls9vNda3Pgay3Q/pub?gid=0&single=true&output=csv")
peerAreaCodes <- read.csv(textConnection(areaCodeConnection))


# allData <- left_join(data.0.Current, series, by = "series_id")
# allData <- left_join(allData, area, by = "area_code")
# 
# save(allData, file = "allData.RData")