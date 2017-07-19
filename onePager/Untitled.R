
function(tableCode, area, currentYear, dataPath) {
  dataHere <- getURL(paste("http://api.census.gov/data/", currentYear, "/acs1/subject?get=NAME,", tableCode, area, "&key=", apiKey))
  dataHere <- read.csv(textConnection(dataHere))
  if (nrow(dataHere) > 2) return (dataHere)
  for (i in currentYear:2015) {
    dataHere <- getURL(paste("http://api.census.gov/data/", i, "/acs1/", dataPath, "?get=NAME,", tableCode, area, "&key=", apiKey))
    dataHere <- read.csv(textConnection(dataHere))
    if (nrow(dataHere) > 2) return (dataHere)
  }
}