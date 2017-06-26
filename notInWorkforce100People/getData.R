## LOAD PACKAGES
library(RMySQL)

## CONNECT TO DATA, MySQL Database, pull peer cities data
con <- dbConnect(MySQL(), group = "kwlmi", dbname = "kwlmi")

peerCities <- "peerCityPUMS"

statement <- function(place) {
  paste("SELECT *", "FROM", place, ";")
}

peerCities <- dbGetQuery(conn = con, statement = statement(peerCities))
save(peerCities, file = "peerCities.RData")
