library(RMySQL)
con <- dbConnect(MySQL(), group = "kwlmi", dbname = "kwlmi")

## Variables
kentucky <- "kentuckyPUMS"
peerCities <- "peerCityPUMS"
louisville <- "louisvilleMSA_PUMS"

statement <- function(place) {
  paste("SELECT *", "FROM", place, ";")
}


# Pull data from MySQL Database, change place argument in statement to run different different population data
kentucky <- dbGetQuery(conn = con, statement = statement(kentucky))
peerCities <- dbGetQuery(conn = con, statement = statement(peerCities))
louisvilleMSA <- dbGetQuery(conn = con, statement = statement(louisville))

kentuckyAllDataAllVariables <- kentucky
peerCitiesAllDataAllVariables <- peerCities
louisvilleAllDataAllVariables <- louisvilleMSA

save(kentuckyAllDataAllVariables, file = "kentuckyAllDataAllVariables.RData")
save(peerCitiesAllDataAllVariables, file = "peerCitiesAllDataAllVariables.RData")
save(louisvilleAllDataAllVariables, file = "louisvilleAllDataAllVariables.RData")
