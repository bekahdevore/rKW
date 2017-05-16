library(RMySQL)
library(psych)

con <- dbConnect(MySQL(), group = "kwlmi", dbname = "kwlmi")

ss15pky <- read.csv('ss15pky.csv')

dbWriteTable(conn = con, name = 'ss15pky', value = ss15pky)

retrieveData <- dbGetQuery(conn = con, statement = "SELECT AGEP,SCHL,RAC1P FROM ss15pin;")
retrieveTable <- dbReadTable(conn = con, name = "ss15pky")

dbGetQuery(conn = con, statement = "SELECT Sentences FROM Test WHERE Sentences > 0.5;")
