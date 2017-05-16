library(dplyr)
library(RMySQL)

con <- dbConnect(MySQL(), group = "kwlmi", dbname = "kwlmi")