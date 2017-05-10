library(choroplethr)
library(acs)
library(dplyr)
library(RCurl)
library(stringr)
library(choroplethr)
library(ggplot2)

dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1BKcuIbG4HsbCdAAQUGTWgRGyUkhwdnebcstP-Y-Lsjw/pub?gid=1258940477&single=true&output=csv")
dataConnectionTracts <- getURL("https://docs.google.com/spreadsheets/d/1BKcuIbG4HsbCdAAQUGTWgRGyUkhwdnebcstP-Y-Lsjw/pub?gid=1126114085&single=true&output=csv")
censusTracts <- read.csv(textConnection(dataConnectionTracts))
jeffCoData <- read.csv(textConnection(dataConnection))


colnames(jeffCoData)[1] <- "NAME"
## Merge data by tract number
test <- left_join(jeffCoData, censusTracts, by = "NAME")

df <- test %>% select(5,2)
colnames(df)[1] <- "region"
colnames(df)[2] <- "value"
df$value <- as.numeric(as.character(str_replace_all(df$value, ",", "")))




tract_choropleth(df, "kentucky", title = "", legend = "", num_colors = 5,
                 tract_zoom = NULL, county_zoom = 21111, reference_map = FALSE) +
                scale_fill_brewer(palette = 'Reds', direction = 1, name = "Labor Force 16 +")


# ky= get_tract_map("kentucky")
# 
# 
# all.tracts=geo.make(state="KY", county="Jefferson", 
#                       tract="*")
# acs.fetch(geography=all.tracts, table.name="Labor", endyear = 2015)
