library(choroplethr)
library(choroplethrMaps)
library(dplyr)
library(RCurl)
library(RColorBrewer)
library(ggplot2)

data(df_pop_county)
county_choropleth(df_pop_county, state_zoom="kentucky")

allData <- getURL("https://docs.google.com/spreadsheets/d/1RzHGJzcDiUwVWKXLHNjEpgqf877zBg9GvpCdIGIo9io/pub?gid=0&single=true&output=csv")
allData <- read.csv(textConnection(allData))

data(county.regions)

counties <- county.regions %>% filter(state.name == "kentucky")
lou_county_names = c("bullitt", "henry", "jefferson", "oldham", "shelby", "spencer", "trimble")
lou_county_fips <-  county.regions %>%
  filter(state.name == "kentucky" & county.name %in% lou_county_names) #%>%

county_choropleth(df_pop_county,
                  legend = "Population",
                  num_colors = 7,
                  county_zoom = lou_county_fips$region)

  #select(region)
population <- allData %>% filter(Datapoint == "population") %>% select(1, 3)
unemploymentRate <- allData %>% filter(Datapoint == "unemploymentRate") %>% select(1, 3)
medicaid <- allData %>% filter(Datapoint == "medicaid") %>% select(1, 3)
workforce <- allData %>% filter(Datapoint == "workforce") %>% select(1, 3)

county_choropleth(unemploymentRate,
                  legend = "Unemployment Rate",
                  num_colors = 7,
                  county_zoom = population$region) + scale_fill_brewer(palette = "Oranges")

county_choropleth(medicaid,
                  legend = "Medicaid",
                  num_colors = 7,
                  county_zoom = population$region) + scale_fill_brewer(palette = "Greys")

county_choropleth(workforce,
                  legend = "Workforce",
                  num_colors = 7,
                  county_zoom = population$region) + scale_fill_brewer(palette = "Reds")


