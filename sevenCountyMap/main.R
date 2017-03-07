library(choroplethr)
library(choroplethrMaps)
library(dplyr)

data(df_pop_county)
county_choropleth(df_pop_county, state_zoom="kentucky")


data(county.regions)

counties <- county.regions %>% filter(state.name == "kentucky")
lou_county_names = c("bullitt", "henry", "jefferson", "oldham", "shelby", "spencer", "trimble")
lou_county_fips <-  county.regions %>%
  filter(state.name == "kentucky" & county.name %in% lou_county_names) %>%
  select(region)
county_choropleth(df_pop_county,
                  legend = "Population",
                  num_colors = 7,
                  county_zoom = lou_county_fips$region)
