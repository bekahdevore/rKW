library(dplyr)
library(plotly)
areaCodes <- c('CN2111100000000', 'MT2131140000000')

metaData <- series%>%
              filter(area_code %in% areaCodes)%>%
              select(area_code, series_id, series_title, measure_code)


metaData <- merge(measure, metaData, by="measure_code", all.y = TRUE)
metaData <- merge(area, metaData, by="area_code", all.y = TRUE)

row.names(metaData) <- NULL

# Select the series id numbers for the Louisville area
data <- data.24.Kentucky %>%
            filter(series_id == metaData$series_id )%>%
            filter(period != "M13")
data <- merge(data, metaData, by="series_id")


# Create a date variable that R recognizes as a date
data$month <- as.numeric(gsub("M" , "", data$period))
data$date <- paste(data$month, '15', sep="-")
data$date <- as.Date(paste(data$date, data$year, sep="-"), format='%m-%d-%Y ')


save(data, file = "data.Rda")
save(metaData, file = "metaData.Rda")

unemploymentData <- data %>%
                      filter(measure_text == "unemployment rate")

ggplot(unemploymentData)
