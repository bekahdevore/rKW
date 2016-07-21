nola <- subset(series, area_code == 31140)

nola <- merge(industry, nola, by="industry_code", all.y = TRUE)
nola <- merge(data_type, nola, by="data_type_code", all.y = TRUE)
nola <- merge(area, nola, by="area_code", all.y = TRUE)

nola <- nola[order(nola$industry_code),]
row.names(nola) <- NULL
remove(industry, data_type, area)

# Select only those series that represent the number of employees over time and a non-seasonal series
nola <- subset(nola, data_type_code == 1 & seasonal == "U")
series.nums <- unique(nola$series_id)

# Select the series id numbers for the New Orleans area
# Remove month 13 ("M13") observations, as these contain annual averages
data <- subset(data.18.Kentucky, series_id %in% series.nums & period != "M13")
remove(series.nums, series, data.18.Kentucky)

# Create a date variable that R recognizes as a date
data$month <- as.numeric(gsub("M" , "", data$period))
data$date <- paste(data$month, '15', sep="-")
data$date <- as.Date(paste(data$date, data$year, sep="-"), format='%m-%d-%Y ')

# Employees is measured in thousands
data$employees <- data$value * 1000

data <- merge(data, nola, by="series_id", all.x = TRUE)
data <- data[order(data$industry_code, data$date),]

save(data, file = "data.Rda")
save(nola, file = "nola.Rda")
