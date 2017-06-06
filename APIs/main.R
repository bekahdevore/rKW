louData <- subset(series, area_code == 31140)

louData <- merge(industry, louData, by = "industry_code", all.y = TRUE)
louData <- merge(data_type, louData, by = "data_type_code", all.y = TRUE)
louData <- merge(area, louData, by = "area_code", all.y = TRUE)

louData <- louData[order(louData$industry_code),]
row.names(louData) <- NULL
remove(industry, data_type, area)

louData <- subset(louData, data_type_code == 1 & seasonal == "U")
series.nums <- unique(louData$series_id)

data <- subset(data.18.Kentucky, series_id %in% series.nums & period == "M13")
data$employees <- data$value * 1000

data <- merge(data, louData, by = "series_id", all.x = TRUE)
data <- data[order(data$industry_code),]

save(data, file = "industryData.RData")

