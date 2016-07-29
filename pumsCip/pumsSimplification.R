library(stringr)

maxRows <- by(pumsCipSoc, pumsCipSoc$PUMS, function(X) X[which.max(X$Median.Hourly.Earnings),])
test <- merge(max)

pumsCipSoc$Median.Hourly.Earnings <- str_replace_all(pumsCipSoc$Median.Hourly.Earnings, '\\$','')
pumsCipSoc$Median.Hourly.Earnings <- as.numeric(pumsCipSoc$Median.Hourly.Earnings)

df.agg <- aggregate(Median.Hourly.Earnings ~ PUMS, pumsCipSoc, max)
pumsCipSoc <- merge(pumsCipSoc, df.agg)
