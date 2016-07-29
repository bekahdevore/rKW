library(stringr)

pumsCipSoc$Median.Hourly.Earnings <- str_replace_all(pumsCipSoc$Median.Hourly.Earnings, '\\$','')
pumsCipSoc$X2016.Jobs             <- str_replace_all(pumsCipSoc$X2016.Jobs, '<10','0')
pumsCipSoc$X2016.Jobs             <- str_replace_all(pumsCipSoc$X2016.Jobs, ',','')

pumsCipSoc$Median.Hourly.Earnings <- as.numeric(pumsCipSoc$Median.Hourly.Earnings)
pumsCipSoc$X2016.Jobs             <- as.numeric(as.character(pumsCipSoc$X2016.Jobs))

#Ranking for Median Wage
rankingMedian <- pumsCipSoc[order(pumsCipSoc$PUMS, -xtfrm(pumsCipSoc$Median.Hourly.Earnings)), ]
rankingMedian$Median <- sequence(rle(r$PUMS)$lengths)

#Ranking for Jobs
rankingJobs <- pumsCipSoc[order(pumsCipSoc$PUMS -xtfrm(pumsCipSoc$X2016.Jobs)), ]
rankingJobs$jobs <- sequence(rle(r$PUMS)$lengths)

newData <- merge(rankingJobs, rankingMedian, by="CIP")
newData <- newData %>%
              mutate(cummulativeRank = (jobs+Median)/2)

df.agg <- aggregate(cummulativeRank ~ PUMS.x, newData, max)
newData <- merge(newData, df.agg)
newData <- newData %>%
                     select(PUMS.x, CIP)

pumsNames <- read.csv("pumsDegree.csv")
newData   <- merge(pumsNames, newData, by="PUMS.x")



write.csv(newData, file = "pumsCipSocRanking01.csv")
