library(dplyr)

emsiOccupations <- read.csv("emsiOccupationWages.csv")
pumsCip         <- read.csv("pumsToCip.csv")
socCip          <- read.csv("SOCtoCIP.csv")

temp <- merge(pumsCip, socCip, by="CIP")
pumsCipSoc <- merge(temp, emsiOccupations, by="SOC")

pumsCipSoc <- pumsCipSoc %>%
                     select(SOC, CIP, PUMS, CIP.Title, ONETTitle, Median.Hourly.Earnings, X2016.Jobs)

write.csv(pumsCipSoc, file = "pumsCipSoc.csv")