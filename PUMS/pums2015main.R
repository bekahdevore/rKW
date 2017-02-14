# Connect to PUMA's data list on googlesheets https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv
# read IN and KY .csv documents 
# filter data by PUMA's to estimate Louisville MSA 
# only select variables of interest
# format for buttefly chart in tableau
# Export to googlesheet 

library(RCurl)
library(dplyr)
library(stringr)


pumaConnection   <- getURL('https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv')

pumas            <- read.csv(textConnection(pumaConnection), check.names = FALSE)
indianaPUMS      <- read.csv('ss15pin.csv')
kentuckyPUMS     <- read.csv('ss15pky.csv')

indianaPUMS      <- indianaPUMS %>%
                      filter(indianaPUMS$PUMA  %in% pumas$inPUMA)
kentuckyPUMS     <- kentuckyPUMS %>%
                      filter(kentuckyPUMS$PUMA %in% pumas$kyPUMA)

pums2015        <- rbind(indianaPUMS, kentuckyPUMS)


df.expanded <- pums2015[rep(row.names(pums2015), pums2015$PWGTP), 1:100]
df.expanded <- df.expanded %>% filter(AGEP >= 25 & WAGP != 0) %>% select(SCHL, NATIVITY, WAGP)
df.expanded <- df.expanded %>% mutate(livingWage = ifelse(WAGP >= 47273, 1, 0))
df.expanded <- df.expanded %>% select(SCHL, NATIVITY, livingWage)

write.csv(df.expanded, file = 'pums2015expanded.csv')



expandedData <- pums2015[rep(row.names(pums2015), pums2015$PWGTP), 1:100]
expandedData <- expandedData %>% filter(AGEP >= 25 & WAGP != 0) %>% select(SCHL, NATIVITY, WAGP)
expandedData <- expandedData %>% mutate(Wage = ifelse(WAGP >= 47273, "Above", "Below"))
# expandedData <- expandedData %>% select(SCHL, NATIVITY, "Family Supporting Wage")
expandedData <- expandedData %>% mutate(Education = ifelse(SCHL <= 20, "No BA", "BA+")) %>%
                                  mutate(Origin = ifelse(NATIVITY == 1, "Native", "Foreign Born"))

mosaic(~ Education + Origin + Wage, data = expandedData, highlighting = "Wage", 
       highlighting_fill = c("lightblue", "pink"), 
       direction=c("v", "h", "v"))

plot3d(expandedData$WAGP, expandedData$SCHL, expandedData$NATIVITY)
       
# highlighting = livingWage, highlighting_fill=c("lightblue", "pink"))
### PRACTICE 3D VISUALIZATIONS
emsiConnection <- getURL("https://docs.google.com/spreadsheets/d/1Fkj4ZBHwdopMh0mdjwWj1qN-2AIUCpSYijBiagcISpk/pub?gid=0&single=true&output=csv")
emsiData       <- read.csv(textConnection(emsiConnection))

#Change variables to numeric
x <- c("X2016.Jobs", "X2026.Jobs", "Median.Hourly.Earnings")
emsiData$Median.Hourly.Earnings <- str_replace_all(emsiData$Median.Hourly.Earnings, '\\$','')
emsiData$X2016.Jobs <- str_replace_all(emsiData$X2016.Jobs, ",", "")
emsiData$X2026.Jobs <- str_replace_all(emsiData$X2026.Jobs, ",", "")

emsiData[,x] <- sapply(emsiData[,x], as.character)
emsiData[,x] <- sapply(emsiData[,x], as.numeric)

plot3d(emsiData$X2016.Jobs, (emsiData$X2026.Jobs - emsiData$X2016.Jobs), emsiData$Median.Hourly.Earnings)

play3d(spin3d(rpm = 2))

# OUTPUT TOO LARGE 
# Connect to Google Sheet
#pums2015louisvilleMSA <- gs_title('pums2015louisvilleMSA')
#pums2015louisvilleMSA <- pums2015louisvilleMSA %>% 
#                              gs_edit_cells(input  = pums2015,
#                                            anchor = "A1", 
#                                            byrow  = TRUE)
