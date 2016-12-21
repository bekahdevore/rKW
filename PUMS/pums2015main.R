# Connect to PUMA's data list on googlesheets https://docs.google.com/spreadsheets/d/1LlC-Nwa_ntWM0kE_vWcjtcNSS3Q9I2mBb-RvO_id614/pub?gid=0&single=true&output=csv
# read IN and KY .csv documents 
# filter data by PUMA's to estimate Louisville MSA 
# only select variables of interest
# format for buttefly chart in tableau
# Export to googlesheet 

library(RCurl)
library(dplyr)


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

pums2015        <- pums2015 %>% select(PWGTP, AGEP, RAC1P, SCHL, PERNP)


# OUTPUT TOO LARGE 
# Connect to Google Sheet
#pums2015louisvilleMSA <- gs_title('pums2015louisvilleMSA')
#pums2015louisvilleMSA <- pums2015louisvilleMSA %>% 
#                              gs_edit_cells(input  = pums2015,
#                                            anchor = "A1", 
#                                            byrow  = TRUE)
