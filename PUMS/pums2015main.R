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

pums2015        <- pums2015 %>%
                    select(PWGTP, AGEP, RAC1P, SCHL, PERNP)


# OUTPUT TOO LARGE 
# Connect to Google Sheet
#pums2015louisvilleMSA <- gs_title('pums2015louisvilleMSA')
#pums2015louisvilleMSA <- pums2015louisvilleMSA %>% 
#                              gs_edit_cells(input  = pums2015,
#                                            anchor = "A1", 
#                                            byrow  = TRUE)
