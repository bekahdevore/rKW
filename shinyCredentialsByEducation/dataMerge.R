library(dplyr)
library(RCurl)

socNamesConnection   <- getURL('https://docs.google.com/spreadsheets/d/1wWVpXkU7OG2dGjCEEOK4Z4sS02tgK9_zee9cl0MdQRE/pub?gid=0&single=true&output=csv')
emsiWageConnection   <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ5dubxmVxSbxynxKG26BCRQK-9Zx6Yodzc5oEYPoSLdlUCPxzPWnEEucvDuornbObHCEaVC_OuOqE9/pub?gid=0&single=true&output=csv")
socNames             <- read.csv(textConnection(socNamesConnection))

wageRanges           <- read.csv(textConnection(emsiWageConnection))
louisvilleDataCerts  <- read.csv('louisvilleDataCerts.csv')

#cip                 <- read.csv('louisvilleDataCIP.csv')
#major               <- read.csv('louisvilleDataStdMajor.csv')

#certifications      <- louisvilleDataCerts %>%
#                                 select(2, 36)

colnames(socNames)[1] <- "socGroup"
socNames      <- socNames %>% select(1:2)

wageRanges    <- select(wageRanges, 1:2, 4:6, 5)

wageRanges                    <- as.data.frame(lapply(wageRanges, function(x) { gsub('\\$', '', x )}))
wageRanges                    <- as.data.frame(lapply(wageRanges, function(x) { gsub('Insf. Data', '0', x )}))

variables <- c('Pct..25.Hourly.Earnings',
               'Median.Hourly.Earnings',
               'Pct..75.Hourly.Earnings')

wageRanges[,variables] <- lapply(wageRanges[,variables] , as.character)
wageRanges[,variables] <- lapply(wageRanges[,variables] , as.numeric)

wageRanges$Pct..25.Hourly.Earnings <- paste("$", (format(round((wageRanges$Pct..25.Hourly.Earnings * 2080), 0), big.mark = ',')), sep = '')
wageRanges$Median.Hourly.Earnings  <- round((wageRanges$Median.Hourly.Earnings * 2080), 0)
wageRanges$Pct..75.Hourly.Earnings <- paste("$", (format(round((wageRanges$Pct..75.Hourly.Earnings * 2080), 0), big.mark = ',')), sep = '')

## CREDENTIAL BY EDUCATION LEVEL
#credentialByEducationLevel <- louisvilleDataCerts %>%
#                                 select(2, 20:23, 36)

#credentialByEducationLevel <- na.omit(credentialByEducationLevel)
#summary(credentialByEducationLevel)

#write.csv(credentialByEducationLevel, file = "credentialByEducation.csv")


## CREDENTIAL TO OCCUPATION SANKEY
credentialsToOccupations <- louisvilleDataCerts %>% select(2, 5:6, 22, 36)
credentialsToOccupations$Certification <- as.character(credentialsToOccupations$Certification)

#credentialsToOccupations[, 5][is.na(credentialsToOccupations[, 5])] <- "No Certification"
credentialsToOccupations$t <- 1

countCredentials <- count(credentialsToOccupations, Certification, wt = t, sort = TRUE)
sum(countCredentials)

credentialsToOccupations <- na.omit(credentialsToOccupations)

credentialsToOccupations <- credentialsToOccupations %>% filter(SOC != "na")

credentialsToOccupations <- credentialsToOccupations %>% 
                                      group_by(SOC, Certification) %>%
                                      tally  %>%
                                      group_by(SOC) 
 
# Seperate first two numbers of SOC codes and put in new variable
splitSOC <- as.data.frame(t(sapply(credentialsToOccupations$SOC, function(x) substring(x, first=c(1, 1), last=c(2, 7)))))

# Change column names 
colnames(splitSOC)[1] <- "socGroup"
colnames(splitSOC)[2] <- "SOC"

splitSOC <- splitSOC %>% select(2, 1)
splitSOC <- unique(splitSOC)

credentialsToOccupations <- left_join(credentialsToOccupations, splitSOC, by = 'SOC')
  credentialsToOccupations$socGroup <- as.character(credentialsToOccupations$socGroup)
  socNames$socGroup                 <- as.character(socNames$socGroup)
  
credentialsToOccupations <- left_join(credentialsToOccupations, socNames, by = "socGroup")

socNames4Digit    <- louisvilleDataCerts %>% select(5:6)
socNames4Digit    <- socNames4Digit      %>% filter(SOC != 'na')
socNames4Digit    <- unique(socNames4Digit)

credentialsToOccupations <- left_join(credentialsToOccupations, socNames4Digit, by = "SOC")
credentialsToOccupations <- credentialsToOccupations %>% filter(socGroup != 55)

credentialsToOccupations <- left_join(credentialsToOccupations, wageRanges, by = 'SOC')

credentialsToOccupations$label <- paste(credentialsToOccupations$SOCName, 
                                        "(", 
                                        credentialsToOccupations$Pct..25.Hourly.Earnings, "-",
                                        credentialsToOccupations$Pct..75.Hourly.Earnings,
                                        ")",
                                        sep = ' ') 

#credentialsToOccupations <- credentialsToOccupations %>% select(7, 10, 2:3, 5:4, 1)
credentialsToOccupations <- credentialsToOccupations %>% select(11, 2:5, 9, 1)


write.csv(credentialsToOccupations, file = "sankey.csv")
######## For Data including job postings with no credential specification
#write.csv(credentialsToOccupations, file = "sankeyAll.csv")

#colnames(credentialsToOccupations)[1] <- 'source'
#colnames(credentialsToOccupations)[2] <- 'target'
#colnames(credentialsToOccupations)[3] <- 'value'

#credentialsToOccupations <- credentialsToOccupations %>%
#                                  select(1:3)

#write.csv(credentialsToOccupations, file = "sankeyD3.csv")


#cip             <- cip %>%
#                    select(2, 4)
# Remove NA
#certifications <- na.omit(certifications)

#louisvilleDataAll <- left_join(louisvilleDataCerts)

