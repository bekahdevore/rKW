library(dplyr)

business                <- read.csv('businessLast90days.csv')
business3plusExperience <- read.csv('businessLast90days3plusExperience.csv')
majorSocCodeNames       <- read.csv('socMajorOccupationGroupsBLS_2010.csv')

all <- full_join(business, business3plusExperience, by = 'SOC')
splitSOC <- as.data.frame(t(sapply(all$SOC, function(x) substring(x, first=c(1, 1), last=c(2, 7)))))

colnames(splitSOC)[1] <- "socGroup"
colnames(splitSOC)[2] <- "SOC"
colnames(majorSocCodeNames)[]

all <- full_join(all, splitSOC, by = 'SOC')
all <- rbind(all, majorSocCodeNames)


all <- all %>% 
              select(1,2,3,5)

colnames(all)[1]