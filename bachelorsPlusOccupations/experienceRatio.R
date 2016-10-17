library(dplyr)

business                <- read.csv('businessLast90days.csv')
business3plusExperience <- read.csv('businessLast90days3plusExperience.csv')
majorSocCodeNames       <- read.csv('socMajorOccupationGroupsBLS_2010.csv')

all <- full_join(business, business3plusExperience, by = 'SOC')
splitSOC <- as.data.frame(t(sapply(all$SOC, function(x) substring(x, first=c(1, 1), last=c(2, 7)))))

colnames(splitSOC)[1] <- "socGroup"
colnames(splitSOC)[2] <- "SOC"
colnames(majorSocCodeNames)[1] <- 'socGroup'

all <- all %>% 
       select(1,2,3,5)

all <- full_join(all, splitSOC, by = 'SOC')

x <- count(all, socGroup, wt = Number.of.Job.Postings.x)
y <- count(all, socGroup, wt = Number.of.Job.Postings.y)

xy <- full_join(x, y, by = 'socGroup')

majorSocCodeNames$socGroup <- as.factor(majorSocCodeNames$socGroup)
xy$socGroup <- as.factor(xy$socGroup)

xy <- full_join(xy, majorSocCodeNames, by = 'socGroup')

xy$n.y <- as.numeric(as.character(xy$n.y))
xy$n.x <- as.numeric(as.character(xy$n.x))

xy$percentExperience <- (xy$n.y)/(xy$n.x)
colnames(xy)[1] <- 'SOC Code'
colnames(xy)[2] <- 'Total Job Postings'
colnames(xy)[3] <- ' 3+ years Experience Job Postings'

write.csv(xy, file = 'businessExperienceRatio.csv')
