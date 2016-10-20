library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)
library(scales)
library(RCurl)
library(ggthemes)

#Load data, upgrade to pull from google sheets
zeroTo2   <- getURL('https://docs.google.com/spreadsheets/d/1c_FBfXw_Oq-p5ocYx5wnHqnA-7dTI01tx7J7YszytsI/pub?gid=0&single=true&output=csv')
threeTo5  <- getURL('https://docs.google.com/spreadsheets/d/10TEwPj-fRlb0kaCFJM86pudOpKObacBXhl4I4Cz6r90/pub?gid=0&single=true&output=csv')     
other     <- getURL('https://docs.google.com/spreadsheets/d/1P9jRvModlmzReivw9Gljt-qv9T1hlW9brhTEPgvMRoI/pub?gid=0&single=true&output=csv')
soc       <- getURL('https://docs.google.com/spreadsheets/d/1wWVpXkU7OG2dGjCEEOK4Z4sS02tgK9_zee9cl0MdQRE/pub?gid=0&single=true&output=csv')

business0to2      <- read.csv(textConnection(zeroTo2))
business3to5      <- read.csv(textConnection(threeTo5))
businessOther     <- read.csv(textConnection(other))
majorSocCodeNames <- read.csv(textConnection(soc))

# https://docs.google.com/spreadsheets/d/1c_FBfXw_Oq-p5ocYx5wnHqnA-7dTI01tx7J7YszytsI/pub?gid=0&single=true&output=csv

#business3to5       <- read.csv('business3to5.csv')
#businessOther      <- read.csv('businessOther.csv')
#majorSocCodeNames  <- read.csv('socMajorOccupationGroupsBLS_2010.csv')

#Merge data
all <- full_join(business0to2, business3to5, by = 'SOC')
all <- full_join(all, businessOther,         by = 'SOC')

#Seperate first two numbers of SOC codes and put in new variable
splitSOC <- as.data.frame(t(sapply(all$SOC, function(x) substring(x, first=c(1, 1), last=c(2, 7)))))
       #Change column names 
       colnames(splitSOC)[1] <- "socGroup"
       colnames(splitSOC)[2] <- "SOC"
       colnames(majorSocCodeNames)[1] <- 'socGroup'

#filter to necessary variables only 
all <- all %>% 
       select(1,2,3,5,7)

#Add soc group codes       
all <- full_join(all, splitSOC, by = 'SOC')

#Count by Soc group for each level of experience
socGroup0to2 <- count(all, socGroup, wt = zeroTo2yearsExperience)
       colnames(socGroup0to2)[2] <- "0-2 years"
socGroup3to5 <- count(all, socGroup, wt = threeToFiveYearsExperience)
       colnames(socGroup3to5)[2] <- "3-5 years"
socGroupOther <- count(all, socGroup, wt = Other)
       colnames(socGroupOther)[2] <- "Other"
 
#Create data table with count of experience levels by the major soc group     
xy <- full_join(socGroup0to2, socGroup3to5, by = 'socGroup')
xy <- full_join(xy, socGroupOther, by = 'socGroup')

#Convert Soc Groups to factors
majorSocCodeNames$socGroup <- as.factor(majorSocCodeNames$socGroup)
xy$socGroup <- as.factor(xy$socGroup)

#Join to SOC code names 
xy <- left_join(xy, majorSocCodeNames, by = 'socGroup')

xy <- xy %>%
       filter(socGroup != 27 & socGroup != 53 & socGroup != 19 
              & socGroup != 23 & socGroup != 35 & socGroup != 33
              & socGroup != 25 & socGroup != 25 & socGroup != 21)

#change to numeric 
xy$`0-2 years` <- as.numeric(as.character(xy$`0-2 years`))
xy$`3-5 years` <- as.numeric(as.character(xy$`3-5 years`))
xy$Other <- as.numeric(as.character(xy$Other))



#Add calculation for percentages
xy$zeroTo2percent  <- (xy$`0-2 years`)/(xy$`0-2 years` + xy$`3-5 years` + xy$Other)
xy$threeTo5percent <- (xy$`3-5 years`)/(xy$`0-2 years` + xy$`3-5 years` + xy$Other)
xy$otherPercent    <- (xy$Other)/(xy$`0-2 years` + xy$`3-5 years` + xy$Other)


rawData     <- xy %>%
                select(1:5)

percentData <- xy %>%
                select(1, 5:8)

rawData     <- melt(rawData)
percentData <- melt(percentData)

allData <- cbind(percentData, rawData)

colnames(allData)[1] <- 'SOC'
colnames(allData)[2] <- 'Occupations'
colnames(allData)[3] <- 'Percent Type'
colnames(allData)[4] <- 'Percent'
colnames(allData)[6] <- 'occ'
colnames(allData)[7] <- 'Experience'
colnames(allData)[8] <- 'Jobs'

allData <- allData %>%
       select(1:4, 7:8)
totals <- count(allData, Occupations, wt = Jobs, sort = TRUE)
allData <- full_join(allData, totals, by = 'Occupations')
allData <- allData %>%
              arrange(n)

Occupation <- reorder(allData$Occupations, allData$n)
#Visualize
g <- ggplot(allData, aes(x = Occupation, 
                            y = Jobs, 
                            fill = Experience, 
                            label = Percent)) +      
              geom_bar(stat = 'identity') +
              labs(x = '', 
                   y = 'Number of Job Postings') +
              coord_flip() +
              theme_minimal()
ggplotly(g)


#+ scale_y_continuous(labels = percent)

#plot_ly(all,  x = ~zeroTo2yearsExperience, y = ~Occupation.Title.x, type = 'bar', name = '0 - 2 years experience') %>%
#       add_trace(x = ~threeToFiveYearsExperience, name = '3 - 5 years experience') %>%
#       layout(yaxis = list(title = 'Count'), barmode = 'stack') + 

write.csv(xy, file = 'businessExperienceRatio.csv')
