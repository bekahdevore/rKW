library(dplyr)
library(plotly)
library(treemap)
library(ggplot2)


allOccupations       <- read.csv('last90daysAll.csv')
businessOccupations  <- read.csv('last90daysBusiness.csv')
majorSocCodeNames    <- read.csv('socMajorOccupationGroupsBLS_2010.csv')

businessThreeNinety <- read.csv('business3plusLast90days.csv')
allThreeNinety      <- read.csv('all3plusLast90days.csv')

#majorSocCodeNames$SOC <- as.numeric(as.character(majorSocCodeNames$SOC))
################################## FUNCTIONS ###########################################
       selectFilter <- function(dataName) {dataName %>% 
                            select(SOC, Occupation, Number.of.Job.Postings)}
       
       removeCommas <- function(dataName) {as.data.frame(lapply(dataName, function(x) {
                            gsub(",", "", x)}))
       }

cleanData <- function(dataName) {
                     dataName <- selectFilter(dataName)
                     dataName <- removeCommas(dataName)
}

       percent       <- function(data){
              data <-  data %>% mutate(per=n/sum(n))
              print(data)
       }

countPercent <- function(dataName) {count(dataName, wt = Number.of.Job.Postings)
       x <- count(dataName, Occupation, wt= Number.of.Job.Postings, sort = TRUE) #weight by Job Postings
       percent(x)}


       filterPercent <- function(dataName) {dataName %>%
                     filter(per >= .01)}


pieChart      <- function(dataName, chartTitle) {
                     dataName <- filterPercent(dataName)
                     
                     plot_ly(dataName, labels = Occupation, values = n, type = "pie") %>%
                     layout(title = chartTitle)
}

treeMapMaker  <- function(dataName) {
                     dataName <- filterPercent(dataName)     
       
                     treemap(dataName, index = c('label'), vSize = 'n', 
                     fontsize.labels = 18,
                     border.col = 0,
                     title = "",
                     fontface.labels = 1,
                     fontfamily.labels = "sans")}

#############################################################################################################################
########################################## 3 to 5 year Stacked Bar Graph Data Manipulation ####################################
business            <- right_join(businessOccupations, majorSocCodeNames, by = 'SOC')
all                 <- right_join(allOccupations, majorSocCodeNames, by = 'SOC') 
businessThreeNinety <- right_join(businessThreeNinety, majorSocCodeNames, by = 'SOC')
allThreeNinety      <- right_join(allThreeNinety, majorSocCodeNames, by = 'SOC')


businessThreeNinety <- cleanData(businessThreeNinety)
allThreeNinety      <- cleanData(allThreeNinety)
business            <- cleanData(business)
all                 <- cleanData(all)


all$Number.of.Job.Postings      <- as.numeric(as.character(all$Number.of.Job.Postings))
business$Number.of.Job.Postings <- as.numeric(as.character(business$Number.of.Job.Postings))
allThreeNinety$Number.of.Job.Postings      <- as.numeric(as.character(allThreeNinety$Number.of.Job.Postings))
businessThreeNinety$Number.of.Job.Postings <- as.numeric(as.character(businessThreeNinety$Number.of.Job.Postings))


allThreeNinety      <- countPercent(allThreeNinety)
businessThreeNinety <- countPercent(businessThreeNinety)
all                 <- countPercent(all)
business            <- countPercent(business)

business <- full_join(business, businessThreeNinety, by = 'Occupation')
all      <- full_join(all, allThreeNinety, by = 'Occupation')
######################################### ORIGNIAL OUTPUT ####################################################
write.csv(all, file = 'all.csv')
write.csv(business, file = 'business.csv')

all      <- cleanData(all)
business <- cleanData(business)

all$Number.of.Job.Postings      <- as.numeric(as.character(all$Number.of.Job.Postings))
business$Number.of.Job.Postings <- as.numeric(as.character(business$Number.of.Job.Postings))

all      <- countPercent(all)
business <- countPercent(business)

all$numberLabel <- format(all$n,
                          big.mark   = ",", 
                          scientific = FALSE)

all$label       <- paste(all$Occupation, 
                         all$numberLabel,
                         sep = "\n")

business$numberLabel <- format(business$n,
                               big.mark   = ",", 
                               scientific = FALSE)

business$label       <- paste(business$Occupation, 
                              business$numberLabel,
                              sep = "\n")


pieChart(all, "All Occupations, Bachelor's or higher")
pieChart(business, 'Business')


treeMapMaker(all)
treeMapMaker(business)


