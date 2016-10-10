library(dplyr)
library(plotly)
library(treemap)


allOccupations       <- read.csv('last90daysAll.csv')
businessOccupations  <- read.csv('last90daysBusiness.csv')
majorSocCodeNames    <- read.csv('socMajorOccupationGroupsBLS_2010.csv')

#majorSocCodeNames$SOC <- as.numeric(as.character(majorSocCodeNames$SOC))

business <- right_join(businessOccupations, majorSocCodeNames, by = 'SOC')
all      <- right_join(allOccupations, majorSocCodeNames, by = 'SOC') 


################################## FUNCTIONS ###########################################
       selectFilter <- function(dataName) {dataName %>% 
                            select(SOC, Occupation, Number.of.Job.Postings)}
       
       removeCommas <- function(dataName) {as.data.frame(lapply(dataName, function(x) {
                            gsub(",", "", x)}))
       }

cleanData <- function(dataName) {
                     selectFilter(dataName)
                     removeCommas(dataName)
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
                     layout(title = chartTitle)}

treeMapMaker  <- function(dataName) {
                     dataName <- filterPercent(dataName)              
       
                     treemap(dataName, index = c('Occupation'), vSize = 'n', 
                     fontsize.labels = 18,
                     border.col = 0,
                     title = "",
                     fontface.labels = 1,
                     fontfamily.labels = "sans")}
#############################################################################################################################


all      <- cleanData(all)
business <- cleanData(business)

all$Number.of.Job.Postings      <- as.numeric(as.character(all$Number.of.Job.Postings))
business$Number.of.Job.Postings <- as.numeric(as.character(business$Number.of.Job.Postings))

all      <- countPercent(all)
business <- countPercent(business)


######################################### OUTPUT ####################################################
write.csv(all, file = 'all.csv')
write.csv(business, file = 'business.csv')


pieChart(all, "All Occupations, Bachelor's or higher")
pieChart(business, 'Business')


treeMapMaker(all)
treeMapMaker(business)


