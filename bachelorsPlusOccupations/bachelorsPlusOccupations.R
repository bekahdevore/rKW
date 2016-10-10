library(dplyr)
library(plotly)
library(treemap)


allOccupations       <- read.csv('allOccupationsSummary2.csv')
businessOccupations <- read.csv('businessBachelorsPlusSummaryLouisville.csv')
majorSocCodeNames   <- read.csv('socMajorOccupationGroupsBLS_2010.csv')

#majorSocCodeNames$SOC <- as.numeric(as.character(majorSocCodeNames$SOC))

business <- right_join(businessOccupations, majorSocCodeNames, by = 'SOC')
all      <- right_join(allOccupations, majorSocCodeNames, by = 'SOC') 


################################## FUNCTIONS ###########################################
selectFilter <- function(dataName) {dataName %>% 
              select(SOC, Occupation, Number.of.Job.Postings)}

filterPercent <- function(dataName) {dataName %>%
              filter(per >= .01)}

pieChart <- function(dataName, chartTitle) {plot_ly(dataName, labels = Occupation, values = n, type = "pie") %>%
              layout(title = chartTitle)}

treeMapMaker <- treemap(pieBusiness, index = c('Occupation'), vSize = 'n', 
                        fontsize.labels = 12,
                        border.col = 0,
                        title = "",
                        fontface.labels = 1,
                        fontfamily.labels = "sans")


percent <- function(data){
       data <-  data %>% mutate(per=n/sum(n))
       print(data)
}


all      <- selectFilter(all)
business <- selectFilter(business)



business <- as.data.frame(lapply(business, function(x) {
       gsub(",", "", x)
}))

business$Number.of.Job.Postings <- as.numeric(as.character(business$Number.of.Job.Postings))



all <- as.data.frame(lapply(all, function(x) {
       gsub(",", "", x)
}))
all$Number.of.Job.Postings <- as.numeric(as.character(all$Number.of.Job.Postings))



count(all, wt = Number.of.Job.Postings)
x <- count(all, Occupation, wt= Number.of.Job.Postings, sort = TRUE) #weight by Job Postings
percent(x)

countPercent <- function(dataName) {count(dataName, wt = Number.of.Job.Postings)
                     x <- count(dataName, Occupation, wt= Number.of.Job.Postings, sort = TRUE) #weight by Job Postings
                     percent(x)}

all <- countPercent(all)
business <- countPercent(business)

write.csv(all, file = 'all.csv')
write.csv(business, file = 'business.csv')





pieChart(pieAll, "All Occupations, Bachelor's or higher")
pieChart(pieBusiness, 'Business')




