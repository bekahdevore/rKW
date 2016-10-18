#load packages
library(dplyr)
library(wordcloud)
library(plotrix)


#load data
hotelData <- read.csv("hotelIndustryData.csv")

cleanData <- function(dataName){
       dataName <- as.data.frame(lapply(dataName, function(x) {
              gsub(",", "", x)
       }))
       
       dataName <- as.data.frame(lapply(dataName, function(x) {
              gsub("<10", "2.21434", x)
       }))
       
       dataName <- as.data.frame(lapply(dataName, function(x) {
              gsub("\\$", "", x)
       }))
}

hotelData                              <- cleanData(hotelData)
hotelData$Employed.in.Industry..2016.  <- as.numeric(as.character(hotelData$Employed.in.Industry..2016.))
hotelData$Description                  <- as.character(hotelData$Description)

hotelData <- hotelData %>%
                     filter(Employed.in.Industry..2016.>0)


percent <- function(data, n){
       data <-  data %>% mutate(percentage=n/sum(n))
       print(data)
}

hotelPercent <- percent(hotelData, hotelData$Employed.in.Industry..2016.)

#Top Four Perce
hotelSubset <- hotelPercent[1:4,]
sum(hotelSubset$percentage)

count(hotelData$Typical.Entry.Level.Education, wt=hotelData$Employed.in.Industry..2016.)

#Pie Chart 
pie(hotelData$Typical.Entry.Level.Education, labels = hotelData$Typical.Entry.Level.Education)
pieData <- count(hotelData, Typical.Entry.Level.Education, wt=Employed.in.Industry..2016.)

percent <- function(data){
       data <-  data %>% mutate(per=n/sum(n))
       print(data)
}

percent(pieData)
pieData$n
pieData$Typical.Entry.Level.Education <-  as.character(pieData$Typical.Entry.Level.Education)
x <- pieData$n
y <- pieData$Typical.Entry.Level.Education
pie(x, y, col=heat.colors(length(lbls)) )

#http://www.statmethods.net/graphs/pie.html

#Learning about wordcloud
pal <- brewer.pal(8,"Dark2")
#hotelData$Employed.in.Industry..2016. <- as.numeric(as.character(hotelData$Employed.in.Industry..2016.))
wordcloud(hotelData$Occupation, hotelData$Frequency, scale=c(2.1,.5), min.freq = 10, max.words = 50, random.color = TRUE,
              colors=pal, random.order = FALSE, rot.per=.1)

#wordcloud(words,freq,scale=c(3,2.5),min.freq=3,max.words=Inf,
#          random.order=TRUE, random.color=FALSE, rot.per=.1,
#          colors="black",ordered.colors=FALSE,use.r.layout=FALSE,
#          fixed.asp=TRUE, ...)



