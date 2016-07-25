#load packages
library(dplyr)
library(wordcloud)


#load data
hotelData <- read.csv("hotelDataAll.csv")

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

hotelData <- cleanData(hotelData)
hotelData$Frequency <- as.numeric(as.character(hotelData$Frequency))
hotelData$Occupation <- as.character(hotelData$Occupation)
hotelData <- hotelData %>%
       filter(Frequency>0)


percent <- function(data, n){
       data <-  data %>% mutate(percentage=n/sum(n))
       print(data)
}


hotelPercent <- percent(hotelData, hotelData$Frequency)

hotelSubset <- hotelPercent[1:4,]
sum(hotelSubset$percentage)

#Learning about wordcloud
pal <- brewer.pal(8,"Dark2")
#hotelData$Employed.in.Industry..2016. <- as.numeric(as.character(hotelData$Employed.in.Industry..2016.))
wordcloud(hotelData$Occupation, hotelData$Frequency, scale=c(2.1,.5), min.freq = 10, max.words = 50, random.color = TRUE,
          colors=pal, random.order = FALSE, rot.per=.1)

#wordcloud(words,freq,scale=c(3,2.5),min.freq=3,max.words=Inf,
#          random.order=TRUE, random.color=FALSE, rot.per=.1,
#          colors="black",ordered.colors=FALSE,use.r.layout=FALSE,
#          fixed.asp=TRUE, ...)



