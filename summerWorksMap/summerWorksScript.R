library(choroplethr)
library(choroplethrZip)
library(RCurl)
library(dplyr)
library(DT)


summerWorksZipsConnection <- getURL('https://docs.google.com/spreadsheets/d/1az6CvuDSvUJfDqvgQZBe2pbm92-BDqhEYGmVvIvka0I/pub?gid=0&single=true&output=csv')
summerWorksZips           <- read.csv(textConnection(summerWorksZipsConnection), check.names = FALSE)
summerWorksZips$ZipCode <- as.character(summerWorksZips$ZipCode)

test <- count(summerWorksZips, ZipCode, wt = N)
colnames(test)[1] <- 'region'
colnames(test)[2] <- 'value'

zip_choropleth(test, 
               title="Summer Works",
               msa_zoom = "Louisville/Jefferson County, KY-IN",
               legend="Summer Works Employee")

datatable(test)
