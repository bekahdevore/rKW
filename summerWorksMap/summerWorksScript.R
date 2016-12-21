library(choroplethr)
library(choroplethrZip)
library(RCurl)
library(dplyr)
library(DT)
library(ggplot2)


summerWorksZipsConnection <- getURL('https://docs.google.com/spreadsheets/d/1az6CvuDSvUJfDqvgQZBe2pbm92-BDqhEYGmVvIvka0I/pub?gid=0&single=true&output=csv')
#jeffersonZipsConnection   <- getURL('https://docs.google.com/spreadsheets/d/1EtBsLE6qklYWulDIIokcqte-Lsy71Noat2vjJzUT5kg/pub?gid=0&single=true&output=csv')

summerWorksZips           <- read.csv(textConnection(summerWorksZipsConnection), check.names = FALSE)
#jeffersonZips             <- read.csv(textConnection(jeffersonZipsConnection),   check.names = FALSE)

summerWorksZips$ZipCode   <- as.character(summerWorksZips$ZipCode)
#jeffersonZips$Zip         <- as.character(jeffersonZips$Zip)

#summerWorks               <- summerWorksZips %>%
#                                filter(summerWorksZips$ZipCode %in% jeffersonZips$Zip)

test <- count(summerWorksZips, ZipCode, wt = N)
colnames(test)[1] <- 'region'
colnames(test)[2] <- 'value'

zip_choropleth(test, 
               title="",
               county_zoom = 21111) + scale_fill_brewer(name = "Population") + scale_fill_brewer(palette = 'Reds', direction = -1)

zip_choropleth(test, 
               title="",
               msa_zoom="Louisville/Jefferson County, KY-IN") + scale_fill_brewer(name = "Population") + scale_fill_brewer(palette = 'Reds', direction = -1)


datatable(test)


