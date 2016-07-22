#load packages
library(dplyr); library(googleVis)

#Load data
kyPums <- read.csv("pumsKY.csv")
socNames <- read.csv("socCodeTitleCrosswalk.csv")

#Louisville PUMA's
pums14 <- c(1701, 1702, 1703, 1704, 1705, 1800)


###########################################Functions##############################################
#Weigthed Count Function
majorToOccupationDataCount <- function(majorCode, majorName){
       workforce <- (kyPums %>%
                            filter(PUMA %in% pums14)   %>% ## filter to Louisville MSA (approximate)
                            filter(SOCP != 395012)     %>%
                            filter(FOD1P == majorCode) %>% ## filter to pop 16 and over
                            
                            select(PUMA, FOD1P, OCCP, SOCP, PWGTP)) ## Select variables of interest 
       
       workforce <- merge(workforce, socNames, by= "SOCP")
       
       businessMajorsOccuptations <- count(workforce, title, wt=PWGTP, sort = TRUE)
}

#Percent function
percent <- function(data){
       data <-  data %>% mutate(per=n/sum(n))
       print(data)
}

#Data clean and visualize function
majorToOccupationData <- function(majorCode, majorName){
       workforce <- (kyPums %>%
                            filter(PUMA %in% pums14)   %>% ## filter to Louisville MSA (approximate)
                            filter(FOD1P == majorCode) %>% ## filter to pop 16 and over
                            filter(SOCP != 395012)     %>%
                            select(PUMA, FOD1P, OCCP, SOCP, PWGTP)) ## Select variables of interest 
       
       workforce <- merge(workforce, socNames, by= "SOCP")
       
       businessMajorsOccuptations <- count(workforce, title, wt=PWGTP, sort = TRUE)
       
       businessMajorsOccuptations <- businessMajorsOccuptations %>%
                                          mutate(major = majorName) %>%
                                          select(major, title, n)
      
       businessMajorsOccuptationsSankey <- plot(gvisSankey(
              businessMajorsOccuptations, 
              from="major",
              to="title",
              weight = "n",
              options=list(height =1200,
                           width = 800, 
                           sankey="{node:{label:{fontSize:14}}}"
               )))
}


#Sankey
generalBusiness <- majorToOccupationData(6200, "General Business")
##BusinessManagement <- majorToOccupationData(6203, "Business Management and Administration")

#Count
generalBusiness <- majorToOccupationDataCount(6200, "General Business")
generalBusiness

#Percents 
generalBusiness <- percent(generalBusiness)

