#load packages
library(dplyr)

#load data
kyPums        <- read.csv("ss14pky.csv")
inPums        <- read.csv("ss14pin.csv")

#Filter by PUMA, approximate Louisville MSA
kyPuma        <- c(1701, 1702, 1703, 1704, 1705, 1800)
inPuma        <- c(3400, 3300)



pumaFilter    <- function(pumaList, pumsData){
       pumsData %>%
                     filter(PUMA %in% pumaList) %>% ## filter to Louisville MSA (approximate)
                     filter(AGEP >= 16) %>% ## filter to pop 16 and over
                     select(PUMA, 
                            ESR, 
                            AGEP, 
                            PWGTP, 
                            WAGP, 
                            SEX, 
                            AGEP, 
                            ENG, 
                            SOCP, 
                            OCCP, 
                            NAICSP,  
                            NATIVITY)
}

indiana       <- pumaFilter(inPuma, inPums)
kentucky      <- pumaFilter(kyPuma, kyPums)
louisvilleMSA <- rbind(indiana, kentucky)

#Filter by occupation (SOCP) 
       #Seperate by nativity status
       #calculate percent of workers by occupation or industry that are foreign born


#Filter by industry (NAICSP)
       #Seperate by nativity status
       #calculate percent of workers by occupation or industry that are foreign born

