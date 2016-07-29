#load packages
library(dplyr)
library(cwhmisc)
library(tidyr)

#load data
kyPums        <- read.csv("ss14pky.csv")
inPums        <- read.csv("ss14pin.csv")
socNames      <- read.csv("socCodeTitleCrosswalk.csv")
naicsNames    <- read.csv("naicsNames.csv")

#Filter by PUMA, approximate Louisville MSA
kyPuma        <- c(1701, 1702, 1703, 1704, 1705, 1800)
inPuma        <- c(3400, 3300)

pumaFilter    <- function(pumaList, pumsData){
       pumsData %>%
              filter(PUMA %in% pumaList) %>% ## filter to Louisville MSA (approximate)
              filter(AGEP >= 16) %>% ## filter to pop 16 and over
              select(PUMA, 
                     PWGTP, 
                     WAGP,
                     CIT)
}

#Filter data sets by PUMA's and age
indiana       <- pumaFilter(inPuma, inPums)
kentucky      <- pumaFilter(kyPuma, kyPums)

#Merge data
louisvilleMSA <- rbind(indiana, kentucky)

#Median Wage by Naturliaztion
#Filter 
louisvilleNaturalizedCitizen <- louisvilleMSA %>%
       filter(CIT == 4 | CIT == 5 | CIT == 1) %>%
       filter(WAGP >0)


#Cacluate weighted median wage by citizenship 
louisvilleMedianByCitizenship <- ddply(louisvilleNaturalizedCitizen, .(CIT), summarise, wMedian=w.median(WAGP, PWGTP))

#Add labels for legibility 
louisvilleMedianByCitizenship <- louisvilleMedianByCitizenship %>%
       mutate(Status = ifelse(CIT == 4, "Naturalized", 
                              ifelse(CIT == 5, "Not Citizen", 
                                     ifelse(CIT == 1, "Native", "Other"))))

#Select necessary variables
louisvilleMedianByCitizenship <- louisvilleMedianByCitizenship %>%
       select(Status, wMedian)

#Write to csv document
write.csv(louisvilleMedianByCitizenship, file = "louisvilleMedianByCitizenship.csv")
