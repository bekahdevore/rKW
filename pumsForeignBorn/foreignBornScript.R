#load packages
library(dplyr)
library(cwhmisc)
library(tidyr)

#load data
kyPums        <- read.csv("ss14pky.csv")
inPums        <- read.csv("ss14pin.csv")
socNames      <- read.csv("socCodeTitleCrosswalk.csv")

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
                            SEX, 
                            SOCP, 
                            OCCP, 
                            NAICSP,  
                            NATIVITY, 
                            CIT)
}

indiana       <- pumaFilter(inPuma, inPums)
kentucky      <- pumaFilter(kyPuma, kyPums)
louisvilleMSA <- rbind(indiana, kentucky)


louisvilleMSA$SOCP   <- as.numeric(as.character(louisvilleMSA$SOCP))
louisvilleMSA$NAICSP <- as.numeric(as.character(louisvilleMSA$NAICSP))

#Filter by occupation (SOCP) 
louisvilleMSAOccupations           <- louisvilleMSA %>%
                                          filter(SOCP != "NA")
#Change variable types 
louisvilleMSAOccupations$SOCP      <- as.factor(louisvilleMSAOccupations$SOCP)
louisvilleMSAOccupations$NATIVITY  <- as.factor(louisvilleMSAOccupations$NATIVITY)


nativityWeight       <- count(louisvilleMSAOccupations, vars = c("NATIVITY", "SOCP"), wt_var = "PWGTP")

foreignBorn          <- nativityWeight %>%
                            filter(NATIVITY == 2)

native               <- nativityWeight %>% 
                             filter(NATIVITY ==1)

foreignBornBySOCP    <- merge(native, foreignBorn, by="SOCP")

foreignBornBySOCP    <- foreignBornBySOCP %>%
                            mutate(percentOfWorkforce = freq.y/freq.x)

foreignBornBySOCP    <- merge(foreignBornBySOCP, socNames, by="SOCP")

#count(louisvilleMSAOccupations, SOCP, wt=PWGTP)
       #Seperate by nativity status
       #calculate percent of workers by occupation or industry that are foreign born


#Filter by industry (NAICSP)
louisvilleMSAIndustries <- louisvilleMSA %>%
                                   filter(NAICSP != "NA")
       #Seperate by nativity status
       #calculate percent of workers by occupation or industry that are foreign born

#Median Wage by Naturliaztion
louisvilleNaturalizedCitizen <- louisvilleMSA %>%
                                   filter(CIT == 4 | CIT == 5) %>%
                                   filter(WAGP >0)

louisvilleMedianByCitizenship <- ddply(louisvilleNaturalizedCitizen, .(CIT), summarise, wMedian=w.median(WAGP, PWGTP))


