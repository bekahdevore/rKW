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
                                          mutate(percentOfWorkforce = freq.y/(freq.y+freq.x))
              
              foreignBornBySOCP    <- merge(foreignBornBySOCP, socNames, by="SOCP")
              
              foreignBornBySOCP    <- foreignBornBySOCP %>%
                                          select(SOCP, title, freq.x, freq.y, percentOfWorkforce)
              
              colnames(foreignBornBySOCP)[2] <- "Occupation"
              colnames(foreignBornBySOCP)[3] <- "Native"
              colnames(foreignBornBySOCP)[4] <- "Foreign Born"
              colnames(foreignBornBySOCP)[5] <- "Percent Foreign Born"
              
              write.csv(foreignBornBySOCP, file = "foreignBornBySOCP.csv")

#count(louisvilleMSAOccupations, SOCP, wt=PWGTP)
       #Seperate by nativity status
       #calculate percent of workers by occupation or industry that are foreign born


#Filter by industry (NAICSP)
louisvilleMSAIndustries <- louisvilleMSA %>%
                                   filter(NAICSP != "NA")
              
              louisvilleMSAIndustries$SOCP      <- as.factor(louisvilleMSAIndustries$SOCP)
              louisvilleMSAIndustries$NATIVITY  <- as.factor(louisvilleMSAIndustries$NATIVITY)
              
              nativityWeightIndustry       <- count(louisvilleMSAIndustries, vars = c("NATIVITY", "NAICSP"), wt_var = "PWGTP")
              
              foreignBorn            <- nativityWeightIndustry %>%
                                                 filter(NATIVITY == 2)
              
              native                 <- nativityWeightIndustry %>% 
                                                 filter(NATIVITY ==1)
              
              foreignBornByNAICSP    <- merge(native, foreignBorn, by="NAICSP")
              
              foreignBornByNAICSP    <- foreignBornByNAICSP %>%
                                                 mutate(percentOfWorkforce = freq.y/(freq.y + freq.x))
              
              foreignBornByNAICSP    <- merge(foreignBornByNAICSP, naicsNames, by="NAICSP")
              
              foreignBornByNAICSP    <- foreignBornByNAICSP %>%
                                                 select(NAICSP, Industry, freq.x, freq.y, percentOfWorkforce)
              colnames(foreignBornByNAICSP)[2] <- "Occupation"
              colnames(foreignBornByNAICSP)[3] <- "Native"
              colnames(foreignBornByNAICSP)[4] <- "Foreign Born"
              colnames(foreignBornByNAICSP)[5] <- "Percent Foreign Born"
              
              write.csv(foreignBornByNAICSP, file = "foreignBornByNAICSP.csv")


       #Seperate by nativity status
       #calculate percent of workers by occupation or industry that are foreign born

#Median Wage by Naturliaztion
louisvilleNaturalizedCitizen <- louisvilleMSA %>%
                                   filter(CIT == 4 | CIT == 5 | CIT == 1) %>%
                                   filter(WAGP >0)

              louisvilleMedianByCitizenship <- ddply(louisvilleNaturalizedCitizen, .(CIT), summarise, wMedian=w.median(WAGP, PWGTP))
              louisvilleMedianByCitizenship <- louisvilleMedianByCitizenship %>%
                                                 mutate(Status = ifelse(CIT == 4, "Naturalized", 
                                                                        ifelse(CIT == 5, "Not Citizen", 
                                                                               ifelse(CIT == 1, "Native", "Other"))))
              louisvilleMedianByCitizenship <- louisvilleMedianByCitizenship %>%
                                                 select(Status, wMedian)
              write.csv(louisvilleMedianByCitizenship, file = "louisvilleMedianByCitizenship.csv")
