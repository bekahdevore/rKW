library(googleVis)

library(plyr)
library(dplyr)
library(cwhmisc)
library(stringr)

ky         <- read.csv("ss14pky.csv") 
oh         <- read.csv("ss14poh.csv")
ind        <- read.csv("ss14pin.csv")
tn         <- read.csv("ss14ptn.csv")
socNames   <- read.csv("socBlsNames.csv")
majorNames <- read.csv("fod1pNames.csv")

socNames$SOCP <- str_replace_all(socNames$SOCP, "-", "")

regionalAreaData <- rbind(ky, oh, ind, tn)

regionalAreaData <- regionalAreaData %>%
       filter(FOD1P != "<NA>") %>%
       filter(SOCP  != "<NA>") %>%
       filter(WAGP  >   0)     %>%
       select(FOD1P, SOCP, WAGP, PWGTP, SCHL)

regionalData <- merge(regionalAreaData, majorNames, by="FOD1P")

regionalData <- regionalData %>%
       mutate(wageRange = ifelse(WAGP < 30000, "Less than $30k", 
                                 ifelse(WAGP >= 30000 & WAGP < 40000, "$30k - 39k", 
                                 ifelse(WAGP >= 40000 & WAGP < 50000, "$40k - 49k", 
                                 ifelse(WAGP >= 50000 & WAGP < 80000, "$50k - 79k", 
                                 ifelse(WAGP >= 80000 & WAGP < 100000, "$80k - 99k", 
                                 ifelse(WAGP >= 100000, "$100k or more", "Other" 
                                 ))))))) %>%
       select(majorShort, wageRange, PWGTP)

regionalData$majorShort <- as.character(regionalData$majorShort)


skillsSankey <- plot(gvisSankey(
       regionalData, 
       from="majorShort",
       to="wageRange",
       weight = "PWGTP",
       options=list(height = 800,
                    width = 1500, 
                    sankey = "{link:{
                    color:{fill:'black'}
                    }}"
               )
       ))
