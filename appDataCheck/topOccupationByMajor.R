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

regionalData <- merge(regionalAreaData, socNames, by="SOCP")
regionalData <- regionalAreaData
regionalData$PWGTP <- as.numeric(as.character(regionalData$PWGTP))

schoolFilter <- function(majorFOD1P){major <- (regionalData %>%
                                                        filter(FOD1P == majorFOD1P)
                                                        )
y <- count(major, SOCP, wt=PWGTP, sort = TRUE)
y        ## Select variables of interest 
}

history    <- schoolFilter(6402)
psychology <- schoolFilter(5200)
english    <- schoolFilter(3301)

history
psychology
english







schoolFilterGreaterThan <- function(schoolCode){history <- (kyInPUMS %>%
                                                                   filter(FOD1P == 6402) %>% 
                                                                   filter(SCHL >= schoolCode)%>%
                                                                   select(PUMA, SOCP, SCHL, PWGTP, FOD1P))
y <- count(history, SOCP, wt=PWGTP, sort=TRUE)
y         ## Select variables of interest 
}

associatesAndAbove <- schoolFilterGreaterThan(20)
bachelorsAndAbove <- schoolFilterGreaterThan(21)

associates <- schoolFilter(20)
bachelors <- schoolFilter(21)
masters <- schoolFilter(22)
professional <- schoolFilter(23)
doctorate <- schoolFilter(24)