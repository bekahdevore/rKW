library(dplyr)

ky <- read.csv("ss14pky.csv")
ind <- read.csv("ss14pin.csv")

kyPUMA <- (c(1701,1702,1703,1704,1705,1800))
inPUMA <- (c(3400,3300))

pumaFilter <- function(dataInput, stateCode, pumaObject){
       dataInput%>%
       filter(ST == stateCode)%>%
       filter(PUMA %in% pumaObject)
       }

kentucky <- pumaFilter(ky, 21, kyPUMA)
indiana <- pumaFilter(ind, 18, inPUMA)

kyInPUMS <- rbind(kentucky, indiana)


schoolFilter <- function(schoolCode){history <- (kyInPUMS %>%
                   filter(FOD1P == 6402) %>% 
                   filter(SCHL == schoolCode)%>%
                   select(PUMA, SOCP, SCHL, PWGTP, FOD1P))
              y <- count(history, SOCP, wt=PWGTP, sort=TRUE)
              y        ## Select variables of interest 
}

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


