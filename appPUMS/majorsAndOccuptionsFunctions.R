library(dplyr)

ky <- read.csv("PUMSdata/ss14pky.csv")
ind <- read.csv("PUMSdata/ss14pin.csv")
oh <- read.csv("PUMSdata/ss14poh.csv")
tn <- read.csv("PUMSdata/ss14ptn.csv")

regionalPUMS <- rbind(ky, ind, oh, tn)

#Load descriptions data and merge with regionalPUMS
socDescriptions <- read.csv("SOCdescriptions.csv")
regionalPUMSwithDescriptions <- merge(regionalPUMS, socDescriptions, by="SOCP")



schoolFilter <- function(schoolCode, majorCode){history <- (regionalPUMSwithDescriptions %>%
                                                        filter(FOD1P == majorCode) %>% 
                                                        filter(SCHL == schoolCode)%>%
                                                        select(PUMA, SOCP, Description, SCHL, PWGTP, FOD1P))
              y <- count(major, Description, wt=PWGTP, sort=TRUE)
              y        ## Select variables of interest 
}

schoolFilterGreaterThan <- function(schoolCode, majorCode){major <- (regionalPUMSwithDescriptions %>%
                                                                   filter(FOD1P == majorCode) %>% 
                                                                   filter(SCHL >= schoolCode)%>%
                                                                   select(PUMA, SOCP, Description,SCHL, PWGTP, FOD1P))
              y <- count(major, Description, wt=PWGTP, sort=TRUE)
              y         ## Select variables of interest 
}

historyBachelorsPlus <- schoolFilterGreaterThan(21, 6402)
historyAssociatesPlus <- schoolFilterGreaterThan(20, 6402)
geoBachelorsPlus <- schoolFilterGreaterThan(21, 5504)
geoAssociatesPlus <- schoolFilterGreaterThan(20, 5504)

philosphyBachelorsPlus <- schoolFilterGreaterThan(21, 4801)

humanitiesBachelorsPlus <- schoolFilterGreaterThan(21, 3402)
chemicalEngineeringBachelorsPlus <- schoolFilterGreaterThan(21, 2405)

associates <- schoolFilter(20)
bachelors <- schoolFilter(21)
masters <- schoolFilter(22)
professional <- schoolFilter(23)
doctorate <- schoolFilter(24)