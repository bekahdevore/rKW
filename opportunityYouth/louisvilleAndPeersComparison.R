library(scales)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

load("louisvilleAllDataAllVariables.RData")
load("peerCitiesAllDataAllVariables.RData")
load("kentuckyAllDataAllVariables.RData")

test <- cleanData(peerCitiesAllData)

cleanData <- function(dataHere) {
#CLEAN
  ## Select variables needed
  allData <- dataHere %>% select(PWGTP, RAC1P, AGEP, DIS, SCHL, PERNP, ESR, NAICSP, OCCP, FOD1P)
  ## RACE
  ## AGE
  ## DISABILITY
  ## EDU ATTAINMENT
  ## MEDIAN INCOME
  ## INCOME DISTRIBUTION
  ## % UNEMPLOYED
  ## % IN LABOR FORCE
  ## % NOT IN LABOR FORCE
  ## INDUSTRY
  ## OCCUPATION
  ## FIELD OF DEGREE (EXTRA IF TIME)
}
# VISUALIZE
  ## BARS
  ## FACET GRID
  ## BOX-PLOT