library(dplyr)
library(scales)

dataFilterOY <- function(dataInput) {
  opportunityYouth <- dataInput %>% 
    filter(ESR == 3 | ESR == 6) %>% 
    filter(SCH == 1) %>%
    filter(AGEP >= 16 & AGEP <= 24)
}  

dataFilterAllYouth <- function(dataInput) {
  opportunityYouth <- dataInput %>% 
    filter(AGEP >= 16 & AGEP <= 24)
}  

raceCategories <- function(dataHere){
  dataHere %>% mutate(race = ifelse(RAC1P == 1, "White", 
                                    ifelse(RAC1P == 2, "Black", 
                                           ifelse(RAC1P == 3, "American Indian", 
                                                  ifelse(RAC1P == 8, "Some other race alone", 
                                                         ifelse(RAC1P == 9, "Two or more races", "Other"))))))
}

louisvilleOY <- raceCategories(dataFilterOY(louisvilleMSA)) %>% select(PWGTP, race)
louisvilleYouth <- raceCategories(dataFilterAllYouth(louisvilleMSA)) %>% select(PWGTP, race)

oy <- count(louisvilleOY, race, wt = PWGTP) 
allYouth <- count(louisvilleYouth, race, wt = PWGTP)

colnames(oy)[2] <- "oy"
colnames(allYouth)[2] <- "all"

allData <- left_join(allYouth, oy, by = "race")

allData <- allData %>% mutate(percentOY = oy/all) %>% filter(race != "Other") %>% 
  mutate(label = percent(percentOY))

allData$race <- factor(allData$race, levels = unique(allData$race)[order(allData$percentOY, decreasing = FALSE)])

g <- ggplot(allData, aes(race, percentOY, fill = race, label = label))
# Number of cars in each class:
g + geom_bar(stat = "identity") + 
  coord_flip() + labs(title = "Louisville MSA", subtitle = "PUMS 2015 1yr", y = "Percent Opportunity Youth", x = "") +
  geom_text(position = position_stack(vjust = 0.5)) + guides(fill = FALSE) + scale_y_continuous(labels = percent)

