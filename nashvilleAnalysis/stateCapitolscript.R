#load packages
library(dplyr)

#Load data
nashvilleData <- read.csv("nashvilleAnalysisData.csv")



#Dummy variable
nashvilleData <- nashvilleData %>%
                     mutate(stateCapitol = ifelse(State.Capitol. == "No", 0,
                                                  ifelse(State.Capitol. == "Yes", 1, 2)))


nashvilleData$A_MEDIAN <- as.character(nashvilleData$A_MEDIAN)
nashvilleData$A_MEDIAN <- as.numeric(gsub(",","",nashvilleData$A_MEDIAN ))
nashvilleData$A_MEDIAN <- as.numeric(nashvilleData$A_MEDIAN)

abridgedMedian <-nashvilleData %>% 
       filter(A_MEDIAN < 46000)




anovaTests <- function(dataInput){
                     noncapitols <- dataInput %>%
                            filter(State.Capitol. == "No")
                     
                     capitols <- dataInput %>%
                            filter(State.Capitol. == "Yes")

                     sample1 <- noncapitols[sample(1:nrow(noncapitols), 36,
                                                   replace=FALSE),]
                     sample2 <- noncapitols[sample(1:nrow(noncapitols), 36,
                                                   replace=FALSE),]
                     sample3 <- noncapitols[sample(1:nrow(noncapitols), 36,
                                                   replace=FALSE),]
                     
                     test1 <- rbind(capitols, sample1)
                     test2 <- rbind(capitols, sample2)
                     test3 <- rbind(capitols, sample3)
                     
                     
                     plot(A_MEDIAN ~ State.Capitol., data=test1)
                     plot(A_MEDIAN ~ State.Capitol., data=test2)
                     plot(A_MEDIAN ~ State.Capitol., data=test3)
}

anovaTests(abridgedMedian)
#ANOVA
nashvilleAnova <- aov(nashvilleData$A_MEDIAN ~ nashvilleData$State.Capitol.)

nashvilleAnova
summary(nashvilleAnova)


#post-hoc
TukeyHSD(nashvilleAnova)


req <- lm(A_MEDIAN ~ stateCapitol, data = nashvilleData)
summary(req)
coefficients(req)
influence(req)
vcov(req)
abline(req)
coef(summary(req))["stateCapitol","Pr(>|t|)"]

