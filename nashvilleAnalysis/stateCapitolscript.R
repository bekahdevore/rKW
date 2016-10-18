#load packages
library(dplyr)
library(ggplot2)

#Load data
nashvilleData <- read.csv("stateCapitolData.csv")




#Dummy variable
nashvilleData <- nashvilleData %>%
                     mutate(stateCapitol = ifelse(State.Capital == "No", 0,
                                               ifelse(State.Capital == "Yes", 1, 2)))

#Change variable types
nashvilleData$Annual.Median.Wage <- as.character(nashvilleData$Annual.Median.Wage)
nashvilleData$Annual.Median.Wage <- as.numeric(gsub(",","",nashvilleData$Annual.Median.Wage))


#Visually cut off outliers
abridgedMedian <-nashvilleData %>% 
       filter(Annual.Median.Wage < 46000)


anovaTests <- function(dataInput){
                     noncapitols <- dataInput %>%
                            filter(State.Capital == "No")
                     
                     capitols <- dataInput %>%
                            filter(State.Capital == "Yes")
                     
                     #Create random samples of noncapitols to match the number of 
                     #states with capitols
                     sample1 <- noncapitols[sample(1:nrow(noncapitols), 36,
                                                   replace=FALSE),]
                     sample2 <- noncapitols[sample(1:nrow(noncapitols), 36,
                                                   replace=FALSE),]
                     sample3 <- noncapitols[sample(1:nrow(noncapitols), 36,
                                                   replace=FALSE),]
                     
                     #Combine capitols with each sample to create test cases
                     test1 <- rbind(capitols, sample1)
                     test2 <- rbind(capitols, sample2)
                     test3 <- rbind(capitols, sample3)
                     
                     
                     
                     #Plot each test case
                     plot(Annual.Median.Wage ~ State.Capital, data=test1)
                     plot(Annual.Median.Wage ~ State.Capital, data=test2)
                     plot(Annual.Median.Wage ~ State.Capital, data=test3)
                     
                     p <- ggplot(test1, aes(factor(State.Capital), Annual.Median.Wage))
                     p + geom_boxplot(aes(fill=factor(State.Capital)))
                     
                     r <- ggplot(test2, aes(factor(State.Capital), Annual.Median.Wage))
                     r + geom_boxplot(aes(fill=factor(State.Capital)))
                     
                     t <- ggplot(test3, aes(factor(State.Capital), Annual.Median.Wage))
                     t + geom_boxplot(aes(fill=factor(State.Capital))) +scale_x_discrete(labels=c("Not A State Capitol", "State Capitol"))
                     
}

#Change column names, more user friendly names



#Run with data
anovaTests(abridgedMedian)
anovaTests(nashvilleData)



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

####TESTING##############

noncapitols <- abridgedMedian %>%
       filter(State.Capital == "No")

capitols <- abridgedMedian %>%
       filter(State.Capital == "Yes")

sample1 <- noncapitols[sample(1:nrow(noncapitols), 36,
                              replace=FALSE),]
sample2 <- noncapitols[sample(1:nrow(noncapitols), 36,
                              replace=FALSE),]
sample3 <- noncapitols[sample(1:nrow(noncapitols), 36,
                              replace=FALSE),]

test1 <- rbind(capitols, sample1)
test2 <- rbind(capitols, sample2)
test3 <- rbind(capitols, sample3)


boxplot(Annual.Median.Wage ~ State.Capital, data=test1)
boxplot(Annual.Median.Wage ~ State.Capital, data=test2)
boxplot(Annual.Median.Wage ~ State.Capital, data=test3)

