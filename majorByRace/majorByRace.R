library(RCurl)
library(dplyr)
library(ggplot2)
library(scales)
library(stats)
library(plotrix)

## Get data (PUMS 2015, 4 STATE REGION - KY, IN, OH, TN) & FOD1P major names
indiana   <- read.csv("ss15pin.csv")
kentucky  <- read.csv("ss15pky.csv")
ohio      <- read.csv("ss15poh.csv")
tennessee <- read.csv("ss15ptn.csv")

majorConnection <- getURL("https://docs.google.com/spreadsheets/d/1uE6r5SxXnw1z14bouQqi0_1cMd9xshvCgf9RGWpWVAA/pub?gid=0&single=true&output=csv")
majorNames      <- read.csv(textConnection(majorConnection))

## Clean/filter Data 
filterData <- function(originalData) {
                originalData <- originalData %>% 
                  select(PWGTP, RAC1P, FOD1P, SCHL) %>%
                  filter(RAC1P <= 2 & SCHL >=21)
                # originalData <- na.omit(originalData) 
} 

indiana   <- filterData(indiana)
kentucky  <- filterData(kentucky)
ohio      <- filterData(ohio)
tennessee <- filterData(tennessee)

allData <- rbind(indiana, kentucky, ohio, tennessee)

allData <- left_join(allData, majorNames, by = "FOD1P")

allData <- allData %>% mutate(Race = ifelse(RAC1P == 1, "White", 'Black'))

black <- allData %>% filter(Race == "Black")
white <- allData %>% filter(Race == "White")

totalWhite = sum(white$PWGTP)
totalBlack = sum(black$PWGTP)
black <- black %>% mutate(percent = PWGTP/totalBlack)
white <- white %>% mutate(percent = PWGTP/totalWhite)

blackWhite <- rbind(black, white)

# cbPalette <- c(
#   '#A4D7F4',
#   '#F8971D',
#   '#9C0059',
#   '#A4D7F4',
#   '#8DC63F',
#   '#D31245',
#   '#00853F',
#   '#767662'
# )

# blackWhite$major_order = factor(blackWhite$majorShort, levels=c('Social Science','Business & Finance','Education','Physical Science', 
#                                                            'Health & Medical', 'Engineering', 'Computer Science & Mathematics', 
#                                                            'Arts', 'Agriculture', 'Trade'))

# 
# p <- ggplot(blackWhite, aes(x = Race, y = percent, fill = Race)) + 
#   geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ major_order)
# 
# 
#   majorByRace <- p + 
#   coord_flip()                       + 
#   facet_grid(major_order ~ ., switch = 'y') + 
#   #theme_minimal()                    +
#   theme(strip.text.y = element_text(angle = 180, 
#                                     hjust = 1, 
#                                     size = 9, 
#                                     face = 'bold'),
#         strip.background  = element_rect(fill   = 'white'),
#         # color  = 'grey'),
#         panel.background  = element_rect(fill   = 'white'),
#         # color  = 'grey'),
#         axis.text.y       = element_blank(), 
#         axis.ticks.y      = element_blank(), 
#         axis.text.x       = element_text(size = 9), 
#         legend.title      = element_blank(), 
#         legend.text       = element_text(size = 14),
#         # face = 'bold'), 
#         legend.position   = c(.78, .7), 
#         legend.background = element_blank(),
#         legend.key        = element_rect(color = 'white', 
#                                          size = 3),
#         legend.key.size   = unit(1, 'lines'),
#         axis.title        = element_blank()) +
#   scale_y_continuous(expand = c(0,0), labels = percent) +
#   scale_fill_discrete(breaks = c("White", "Black"))
#   # scale_fill_manual(values = cbPalette)
# majorByRace


############## RACE BY SOCIAL SCIENCE MAJORS

# socialScience <- blackWhite %>% filter(majorShort == "Social Science")
# business      <- blackWhite %>% filter(majorShort == "Business & Finance")

bachelors <- blackWhite %>% filter(SCHL == 21)
masters   <- blackWhite %>% filter(SCHL == 22)


# test <- aggregate(socialScience$PWGTP, by = list(Category = socialScience$major), FUN = sum)
# test <- test[order(-test$x),]

detailedMajorChart <- function(enterData, enterTitle) {

  majorDifference <- enterData %>% ungroup() %>% group_by(Race, major) %>% summarize(t = sum(percent))
  test <- majorDifference %>% ungroup() %>% group_by(major) %>% summarize(testing = t[1] - t[2])
  # socialScience$major_order = factor(socialScience$major, levels= unique(socialScience$major[order(socialScience$PWGTP)]))
  
  test[["sign"]] = ifelse(test[["testing"]] >= 0, "positive", "negative")
  test <- na.omit(test)
  p <- ggplot(test, aes(x = reorder(major, testing), y = testing, fill = factor(sign, levels = c("positive", "negative"),
                                                                                labels = c("Higher proportion of black/African American\nstudents choose this major\n", 
                                                                                                 "Higher proportion of white/non-Hispanic\nstudents choose this major\n")))) + 
    geom_bar(stat = 'identity') + ggtitle(enterTitle)
  # + facet_grid(~ major_order)  position = 'stack'

  
  
  p + coord_flip() + scale_y_continuous(expand = c(0,0), labels = percent) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position   = c(.65, .6),
          panel.background = element_blank()) +
          scale_fill_manual(values = c("#4ca6a6", "#ff4d4d"))
}


detailedMajorChart(blackWhite, "All Majors")
detailedMajorChart(bachelors, "Bachelor's Degree by Major")
detailedMajorChart(masters, "Master's Degree by Major")


bachelorsBlack <- black %>% filter(SCHL == 21)
bachelorsWhite <- white %>% filter(SCHL == 21)

mastersBlack <- black %>% filter(SCHL == 22)
mastersWhite <- white %>% filter(SCHL == 22)

bachelorsBlackMean <- weighted.mean(bachelorsBlack$SEMP, bachelorsBlack$PWGTP)
bachelorsWhiteMean <- weighted.mean(bachelorsWhite$SEMP, bachelorsWhite$PWGTP)

graduateBlackMean <- weighted.mean(mastersBlack$SEMP, mastersBlack$PWGTP)
graduateWhiteMean <- weighted.mean(mastersWhite$SEMP, mastersWhite$PWGTP)

bachelorsBlackMean
bachelorsWhiteMean
graduateBlackMean
graduateWhiteMean

bachelorsBlackSEMP <- bachelorsBlack %>% filter(WAGP != 0 & INTP != 0) 
bachelorsFilter <- bachelors %>% filter(WAGP != 0 & INTP != 0) 
p6 <- ggplot(bachelorsFilter, aes(x = WAGP, y = COW, fill = Race, size = PWGTP )) + geom_point(shape = 21) + scale_fill_discrete("plum1", "purple4")
p6

bachelorsWhiteSEMP <- bachelorsWhite %>% filter(WAGP != 0) 
p6 <- ggplot(bachelorsWhiteSEMP, aes(x = INTP, y = PWGTP, size = INTP)) + geom_point() + scale_x_continuous(labels = comma)
p6

df.expanded <- bachelors[rep(row.names(bachelors), bachelors$PWGTP)]


