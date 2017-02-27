library(RCurl)
library(dplyr)
library(ggplot2)
library(scales)

dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1uMjWjAh1aMzEFJRKRSeG1eZkNBrPTZtX5GRyENgsl9I/pub?output=csv")
laborConnection <- getURL("https://docs.google.com/spreadsheets/d/1uMjWjAh1aMzEFJRKRSeG1eZkNBrPTZtX5GRyENgsl9I/pub?gid=300333013&single=true&output=csv")

originalData   <- read.csv(textConnection(dataConnection))
laborData      <-  read.csv(textConnection(laborConnection))

rm(dataConnection, laborConnection)
 
## FUNCTIONS
scaleFUN <- function(x) sprintf("%.1f", x)
scaleFUNzero <- function(x) sprintf("%.1f", x)




unemployment <- originalData %>% select(Place, Unemployment, Group)

unemployment$Place <- factor(unemployment$Place, levels = unemployment$Place[order(-unemployment$Unemployment)])

p <- ggplot(unemployment, aes(x = Place, y = Unemployment, fill = Group)) + geom_bar(stat = "identity") +
  ylab("Unemployment Rate") + ggtitle("Unemployment Rate, Dec. 2016") + guides(fill = FALSE)


medianWage <- originalData %>% select(Place, Annual.Median, Group)

medianWage$Place <- factor(medianWage$Place, levels = medianWage$Place[order(medianWage$Annual.Median)])
medianWage$label <- dollar(medianWage$Annual.Median)
medianWage$label <- gsub("\\..*","", medianWage$label)

q <- ggplot(medianWage, aes(x = Place, y = Annual.Median, fill = Group, label = label)) + geom_bar(stat = "identity", position = "dodge") +
  ylab("Median Wage") + ggtitle("Median Wage 2016") + geom_text(position = position_dodge(0.7), hjust = 1.2) + guides(fill = FALSE)


jobGrowth <- originalData %>%  select(Place, Job.Growth.2005...2016, Group_negative) %>% filter(Job.Growth.2005...2016 < 8879347)
jobGrowth$Place <- factor(jobGrowth$Place, levels = jobGrowth$Place[order(jobGrowth$Job.Growth.2005...2016)])

r <- ggplot(jobGrowth, aes(x = Place, y = Job.Growth.2005...2016, fill = Group_negative)) + geom_bar(stat = "identity") + guides(fill = FALSE) + ylab("Job Growth") + ggtitle("Job Growth 2005 - 2016")


laborData$Place <- factor(laborData$Place, levels = laborData$Place[order(laborData$participation)])
s <- ggplot(laborData, aes(x = Place, y = participation, fill = Group)) + 
      geom_bar(stat = "identity") + guides(fill = FALSE) + 
      ylab("Labor Force Participation Rate") + 
      ggtitle("Labor Force Participation Rate, age 25 -54, 2015")

chartHere(s, percent)


cbPalette <- c(
  '#A4D7F4',
   
  # '#9C0059',
  '#8DC63F',  #Louisville
  '#767662', #Peer
 #'#00853F'
  '#F8971D' #USA
  # '#D31245'
)

chartHere(p, scaleFUN)

chartHere(r, comma)

chartHere(q, dollar_format())

chartHere <- function(insertPlot, scales) {
   chartt <- insertPlot + 
  coord_flip()                       + 
  #theme_minimal()                    +
  theme(title = element_text(face = "bold"),
        # color  = 'grey'),
        panel.background  = element_rect(fill   = 'white'),
        # color  = 'grey'),
        axis.ticks.y      = element_blank(), 
        axis.title.y      = element_blank(),
        axis.text.x       = element_text(size = 9),
        legend.title      = element_blank(), 
        legend.text       = element_text(size = 14),
        # face = 'bold'), 
        legend.position   = c(.9, .5), 
        legend.background = element_blank(),
        legend.key        = element_rect(color = 'white', 
                                         size = 3),
        legend.key.size   = unit(1, 'lines')
        #axis.title        = element_blank()
        ) +
  scale_y_continuous(expand = c(0,0),  labels = scales) +
  scale_fill_manual(values = cbPalette)

  chartt
}







