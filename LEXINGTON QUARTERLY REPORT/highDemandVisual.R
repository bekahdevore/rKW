library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(scales)
library(extrafont)


originalData             <- read.csv('highDemandData.csv')
highDemandVisualData     <- originalData %>% top_n(-25, indexRank ) %>% select(3:6)
indexRank                <- originalData %>% select(3, 8)

colnames(highDemandVisualData)[2] <- 'Job Postings'
colnames(highDemandVisualData)[3] <- 'Job Growth'
colnames(highDemandVisualData)[4] <- 'Possible Retirements'

#highDemandVisualData     <- highDemandVisualData %>% arrange(indexRank) 

melted     <- melt(highDemandVisualData, 'Occupation')

melted$cat <- ''
melted[melted$variable == 'Job Postings',]$cat <- '.'
melted[melted$variable != 'Job Postings',]$cat <- '-'

melted <- left_join(melted, indexRank, by = "Occupation")

## Add variable factor to sort by
melted<- melted %>%
  mutate(test = factor(Occupation, 
                       ordered=TRUE, 
                       levels=unique(melted[order(melted$indexRank),"Occupation"])))
cbPalette <- c(
  '#9C0059',
  '#A4D7F4',
  '#8DC63F',
  '#F8971D',
  '#D31245',
  '#A4D7F4',
  '#00853F',
  '#767662'
)

p <- ggplot(melted, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ test)

highDemand <- p                                  + 
  coord_flip()                       + 
  facet_grid(test ~ ., switch = 'y') + 
  #theme_minimal()                    +
  theme(strip.text.y = element_text(angle = 180, 
                                    hjust = 1, 
                                    size = 9, 
                                    face = 'bold'),
        strip.background  = element_rect(fill   = 'white'),
        # color  = 'grey'),
        panel.background  = element_rect(fill   = 'white'),
        # color  = 'grey'),
        axis.text.y       = element_blank(), 
        axis.ticks.y      = element_blank(), 
        axis.text.x       = element_text(size = 9), 
        legend.title      = element_blank(), 
        legend.text       = element_text(size = 14),
        # face = 'bold'), 
        legend.position   = c(.65, .7), 
        legend.background = element_blank(),
        legend.key        = element_rect(color = 'white', 
                                         size = 3),
        legend.key.size   = unit(1, 'lines'),
        axis.title        = element_blank()) +
  scale_y_continuous(expand = c(0,0), labels = comma) + 
  scale_fill_manual(values = cbPalette)

highDemand


# png(file="highDemand.png", width = 4000, height = 4000)
# highDemand
# dev.off()
