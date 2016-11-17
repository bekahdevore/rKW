library(dplyr)
library(plotly)
library(reshape2)
library(ggthemes)


originalData             <- read.csv('highDemandData.csv')
highDemandVisualData     <- originalData %>% top_n(-25, indexRank ) %>% select(3:6)
indexRank                <- originalData %>% select(3, 8)

colnames(highDemandVisualData)[2] <- 'Job Postings'
colnames(highDemandVisualData)[3] <- 'Job Growth'
colnames(highDemandVisualData)[4] <- 'Possible Retirements'

#highDemandVisualData     <- highDemandVisualData %>% arrange(indexRank) 

melted <- melt(highDemandVisualData, 'Occupation')

melted$cat <- ''
melted[melted$variable == 'Job Postings',]$cat <- '.'
melted[melted$variable != 'Job Postings',]$cat <- '-'

melted <- left_join(melted, indexRank, by = "Occupation")

## Add variable factor to sort by
melted<- melted %>%
  mutate(test = factor(Occupation, 
                     ordered=TRUE, 
                     levels=unique(melted[order(melted$indexRank),"Occupation"])))

p <- ggplot(melted, aes(x = cat, y = value, fill = variable)) + 
      geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ test)
p + coord_flip() + facet_grid(test ~ ., switch = 'y') + 
    theme_minimal() +
  theme(strip.text.y = element_text(angle = 180, hjust = 1, size = 12),
        strip.background = element_blank(), 
        axis.text.y      = element_blank(), 
        axis.ticks.y     = element_blank(), 
        legend.title     = element_blank(), 
        legend.text      = element_text(size = 16), 
        legend.position  = c(.7, .7), 
        legend.background = element_blank(),
        axis.title       = element_blank()) +
        scale_y_continuous(expand = c(0,0))
        

