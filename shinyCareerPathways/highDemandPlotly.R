library(dplyr)
library(plotly)
library(reshape2)


highDemandVisualData     <- read.csv('highDemandData.csv')
highDemandVisualData     <- highDemandVisualData %>% top_n(-25, indexRank ) %>% select(3:6)

#highDemandVisualData     <- highDemandVisualData %>% arrange(indexRank) 

melted <- melt(highDemandVisualData, 'Occupation')

melted$cat <- ''
melted[melted$variable == 'Number.of.Job.Postings',]$cat <- '.'
melted[melted$variable != 'Number.of.Job.Postings',]$cat <- '..'

p <- ggplot(melted, aes(x = cat, y = value, fill = variable)) + 
      geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ Occupation)
p + coord_flip() + facet_grid(Occupation ~ ., switch = 'y') +
  theme(strip.text.y = element_text(angle = 180, hjust = .99, size = 12),
        strip.background = element_blank(), 
        axis.text.y      = element_blank(), 
        axis.ticks.y     = element_blank())

