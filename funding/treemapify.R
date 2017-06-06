library(ggplot2)
library(treemapify)
library(RColorBrewer)

# plot1 <- ggplot(noTotalData2003, aes(area = AmountMillions, fill = Source, label = label)) +
#   geom_treemap() +
#   geom_treemap_text(
#     fontface = "italic",
#     colour = "white",
#     place = "centre",
#     grow = FALSE
#   ) 

colourCount = length(unique(noTotalData$Source))
getPalette = colorRampPalette(brewer.pal(9, "Accent"))
  
  ggplot(noTotalData, aes(area = AmountMillions, fill = Source, label = label)) +
  geom_treemap() +
  facet_wrap(~ Year, ncol = 1) +
  #scale_fill_brewer("Dark2") +
  scale_fill_manual(values = getPalette(colourCount), breaks=rev(levels(noTotalData$Source))) +
  #scale_fill_brewer(palette = "RdGy") +
  theme(legend.position = "none") +
  labs(
    title = "KentuckianaWorks Funding",
    caption = "",
    fill = "Source"
  ) +
    geom_treemap_text(
      #fontface = "italic",
      colour = "black",
      place = "centre",
      grow = F
    ) + 
    theme(panel.spacing = unit(1, "lines"))

