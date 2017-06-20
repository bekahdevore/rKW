
library(vioplot)
moneyStuff <- allData %>% filter(ESR == 1 | ESR == 2)
ky <- moneyStuff %>% filter(place == "Kentucky")
lou <- moneyStuff %>% filter(place == "Louisville") %>% select(PWGTP, PERNP)
peerGroup <- moneyStuff %>% filter(place == "Peers")

# vioplot(expandedKy$name, expandedLou$name, expandedPeerGroup$name,
#         names = c("Kentucky", "Louisville", "Peers"), 
#         col = (c("#D9E87D", "#267DE8", "#FFFF66")))

expandedLou <- data.frame(name = rep(lou$PERNP, lou$PWGTP)) %>% mutate(place = "Louisville")
expandedPeerGroup <- data.frame(name = rep(peerGroup$PERNP, peerGroup$PWGTP)) %>% mutate(place = "Peers")
expandedKy <- data.frame(name = rep(ky$PERNP, ky$PWGTP)) %>% mutate(place = "Kentucky")

allDataMoney <- rbind(
  expandedLou, 
  expandedPeerGroup, 
  expandedKy
)

moneyPlot <- ggplot(allDataMoney, aes(x=place, y=name, fill = place)) +
  geom_violin(trim = FALSE)

moneyPlotFinal <- moneyPlot + 
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.title = element_blank(),
    axis.ticks = element_blank() 
  ) +    
  scale_fill_manual(values = colorsAge) +
  ggtitle("Income Distribution") + 
  geom_boxplot(width = 0.1, outlier.shape = NA)
  

moneyPlotFinal

                          