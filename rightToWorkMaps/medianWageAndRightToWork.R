library(ggplot2); library(ggmap)

BOTH.R <- read.csv("../../rData/rightToWorkData/Rtw.mw4.29.16.csv",header = TRUE)

allstates <- (map_data("state"))


Total <- merge(allstates, BOTH.R, by="region")
Total$

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$rtw.mw),
                      colour="white") + scale_fill_brewer(palette="RdGy", 
                        direction = -1, name="Legend")

P1 <- p + theme_bw()+ labs(title= "Median Wage by State", x="", y="")+
        theme(plot.title=element_text(size = 24, face = "bold"), 
                                        legend.text=element_text(size=24),
                                        legend.title=element_blank())

P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


