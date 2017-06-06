library(ggplot2)
library(gcookbook)
library(plyr)
library(RCurl)
library(dplyr)
library(scales)
library(RColorBrewer)
library(treemap)
library(lattice)
library(gridBase)
library(gridExtra)
library(grid)

dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1vbMB49xcHjPzApRaU8EWZJ6O1pDrPJikboTQkmLlo5w/pub?gid=0&single=true&output=csv")
spendingConnection <- getURL("https://docs.google.com/spreadsheets/d/1vbMB49xcHjPzApRaU8EWZJ6O1pDrPJikboTQkmLlo5w/pub?gid=537181117&single=true&output=csv")
colorConnection <- getURL("https://docs.google.com/spreadsheets/d/1vbMB49xcHjPzApRaU8EWZJ6O1pDrPJikboTQkmLlo5w/pub?gid=1743863858&single=true&output=csv")

spending <- read.csv(textConnection(spendingConnection))
allData <- read.csv(textConnection(dataConnection))
colorData <- read.csv(textConnection(colorConnection))

noTotalData <- allData %>% filter(Source != "Total")



# create the ggplot2 data, using Year and thousands (of people) and filled by age group.
# ggplot(noTotalData, aes(x=Year, y=AmountMillions, fill=Source)) +
#   # add a area plot layer
#   geom_area() +
#   # change the colors and reserve the legend order (to match to stacking order)
#   scale_fill_brewer(palette="Blues", breaks=rev(levels(noTotalData$Source)))

# colorData <-as.data.frame(unique(noTotalData$Source))
# colorData$Color <- rainbow(nlevels(colorData$`unique(noTotalData$Source)`))

# colourCount = length(unique(noTotalData$Source))
# getPalette = colorRampPalette(brewer.pal(9, "Spectral"))
# 
# g <- ggplot(noTotalData, aes(as.factor(Year), AmountMillions))
# g + geom_bar(aes(fill = Source), stat = "identity") + scale_y_continuous(name = "Funding Amount", labels = dollar_format(suffix = " M") ) +
#   scale_fill_manual(values = getPalette(colourCount), breaks=rev(levels(noTotalData$Source))) + xlab("Year") +
#   theme(legend.position = "bottom") 

noTotalData <- left_join(noTotalData, colorData, by = "Source")

noTotalData$label <- paste0(noTotalData$Source, "\n", "$",noTotalData$AmountMillions, "M")

noTotalData2011 <- noTotalData %>% filter(Year == 2011)
noTotalData2007 <- noTotalData %>% filter(Year == 2007)
noTotalData2003 <- noTotalData %>% filter(Year == 2003)
noTotalData2016 <- noTotalData %>% filter(Year == 2016)
noTotalData2019 <- noTotalData %>% filter(Year == 2019)

## Add amount in millions to labels and total to titles

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
treemap(noTotalData2003, "label",  "AmountMillions", title = "", type = "color", vColor = "Color", 
       vp = vplayout(1,1))
treemap(noTotalData2007, "label",  "AmountMillions", title = "", type = "color", vColor = "Color",
        vp = vplayout(2,1))
treemap(noTotalData2011, "label",  "AmountMillions", title = "", type = "color", vColor = "Color",
        vp = vplayout(3,1))


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
treemap(noTotalData2016, "label",  "AmountMillions", title = "", type = "color", vColor = "Color", 
        vp = vplayout(1,1))
treemap(noTotalData2019, "label",  "AmountMillions", title = "", type = "color", vColor = "Color",
        vp = vplayout(2,1))

spending$label <- paste0(spending$Expenditure, "\n", "$", spending$Amount, "M")
treemap(spending, "label", "Amount")

spending$Expenditure

