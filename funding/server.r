library(shiny)
library(plyr)
library(RCurl)
library(dplyr)
library(scales)
library(RColorBrewer)
library(treemap)
library(lattice)
library(streamgraph)
library(stringr)
library(lazyeval)
library(gridExtra)
library(gridBase)
library(grid)

dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1vbMB49xcHjPzApRaU8EWZJ6O1pDrPJikboTQkmLlo5w/pub?gid=0&single=true&output=csv")
spendingConnection <- getURL("https://docs.google.com/spreadsheets/d/1vbMB49xcHjPzApRaU8EWZJ6O1pDrPJikboTQkmLlo5w/pub?gid=537181117&single=true&output=csv")
colorConnection <- getURL("https://docs.google.com/spreadsheets/d/1vbMB49xcHjPzApRaU8EWZJ6O1pDrPJikboTQkmLlo5w/pub?gid=1536679231&single=true&output=csv")

spending <- read.csv(textConnection(spendingConnection))
allData <- read.csv(textConnection(dataConnection))
colorData <- read.csv(textConnection(colorConnection))

noTotalData <- allData %>% filter(Source != "Total")

noTotalData$Source <-sub("WIA|WIOA", "WIA/WIOA", noTotalData$Source)

noTotalData <- left_join(noTotalData, colorData, by = "Source")

noTotalData$label <- paste0(noTotalData$Source, "\n", "$",noTotalData$AmountMillions, "M")

noTotalData2003 <- noTotalData %>% filter(Year == 2003)
noTotalData2007 <- noTotalData %>% filter(Year == 2007)
noTotalData2011 <- noTotalData %>% filter(Year == 2011)
noTotalData2016 <- noTotalData %>% filter(Year == 2016)
noTotalData2019 <- noTotalData %>% filter(Year == 2019)

test <- subset(noTotalData, !duplicated(Source))
test <- test %>% arrange(Source)

shinyServer(function(input, output) {
  

  # 
  # 
  output$fundingStream <- renderStreamgraph({
    streamgraph(noTotalData, key="Source", value="AmountMillions", date="Year") %>% 
      sg_legend(show=TRUE, label="Funding source: ") %>% 
      sg_fill_manual(test$Color) %>% 
      sg_axis_x(1, "Year")
  })
  # 
  # 
  output$fundingTree2003 <-  renderPlot({
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(3, 2)))
    treemap(noTotalData2003, "label",  "AmountMillions", title = "2003", type = "color", vColor = "Color", 
            vp = vplayout(1,1))
    treemap(noTotalData2007, "label",  "AmountMillions", title = "2007", type = "color", vColor = "Color",
            vp = vplayout(2,1))
    treemap(noTotalData2011, "label",  "AmountMillions", title = "2011", type = "color", vColor = "Color",
            vp = vplayout(3,1))
    treemap(noTotalData2016, "label",  "AmountMillions", title = "2016", type = "color", vColor = "Color", 
            vp = vplayout(1,2))
    treemap(noTotalData2019, "label",  "AmountMillions", title = "2019", type = "color", vColor = "Color",
            vp = vplayout(2,2))

  })
  
})