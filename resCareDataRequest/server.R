library(dplyr)
library(shiny)
library(DT)

topTen <- read.csv("topProjected.csv",             check.names = FALSE)
loss   <- read.csv("historicAndProjectedLoss.csv", check.names = FALSE)

shinyServer(function(input, output) {

  output$distPlot <- DT::renderDataTable(
              topTen,  
              options  = list(dom = "t"), 
              rownames = FALSE)
  
  output$loss  <- DT::renderDataTable(
            loss,  
            options  = list(dom = "t"), 
            rownames = FALSE)
  

})
