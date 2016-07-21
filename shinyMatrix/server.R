
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(networkD3)
library(igraph)
library(dplyr)


shinyServer(function(input, output) {
  
  #Load Data
  onet <- (read.csv("CareerChangersMatrix.csv", header=TRUE))

  
  ##CREATE LINKS WITH TITLE RELATED AND STRENGTH OF CONNECTIONS
  careers$Title <- as.character(careers$Title)
  careers$Related <- as.character(careers$Related)
  
  
  networkData <- reactive({
          inputOccupations <- as.list(input$occupation)
          r <- careers %>%
            filter( Title %in%  inputOccupations)%>%
            select(Title, Related)
            
  })
  
 
    

  output$Network <-renderSimpleNetwork({
    careersData <- networkData()
    simpleNetwork(careersData, fontSize = 9, textColour = "#000", zoom = TRUE)
    })
})




  
