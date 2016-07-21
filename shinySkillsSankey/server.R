
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(googleVis)
library(dplyr)

skills <- as.data.frame(read.csv("skills.csv"))


shinyServer(function(input, output) {
  
  sankeyData <- reactive({
    data <- skills %>%
          filter(Element.Name == input$skill)%>%
          filter(Data.Value >=4)%>%
          as.data.frame()
  }) 

  output$view <- renderGvis({

    gvisSankey(sankeyData(), 
               from="Element.Name",
               to="Title",
               weight = "Weight",
               options=list(height =1000,
                            width = "100%", 
                            sankey = "{link:{
                            color:{fill:'grey'}, 
                            
  }}"
               )
               )
    })
  })



