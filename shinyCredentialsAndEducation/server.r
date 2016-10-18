
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(googleVis)
library(dplyr)

credentialsData <- as.data.frame(read.csv("credentialsAndEducation.csv"))
credentialsData$source  <-  as.character(credentialsData$source)
credentialsData$target <- as.character(credentialsData$target)
credentialsData$weight <- as.numeric(credentialsData$weight)
credentialsData[,1] <- as.character(credentialsData[,1])

shinyServer(function(input, output) {
       
       credentialsDataReactive <- reactive({
              data <- credentialsData %>%
                     filter(source == input$credential)%>%
                     select(-X) %>%
                     as.data.frame()
       })

       
       output$credentials <- renderGvis({
              gvisSankey(credentialsDataReactive(), 
                         from = "source", 
                         to = "targe", 
                         weight = "weight", 
                         options=list(height =900,
                                      width = 1200,
                                      sankey="{node:{label:{fontSize:14}}}"
                                      
                         ))})
       
       output$credentialTable <- DT::renderDataTable(DT::datatable({
              data <- credentialsData %>%
                     filter(source == input$credential) %>%
                     select(-X)
       }))
})
