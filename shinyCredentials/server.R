
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(googleVis)
library(dplyr)

credentialsData <- as.data.frame(read.csv("allCredentials.csv"))
credentialsData$Credential <-  as.character(credentialsData$Credential)
credentialsData$Occupation <- as.character(credentialsData$Occupation)
credentialsData[,1] <- as.character(credentialsData[,1])

shinyServer(function(input, output) {
       
       credentialsDataReactive <- reactive({
              data <- credentialsData %>%
                     filter(Credential == input$credential)%>%
                     select(Credential, Occupation, NumberOccupations)%>%
                     as.data.frame()
       })
       
       occupationsData <- reactive({
              data <- credentialsData %>%
                     filter(Occupation == input$occupation)%>%
                     select(Credential, Occupation, NumberOccupations)%>%
                     as.data.frame()
       })

       output$credentials <- renderGvis({
              gvisSankey(credentialsDataReactive(), 
                      from = "Credential", 
                      to = "Occupation", 
                      weight = "NumberOccupations", 
                     options=list(height =900,
                           width = 1200,
                           sankey="{node:{label:{fontSize:14}}}"
              
       ))})
       
      
       
       output$occupations <- renderGvis({
              gvisSankey(occupationsData(), 
                         from = "Occupation", 
                         to = "Credential", 
                         weight = "NumberOccupations", 
                         options=list(height =900,
                                      width = 1200,
                                      sankey="{node:{label:{fontSize:14}}}"
                         ))})
       
       output$credentialTable <- DT::renderDataTable(DT::datatable({
              data <- credentialsData %>%
                     filter(Credential == input$credential)%>%
                     select(Occupation, PercentOccupations, NumberOccupations) 
       }))
       
       output$occupationTable <- DT::renderDataTable(DT::datatable({
              occupationsForTable <- credentialsData %>%
                     filter(Occupation == input$occupation)%>%
                     select(Credential) 
       }))
})
