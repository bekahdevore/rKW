library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)

credentialByEducationLevel   <- read.csv("credentialByEducation.csv")

credentialByEducationLevel$n <- 1

credentialByEducationLevel   <- credentialByEducationLevel %>%
                                    filter(Degree != 'na')

credentialByEducationLevel <- credentialByEducationLevel %>% 
                                    group_by(Degree, Certification) %>%
                                    tally  %>%
                                    group_by(Degree) 

credentialByEducationLevel$commaNumber <- format(credentialByEducationLevel$nn, big.mark = ',')

credentialByEducationLevel$label <- paste(credentialByEducationLevel$Certification, 
                                          credentialByEducationLevel$commaNumber,  
                                          sep = '\n') 


shinyServer(function(input, output) {
  
  credentials <- reactive({
                      credentialByEducationLevel <- credentialByEducationLevel %>%
                      filter(Degree == input$select) %>%
                      top_n(30, wt = nn)
                      
  })
  
  output$value <- renderPlot({
    
    treemap(credentials(),  index = 'label', vSize = 'nn',
            vColor = 'Certification', 
            title  = '')
  })
})