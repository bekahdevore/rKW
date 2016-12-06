library(shiny)
library(RCurl)
library(DT)
library(dplyr)


healthData        <- read.csv("healthData.csv", check.names = FALSE)
manufacturingData <- read.csv("manufacturingData.csv", check.names = FALSE)

healthData        <- healthData %>% select(2:6)
manufacturingData <- manufacturingData %>% select(2:6)


shinyServer(function(input, output) {
        
        output$healthData <- renderDataTable({
                                datatable(healthData, 
                                          options = list(dom = "t"), 
                                          rownames = FALSE)
                             })
        
        output$manuData <- renderDataTable({
                                datatable(manufacturingData, 
                                        options = list(dom = "t"), 
                                        rownames = FALSE)
        })
        
        
        })
            