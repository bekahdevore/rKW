library(shiny)
library(RCurl)
library(DT)
library(dplyr)


healthData        <- read.csv("healthData.csv",        check.names = FALSE)
manufacturingData <- read.csv("manufacturingData.csv", check.names = FALSE)
govData           <- read.csv("govData.csv",           check.names = FALSE)
animalData        <- read.csv("animalData.csv",        check.names = FALSE)
businessData      <- read.csv("businessData.csv",      check.names = FALSE)

healthData        <- healthData        %>% select(2:6)
manufacturingData <- manufacturingData %>% select(2:6)
govData           <- govData           %>% select(2:6)
animalData        <- animalData        %>% select(2:6)
businessData      <- businessData      %>% select(2:6)


shinyServer(function(input, output) {
        
        output$healthData <- renderDataTable({
                                datatable(healthData, 
                                          options  = list(dom = "t"), 
                                          rownames = FALSE)
        })
        
        output$manuData <- renderDataTable({
                                datatable(manufacturingData, 
                                          options  = list(dom = "t"), 
                                          rownames = FALSE)
        })
        
        output$govData  <- renderDataTable({
                                datatable(govData, 
                                          options  = list(dom = "t"), 
                                          rownames = FALSE)
        })
        
        output$animalData <- renderDataTable({
                                datatable(animalData, 
                                          options  = list(dom = "t"), 
                                          rownames = FALSE)
        })
        
        output$businessData <- renderDataTable({
                              datatable(businessData, 
                                        options  = list(dom = "t"), 
                                        rownames = FALSE)
        })
        
        
})
            