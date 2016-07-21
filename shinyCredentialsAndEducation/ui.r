
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(googleVis)

credentialsData <- as.data.frame(read.csv("credentialsAndEducation.csv"))
occupationsList <- as.character(unique(credentialsData$source))


shinyUI(fluidPage(
       
       # Application title
       titlePanel("Louisville MSA Credentials"),
       
       # Sidebar with a slider input for number of bins
       fluidRow(
              column(12, 
                     selectInput("credential",
                                 "Select a credential",
                                 choices = occupationsList)),
              fluidRow(
                     column(4, 
                            (htmlOutput("credentials"))), 
                     
                     column(4, offset = 4,
                            h4("Occupations"),
                            DT::dataTableOutput("credentialTable"))
                     
              )
              )))
