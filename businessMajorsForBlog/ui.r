
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(googleVis)

workforce     <- read.csv("pumsKY.csv")
socNames      <- read.csv("socCodeTitleCrosswalk.csv")
appData       <- merge(workforce, socNames, by= "SOCP")
dataList      <- as.character(unique(appData$title))


shinyUI(fluidPage(
       
       # Application title
       titlePanel(""),
       
       # Sidebar with a slider input for number of bins
       fluidRow(
              fluidRow(
                     column(4, 
                            (htmlOutput("sankey"))) 
                     
       ))))