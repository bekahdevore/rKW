
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

careers <- as.data.frame(read.csv("CareerChangersMatrix.csv",
                    header=TRUE))
occupation <- sort(as.character(unique(careers$Title)))

library(shiny)
library(networkD3)

shinyUI(fluidPage(
  
  tags$head(
    tags$script(src = 'http://d3js.org/d3.v3.min.js')
  ),

  # Application title
  titlePanel("Career Matrix"),

 
  fluidRow(
    column(4,
    checkboxGroupInput("occupation", label = h3("Checkbox group"), 
                       choices = occupation,
                       selected = "Accountants")),

    
    column(8,
           mainPanel(
             simpleNetworkOutput("Network"), width = 15
    ))
  )
))


