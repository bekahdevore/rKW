
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(DT)

shinyUI(fluidPage(
  # theme = shinytheme("sandstone"),
  includeCSS("main.css"),
  
  # Application title
  titlePanel("Career Pathways Bluegrass Area"),
  mainPanel(
    
    tabsetPanel(
      
      tabPanel(
        'IT',
        h2("Information Technology", align = "center"),
        htmlOutput('it')),
      
      tabPanel(
        'Business',
        h2("Consumer and Business Services", align = "center"),
        htmlOutput('business')
      ),
      
      tabPanel(
        'Healthcare',
        h2("Health Enterprises and Lifelong Wellness & Aging", align = "center"),
        htmlOutput('healthcare')
      ),
      
      tabPanel(
          'Manufacturing',
          h2("Advanced Manufacturing", align = "center"),
          htmlOutput('manufacturing')
          ),
      
      tabPanel(
        'Logistics',
        h2("Logistics and Supply Chain Management", align = "center"),
          htmlOutput('logistics')
        ),
      
      tabPanel(
        'Food & Beverage',
        h2("Food and Beverage", align = "center"),
        htmlOutput('food')
        ), 
      
      tabPanel(
        'Construction',
        h2("Construction", align = "center"),
        htmlOutput('construction')
      ),
      
      tabPanel(
        'Main Data File', 
        br(),
        dataTableOutput('mainData')
      )
  ))
  # Sidebar with a slider input for number of bins
))


