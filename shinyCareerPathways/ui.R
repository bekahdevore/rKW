
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("sandstone"),
  includeCSS("main.css"),
  
  # Application title
  titlePanel("Career Pathways Louisville MSA"),
  mainPanel(
    tabsetPanel(
      
      tabPanel(
        'IT',
        htmlOutput('it')),
      
      tabPanel(
        'Business',
        htmlOutput('business')
      ),
      
      tabPanel(
        'Healthcare',
        htmlOutput('healthcare')
      ),
      
      tabPanel(
          'Manufacturing',
          htmlOutput('manufacturing')
          ),
      
      tabPanel(
        'Logistics',
          htmlOutput('logistics')
        ),
      
      tabPanel(
        'Food & Beverage',
        htmlOutput('food')
        )
  ))
  # Sidebar with a slider input for number of bins
))


