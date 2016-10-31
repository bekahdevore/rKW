
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(fluidPage(

  includeCSS("main.css"),
  
  # Application title
  titlePanel("Careeer Pathways Louisville MSA"),
  mainPanel(
    tabsetPanel(
      tabPanel(
          'Manufacturing'#,
          #htmlOutput('manufacturing')
          ),
      
      tabPanel(
          'Healthcare'#,
          # htmlOutput('healthcare')
          ),
      
      tabPanel(
        'Logistics'#,
        #htmlOutput('logistics')
        ),
      
      tabPanel(
        'IT',
        htmlOutput('it')),
      
      tabPanel(
        'Food & Beverage'#,
        #htmlOutput('foodAndBeverage')
        ),
      
      tabPanel(
        'Business'#,
        # htmlOutput('business')
        )
  ))
  # Sidebar with a slider input for number of bins
))


