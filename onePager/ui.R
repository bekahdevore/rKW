
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyUI(fluidPage(

  # Application title
  titlePanel("Labor Market Data"),

    # Show a plot of the generated distribution
    mainPanel(
      h2("Louisville MSA and Peer Cities"),
      tabsetPanel(
        tabPanel("Louisville Rankings", DT::dataTableOutput("louisvilleRankings")),
        tabPanel("All Peer City Data", DT::dataTableOutput("allData"))
      ),
      br(), 
      br(), 
      h2("MIT Living Wage, Louisville MSA"),
      tabsetPanel(
        tabPanel("Family of Four", DT::dataTableOutput("mitLivingWageTableFamilyOfFour")), 
        tabPanel("All Family Sizes", DT::dataTableOutput("mitLivingWageTable"))
      ),
      br(), 
      br(), 
      br(), 
      br()
    )
  )
)
