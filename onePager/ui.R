
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
        tabPanel("Louisville Rankings", 
                 DT::dataTableOutput("louisvilleRankings")),
        tabPanel("All Rankings", DT::dataTableOutput("allRankings")),
        tabPanel("All Peer City Data",
                 h1("Peer City Data"),
                 p("Alphabetical order"),
                 DT::dataTableOutput("allData"))
      ),
      br(), 
      br(), 
      h2('State and US Data'),
      DT::dataTableOutput("stateUsData"),
      br(), 
      br(), 
      h2("Louisville, growth since 2011"), 
      DT::dataTableOutput("growthLouisville"), 
      br(), 
      br(), 
      h2("MIT Living Wage, Louisville MSA"),
      tabsetPanel(
        tabPanel("Family of Four", DT::dataTableOutput("mitLivingWageTableFamilyOfFour")), 
        tabPanel("All Family Sizes/Expenses", DT::dataTableOutput("mitLivingWageTable"))
      ),
      br(), 
      br(), 
      br(), 
      br(), 
      br(), 
      br(),
      p("Source: Median Wage: BLS OES; Labor Force Size and Unemployment Rate: BLS LAUS; Establisments and Employment: QCEW;
        Home Value, Household Income, Monthly Rent, and Population: ACS"),
      br()
    )
  )
)
