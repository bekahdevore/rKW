
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Louisville Tech/Developer Jobs"),

  # Sidebar with a slider input for number of bins

    fluidRow(
      DT::dataTableOutput("table")
      )
    )
  )

