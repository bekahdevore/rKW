
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("yeti"),
  # Application title
  titlePanel("Economic News, Greater Louisville Area"),
  p("Data source:", a("EMSI Analyst", href = "http://www.economicmodeling.com/analyst/")),

  # Sidebar with a slider input for number of bins

    # Show a plot of the generated distribution
    mainPanel(
      h1("Top Ten Projected Job Growth by Industry"),
     DT::dataTableOutput("distPlot"), 
     h1("Top Loss, Historic and Projected"),
     DT::dataTableOutput("loss")

))
)