
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cyborg"),
  # Application title
  mainPanel(
    tabsetPanel(
      tabPanel(
        'Home',
        h1("Manual"),
        h3("Welcome fellow traveler!"),
        h5("This is a guide."),
        p("A process manual to help anyone replicate reports, data, and visualizations published the KentuckianaWorks Labor Market Intelligence Department."), 
        h5("We will attempt to make this as precise and painless as possible for all involved."),
        h3("Each tab contains processes for different projects"), 
        h2("May the odds be ever in your favor.")
      ),
      
      tabPanel(
        'Quarterly Report'
      ),
      
      tabPanel(
        'Monthly Newsletter'
      ), 
      
      tabPanel(
        'About Datasets'
      )


  ))))

