#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Opportunity Youth"),
  p("Youth not working or enrolled in school in the past 3 months (from filling out survey), ages 16 - 24"), 
  p("Data source: PUMS 2015 1 yr."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose Dataset:", c("Peer Cities", "Kentucky", "Louisville MSA"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("race"),
       plotOutput("disability"),
       plotOutput("sex"), 
       plotOutput("workStatus"),
       plotOutput("age"),
       plotOutput("lastWorked"),
       plotOutput("foodStamps"),
       plotOutput("socialSecurity"),
       plotOutput("ssi"),
       plotOutput("children"),
       plotOutput("child12Months"),
       plotOutput("cognitive"),
       plotOutput("school"), 
       br(),
       br(),
       br(),
       br()
    )
  )
))
