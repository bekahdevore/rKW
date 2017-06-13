library(shiny)
library(shinythemes)
library(streamgraph)
library(grid)
library(gridBase)
library(gridExtra)

shinyUI(fluidPage(
  br(),
  h1("KentuckianaWorks"),
  theme = shinytheme("united"),
  
  # Show a plot of the generated distribution
  fluidRow(
    br(),
    br(),
    column(12, 
           h3("Funding (in millions)"), 
           h5("2003 - 2019 (projected)"),
           align = "center",
           streamgraphOutput("fundingStream"),
           br(),
           br()),
    
    column(12, 
           br(), 
           br(),
           align = "center", 
           plotOutput("fundingTree2003")
           #plotlyOutput("laborForcePlot")
    ),
    column(12,
           align = "center"

    )
    
  )))
