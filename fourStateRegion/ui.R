library(shiny)
library(googleVis)


list <- c("Bachelors", "Masters")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Education and Race, Four State Region"),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    column(12,
           
           selectInput("dataSet",
                       "Select an education level for the sankey diagram:",
                       choices = list)
    ),
    
    # Show a plot of the generated distribution
    column(12, align = "center",
           htmlOutput("view") 
    )), 
    fluidRow(
    column(12, align = "center",
           plotOutput("allData", width = "100%")
    )
    
  )
))