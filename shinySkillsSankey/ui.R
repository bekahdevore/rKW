
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(googleVis)

skills <- as.data.frame(read.csv("skills.csv"))
skillList <- as.character(unique(skills$Element.Name))

shinyUI(fluidPage(

  # Application title
  titlePanel("Occupations and Skills"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("skill",
                  "Select an skill:",
                  choices = skillList)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("view")
    )
  )
))
