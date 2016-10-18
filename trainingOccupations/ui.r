

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(dplyr)
library(stringr)


# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Louisville MSA Occupations"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4,
             sliderInput("earnings",
                         "Entry level earnings:",
                          min = 9,
                          max = 20, 
                          value = 12.50,
                          step = 0.1)
      ),
      column(4,
             sliderInput("growth",
                         "Minimum Number of Jobs Added:",
                         min = 10,
                         max = 500, 
                         value = 100)
      ),
      column(3, 
             downloadLink('downloadData', 'Download')
             )),
   
      
      
    
    # Create a new row for the table.
    fluidRow(
      DT::dataTableOutput("table")
    )
  )
)