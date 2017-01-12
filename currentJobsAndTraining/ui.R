library(shiny)
library(RCurl)
library(dplyr)

dataConnectionSectors <- getURL('https://docs.google.com/spreadsheets/d/1n0DVC8fBwTr6ne9Fw5dFaRtO3ka-Y2i8zweFGIzmVcQ/pub?gid=624724603&single=true&output=csv')
sectors      <- read.csv(textConnection(dataConnectionSectors), check.names = FALSE)

sectors <- sectors %>% select(3)



c <- as.data.frame("All")
colnames(c)[1] <- "sector"

sectors <- rbind(c, sectors)
choices <- sort(unique(sectors$sector))

rm(c, sectors)

library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Louisville MSA Jobs with Openings in the Last 90 days"),
    p("Data Source: Burning Glass Labor Insights, Online Job Postings"), 
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4,
             selectizeInput("sector",
                             "Choose/type a sector:",
                              choices = choices)
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