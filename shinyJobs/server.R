
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(DT)
library(RCurl)


dataConnection <- getURL("https://docs.google.com/spreadsheets/d/1BxPUQVLcH_Pclo8lp79jlw8ajsCdO31sB2_g9xQsYJY/pub?gid=0&single=true&output=csv")
techJobs <- read.csv(textConnection(dataConnection))

techJobs <- techJobs%>%
  select(Title, Employer, City, JobDate, JobUrl)%>%
  arrange(desc(JobDate))

jobs <- techJobs
jobs$JobUrl <- paste0("<a href='",jobs$JobUrl,"'target='_blank'>",jobs$JobUrl,"</a>")

shinyServer(function(input, output) {

  output$table <- DT::renderDataTable(DT::datatable(
     jobs, escape = FALSE
  )) 
})
