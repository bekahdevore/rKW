
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(DT)

techJobs <- read.csv("techJobs.csv")

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
