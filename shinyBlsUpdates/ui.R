
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(blsAPI)
library(RCurl)
library(RJSONIO)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(shinythemes)

month             <- "September"
dataRelease       <- "11/2/16"
totalJobPostings  <- "11,471"
baPlusJobPostings <- "2,385"
percentageBaPlus  <- "20.8%"

totalPostings <- 11471
baPostings <- 2385
percentBachelors <- round((baPostings/totalPostings), digits = 4) 

lastMonthsPostings <- 9239
baLastMonth <- 2059
postingsChange <- (totalPostings - lastMonthsPostings)
baChange <- (baPostings - baLastMonth)



shinyUI(fluidPage(
       theme = shinytheme("journal"),

    # Show a plot of the generated distribution
   fluidRow(
    column(12, 
       align = "center",
       h1("Unemployment Rate,", month, "2006-2016"),
       h6('Data Source: Bureau of Labor Statistics, Local Area Unemployment Statistics. Data Released: ', dataRelease),
       plotlyOutput("unemploymentRatePlot"),
       br(),
       br()),
    
    column(12, 
       br(), 
       br(),
       align = "center",
       h1("Size of Labor Force,", month,  "2006-2016"),
       h6('Data Source: Bureau of Labor Statistics, Local Area Unemployment Statistics. Data Released: ', dataRelease),
       plotlyOutput("laborForcePlot")
    ),
    column(12,
       align = "center",
       br(), 
       br(),
       br(), 
       br(),
       h1("Online Job Postings, ", month), 
       h3("Total: ", totalJobPostings, " Bachelors or higher: ", baPlusJobPostings)
           ), 
       align = "center",
       h4(percentageBaPlus, "of online job postings are adverstising for a bachelor's degree or higher in the Louisville MSA"), 
       h6('Data Source: Burning Glass Labor Insights')
  )))



