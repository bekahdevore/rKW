library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)
library(googleVis)

credentialByEducationLevel <- read.csv("credentialByEducation.csv")
credentialByEducationLevel <- credentialByEducationLevel %>% filter(Degree != 'na')
occupationNames            <- read.csv('sankey.csv')
#occupationNames            <- occupationNames %>% filter(socGroup != 55)

navbarPage(
  #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
  
  theme = shinytheme('cosmo'),
  title = 'Job Postings in the Louisville MSA',
  
  tabPanel('Occupation Group', 
           #tags$script(src="https://d3js.org/d3.v3.min.js"),
           #tags$script(src="sankey.js"),
           #tags$div(id="chart"),
           h5("Credentials by Major Occupation Group"),
           selectInput("occupationGroup", label = h4("Choose an occupation group"), 
                       choices = unique(occupationNames$Occupation)), 
           sliderInput("wageSlide", "Occupations with a median wage at or above:", 
                       min=0, max=216216, value=47273), 
           htmlOutput('view')
           #br(), 
           #br(), 
           #h2('Including postings not asking for a credential:'), 
           #htmlOutput('sankeyAll')
  ),
  
  tabPanel('Education Level', 
  
          h5("Top 30 Certifications by Education Level"),
          selectInput("select", label = h4("Choose an education level"), 
                      choices = unique(credentialByEducationLevel$Degree)), 
          plotOutput('value')
  ),
  
  
  tabPanel('About Data', 
           h1('Burning Glass, Labor Insights'),
           p('Online Job postings in the Louisville MSA, January 2015 - June 2016 ')
  ))
