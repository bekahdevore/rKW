library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)
library(googleVis)

credentialByEducationLevel <- read.csv("credentialByEducation.csv")
credentialByEducationLevel <- credentialByEducationLevel %>% filter(Degree != 'na')
occupationNames            <- read.csv('sankey.csv')
occupationNames            <- occupationNames %>% filter(socGroup != 55)

navbarPage(
  #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
  
  theme = shinytheme('cosmo'),
  title = 'Job Postings in the Louisville MSA',
  
  tabPanel('Visualization', 
           
           h5("Top 30 Certifications by Education Level"),
           selectInput("select", label = h4("Choose an education level"), 
                       choices = unique(credentialByEducationLevel$Degree)), 
           plotOutput('value'),
           #tags$script(src="https://d3js.org/d3.v3.min.js"),
           #tags$script(src="sankey.js"),
           #tags$div(id="chart"),
           h5("Top 30 Certifications by Education Level"),
           selectInput("occupationGroup", label = h4("Choose an occupation group"), 
                       choices = unique(occupationNames$Occupation)), 
           htmlOutput('view')
  ),
  
  tabPanel('About Data', 
           h1('Burning Glass, Labor Insights'),
           p('Online Job postings in the Louisville MSA, January 2015 - June 2016 ')
  ))
