library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)
library(googleVis)
library(plotly)

credentialByEducationLevel <- read.csv("credentialByEducation.csv")
credentialByEducationLevel <- credentialByEducationLevel %>% filter(Degree != 'na')
majors                     <- read.csv('majors.csv')
occupationNames            <- read.csv('sankey.csv')
occupationNames            <- occupationNames %>% filter(socGroup != 55)
occupationNames$label      <- as.character(occupationNames$label)

selectChoices              <- c("High School", 
                                "Associate's", 
                                "Bachelor's", 
                                "Master's", 
                                "PhD")

selectChoicesMajor         <- c("Associate's", 
                                "Bachelor's", 
                                "Master's", 
                                "PhD")


navbarPage(
  #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
  
  theme = shinytheme('cosmo'),
  title = 'Job Postings in the Louisville MSA',
  
  tabPanel('Occupation Group', 
           #tags$script(src="https://d3js.org/d3.v3.min.js"),
           #tags$script(src="sankey.js"),
           #tags$div(id="chart"),
           h5("Credentials by Major Occupation Group"),
           selectInput("occupationGroup", 
                       label = h4("Choose an occupation group"), 
                       choices = unique(occupationNames$Occupation)), 
           sliderInput("wageSlide", 
                       "Occupations with a median wage at or above:", 
                       min=0, 
                       max=216216, 
                       value=47273), 
           
            htmlOutput('view')
           #br(), 
           #br(), 
           #h2('Including postings not asking for a credential:'), 
           #htmlOutput('sankeyAll')
  ),
  
  tabPanel('Education Level', 
          fluidPage(
            fluidRow(
          column(2, 
          selectInput("select", 
                      label = h4("Choose an education level"), 
                      choices = selectChoices, 
                      0)),
          column(10, align ='center', 
                 h1("Top Certifications by Education Level"))),
          fluidRow(
          #plotOutput('value'), 
          column(12, plotOutput('educationBar'))),
          br(), 
          br(), 
          fluidRow(
          column(2,
                 selectInput('majorsDegree', 
                      label   = h4('Choose an education level'), 
                      choices = selectChoicesMajor)),
          column(10, align = 'center',
                 h1('Certifications by Education Level and Major'))),
          fluidRow(
          column(2, 
                 selectizeInput('majors', 
                      label   = h4('Choose or type a major'), 
                      choices = unique(majors$STDMajor))), 
          column(10, align ='center',
                 plotOutput('majors'))),
          br(), 
          br(), 
          br(), 
          br(), 
          br(), 
          br()
  )),
  
  tabPanel('About Data', 
           h1('Burning Glass, Labor Insights'),
           p('Online Job postings in the Louisville MSA, January 2015 - June 2016 ')
  ))
