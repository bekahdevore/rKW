library(shiny)
library(shinythemes)
library(dplyr)

library(treemap)

credentialByEducationLevel <- read.csv("credentialByEducation.csv")
credentialByEducationLevel <- credentialByEducationLevel %>%
  filter(Degree != 'na')

navbarPage(
  theme = shinytheme('cosmo'),
  title = 'Job Postings in the Louisville MSA',
  
  tabPanel('Visualization', 
           
           h5("Top 30 Certifications by Education Level"),
           selectInput("select", label = h4("Choose an education level"), 
                       choices = unique(credentialByEducationLevel$Degree)), 
           plotOutput('value')
  ),
  
  tabPanel('About Data', 
           h1('Burning Glass, Labor Insights'),
           p('Online Job postings in the Louisville MSA, January 2015 - June 2016 ')
  ))
