
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

navbarPage(
       theme = shinytheme("united"),
       title = 'JCTC Data Request',

       tabPanel('Occupations and Labor Market Statistics', 
                h1('Occupations, Counties in a 50 mile Radius of Louisville'), 
                DT::dataTableOutput("dataTable"),
                h6('EMSI Analyst, Burning Glass Labor Insights (Job Postings)'),
                br(),
                h1('Labor Force Statistics, Louisville MSA (12 County Region), Ages 16-64'), 
                h3('Population: 831,815'), 
                h3('Labor Force Participation Rate: 75.15%'), 
                h3('Unemployed: 50,691'), 
                h3('Unemployment Rate: 6.09%'), 
                h6('American Community Survey, 1 year estimates, 2015')
                ),
  
       tabPanel('Data Sources', 
                h1('Counties in 50 Mile Radius'), 
                h1('Counties in the Louisville MSA')

  ))

