
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
                downloadLink('downloadData', 'Download Data Table'), 
                br(),
                br(),
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
  
       tabPanel('About Data', 
                h1('EMSI Data'),
                     p('Emsi occupation employment data are based on final Emsi industry data and final 
                     Emsi staffing patterns. Wage estimates are based on Occupational Employment Statistics 
                     (QCEW and Non-QCEW Employees classes of worker) and the American Community Survey 
                     (Self-Employed and Extended Proprietors). Occupational wage estimates also affected by
                     county-level Emsi earnings by industry.'),
                     p('This report uses state data from the following agencies: Indiana Department of 
                     Workforce Development; Kentucky Office of Employment and Training'),
                     p('For more information about EMSI job projections, ', 
                       tags$a(href = "http://www.economicmodeling.com/2014/11/18/emsi-faq-where-do-emsi-projections-come-from/", 
                              'Click Here.')),
                h1('Burning Glass, Labor Insights'),
                     p('Online Job postings, October 2015 - September 2016'),
                h1('American Community Survey, ACS Data'),
                     p('2015 one year estimates'),
                h1('Counties in 50 Mile Radius'), 
                tags$ul(
                       tags$li(
                              'Indiana',
                              tags$ul(tags$li(
                                            'Clark,
                                            Crawford,
                                            Floyd,
                                            Harrison,
                                            Jackson,
                                            Jefferson,
                                            Jennings,
                                            Lawrence,
                                            Orange ,
                                            Perry,
                                            Ripley,
                                            Scott,
                                            Switzerland, &
                                            Washington Counties'))), 
                       tags$li(
                              'Kentucky',
                              tags$ul(tags$li(
                                            'Anderson,
                                            Breckinridge,
                                            Bullitt,
                                            Carroll,
                                            Franklin,
                                            Hardin,
                                            Henry,
                                            Jefferson,
                                            Larue,
                                            Marion,
                                            Meade,
                                            Mercer,
                                            Nelson,
                                            Oldham,
                                            Owen,
                                            Shelby,
                                            Spencer,
                                            Trimble,
                                            Washington, &
                                            Woodford Counties')))), 
                
                h1('Counties in the Louisville MSA'), 
                tags$ul(
                       tags$li(
                              'Indiana',
                              tags$ul(tags$li(
                                     'Clark, 
                                     Floyd,  
                                     Harrison,  
                                     Scott, & 
                                     Washington Counties
                                     '))), 
                       tags$li(
                              'Kentucky',
                              tags$ul(tags$li(
                                     'Bullitt,  
                                     Henry,  
                                     Jefferson,  
                                     Oldham,  
                                     Shelby,  
                                     Spencer, &
                                     Trimble Counties
                                     '))))
  ))

