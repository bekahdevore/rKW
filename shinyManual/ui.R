
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cyborg"),
  # Application title
  mainPanel(
    tabsetPanel(
      tabPanel(
        'Home',
        h1("Manual"),
        h3("Welcome!"),
        h5("This is a guide."),
        p("A process manual to help anyone replicate reports, data, and visualizations published the KentuckianaWorks Labor Market Intelligence Department."), 
        h5("An attempt will be made to make this as precise and painless as possible for all involved."),
        h3("Each tab contains processes for different projects"), 
        h2("May the odds be ever in your favor.")
      ),
      
      tabPanel(
        'Quarterly Report',
        h1("Quarterly Report")
      ),
      
      tabPanel(
        'Monthly Newsletter', 
        h1("Monthly Newsletter")
      ), 
      
      tabPanel(
        'Adding Codes to Career Pathways', 
        h1('Career Pathways Data Update'), 
        h6('In order for the career pathways to work catagories need to be added. To do this you will need two things:'),
        p('Access to google drive called "Career Pathways" with excel documents that have original pathway layouts', a('here', href = 'https://drive.google.com/drive/folders/0B3xkhRLVTMYdUXRldU9IS2t5Qms?usp=sharing')), 
        p('Access to the googleSheet that has the list of occupations for the career pathways', a('here', href = 'https://docs.google.com/a/kentuckianaworks.org/spreadsheets/d/1rL0sCtUSzBbhlZYSGvUgx3fXip55o2OpMWUMK_6TKaA/edit?usp=sharing')),
        h6('The main objective it to add the category and SOC code for each Occupation (Description
        ) on the list.'), 
        h6('To do this open a career pathway from the google drive (e.g. Business), and add the category (column) that each occupation belongs too'), 
        h6('To find the SOC code you might have to look up each occupation in EMSI and then record the SOC code associated with it.'), 
        h6('You can find the names to call each cateogry on the second tab of the career pathways googleSheet')
      ),
      
      tabPanel(
        'Shiny App Links', 
        h1('Click on link below to open Shiny App'),
        h5(a('Credentials Analysis', href = 'https://kwlmi.shinyapps.io/shinyCredentialsByEducation/')),
        p('In Progress:'),
        p('Adding wage ranges for occupations in sankey chart'),
        p('Adding download .PNG button for easy transfer to communications dept.'),
        br(),
        h5(a('Business Postings', href = 'https://kwlmi.shinyapps.io/shinyBusiness/')),
        br(), 
        h5(a('Career Pathways',   href = 'https://kwlmi.shinyapps.io/shinyCareerPathways/')), 
        p('In progress: Developing backend for data input')
      ),
      
      tabPanel(
        'About Datasets', 
        h1('The datasets...')
      )


  ))))

