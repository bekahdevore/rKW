
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
        'Career Calculator',
        h1("Career Calculator"), 
        h5('Data update schedule can be found here:', a('Career Calculator Update Schedule', href = 'https://docs.google.com/document/d/1dXribqXG8DJbaM_NsCI20mfLfI73PB-c7805_O0dv_g/pub'))
      ),
      
      tabPanel(
        'Data Requests',
        h1("Data Requests"), 
        h5('Protocol to answering data requests:'), 
        h6('Step One:'), 
        p('Respond to inform the requester that you are aware of their request'), 
        h6('Step Two:'), 
        p('Determine whether or not you have answered a similar question, how you organize your data and visualizations is key here. 
          If you have already answered a similar question that is current and will fufill the request, go no further and respond to
          the requester using the data and write a kind and succint email with some information about the data/visualization. If not, 
          proceed to Step Three.'), 
        h6('Step Three:')
      ),
      
      tabPanel(
        'Cradle to Career Update',
        h1("Cradle to Career Update"), 
        h5('Reported once a year around Nov. - Dec.'), 
        h6('Measures to report:'), 
        tags$ol(tags$li('Median Income adjusted for inflation'), 
                tags$li('% of jobs paying a median wage above the family supporting wage'), 
                tags$li('% of jobs with a typical entry-level education of a bachelor\'s degree or higher'), 
                tags$li('Number of individuals active in labor force (16 +)'), 
                tags$li('Labor Force Participation Rate'), 
                tags$li('Youth Employment (16 -19)')), 
        h6('Data Sources and Processes'), 
        tags$ol(tags$li('Median annual wage (BLS OES) adjusted for COL (BEA RPP). 
                        Median divided by Louisville BEA RPP (move decimal over two places to the left).', 
                        br(), 'Ex. Median wage = 37,571, Louisville BEA RPP = 91.4. 
                        Median adjusted for COL = 37,571/.914 = 41,106'), 
                tags$li('Use EMSI, naviagate to Occupations then Occupations Table, select Louisville MSA, custom add the following columns: Median Wage, 
                        Current (the year) Jobs, and Typical Entry Level Education, make sure the occupations are at the 5-digit level'))
      ),
      
      tabPanel(
        'Kentuckianalytics',
        h1("KentuckiAnalytics"), 
        h5('The LMI blog is housed at', a('KentuckiAnalytics.org', href = 'http://kentuckianalytics.org/'), 
           'a wordpress.org based site.'),
        h5('Goal: publish a new blog post once a month, and answer data questions from the public'),
        p('Questions from blog are sent to LMI@kentuckianaworks.org')
      ),
      
      tabPanel(
        'Monthly Newsletter', 
        h1("Monthly Newsletter"), 
        h5('Every 30 days we (in partnership with the KW communications department) 
           release a newsletter with BLS updates for the Louisville MSA area'),
        h5('Schedule of updates can be found here:',
           a('BLS MSA Release Schedule', href = 'http://www.bls.gov/schedule/news_release/metro.htm')), 
        h5('To update the data and visualizations follow these steps:')
      ), 
      
      tabPanel(
        'Shiny App Links', 
        h1('Shiny Apps'),
        h4('Click on a link below to open Shiny App'),
        h5(a('Credentials Analysis', href = 'https://kwlmi.shinyapps.io/shinyCredentialsByEducation/')),
        p('In Progress:'),
        p('Adding wage ranges for occupations in sankey chart'),
        p('Adding download .PNG button for easy transfer to communications dept.'),
        br(),
        h5(a('Career Pathways',        href = 'https://kwlmi.shinyapps.io/shinyCareerPathways/')), 
        p('In progress: Developing backend for data input'), 
        br(),
        h5(a('BLS Newsletter Update',   href = 'https://kwlmi.shinyapps.io/shinyBlsUpdates/')),
        br(),
        h5(a('Business Postings',       href = 'https://kwlmi.shinyapps.io/shinyBusiness/')),
        br(), 
        h5(a('JCTC Data Request',       href = 'https://kwlmi.shinyapps.io/shinyJCTC/')),
        br(),
        h5(a('Business Majors',         href = 'https://kwlmi.shinyapps.io/louisvilleBusinessMajors/')),
        br(),
        h5(a('Tech Jobs Data Table',    href = 'https://kwlmi.shinyapps.io/shinyJobs/')),
        br(),
        h5(a('Louisville by Industry',  href = ' https://kwlmi.shinyapps.io/louByIndustry/')),
        br(),
        h5(a('Puerto Rico by Industry', href = ' https://kwlmi.shinyapps.io/puertoRicoByIndustry/')),
        br(),
        h5(a('Training Occupations',    href = ' https://kwlmi.shinyapps.io/trainingOccupations/')),
        br()
      ),
      
      tabPanel(
        'About Datasets', 
        h1('The datasets...'), 
        h5('American Community Survey'),
        h5('Bureau of Labor Statistics'), 
        h5('EMSI Analyst'), 
        h5('Burning Glass'), 
        h5('KCEWS')
      )


  ))))

