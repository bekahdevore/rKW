
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
        p("An attempt will be made to make this as precise and painless as possible for all involved."),
        p("Each tab contains processes for different projects"), 
        h5("May the odds be ever in your favor :)")
      ),
      
      
      tabPanel(
        'Career Calculator',
        h1("Career Calculator"), 
        h5('Data update schedule can be found here:', 
           a('Career Calculator Update Schedule',
              href = 'https://docs.google.com/document/d/1dXribqXG8DJbaM_NsCI20mfLfI73PB-c7805_O0dv_g/pub')), 
        h3('Updating the App'), 
        p('You need access to the login credentials for the AWS console found here:', 
           a('AWS credentials', 
              href = 'https://docs.google.com/a/kentuckianaworks.org/spreadsheets/d/1Rv5j9HcVbbSdPOGJUeu4cSYID6AL6d2sDDBtbKmesuY/edit?usp=sharing')), 
        p('Once logged in navigate to "Services", then "S3"'), 
        p('This is where you will find a data "bucket", with downloadable and uploadable data connected to the app'), 
        p('To update a specific data point, download the datasheet, edit, save, then upload back into the same S3 bucket'), 
        p('To upload an entire data set, just upload the data set into the bucket, the app is programmed to
          automatically select and place the new data')
        
      ),
      
      tabPanel(
        'Data Requests',
        h1("Data Requests"), 
        h5('Protocol:'), 
        h6('Step One:'), 
        p('Respond to inform the requester that you are aware of their request'), 
        h6('Step Two:'), 
        p('Determine whether or not you have answered a similar question, how you organize your data and visualizations is key here. 
          If you have already answered a similar question that is current and will fufill the request, go no further and respond to
          the requester using the data and write a kind and succint email with some information about the data/visualization. If not, 
          proceed to Step Three.'), 
        h6('Step Three:'), 
        p('Ask questions! Make sure you understand what they want, it is better to clarify the question and parameters than have to repeat the data pull/visualization from scratch'), 
        p('Give time estimate, and let requestor know if it changes for any reason'),
        p('Fufill request and let requestor know to follow up with any questions or data moving forward :)')
      ),
      
      tabPanel(
        'Cradle to Career',
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
        'Blog',
        h1("Blog"), 
        h5('The LMI blog is housed at', a('KentuckiAnalytics.org', href = 'http://kentuckianalytics.org/')), 
        p('The blog is a wordpress.org based site self-hosted on AWS.'),
        h6('Goal: publish a new blog post once a month, and answer data questions from the public'),
        p('Questions from blog are sent to LMI@kentuckianaworks.org')
      ),
      
      tabPanel(
        'Monthly Newsletter', 
        h1("Monthly Newsletter"), 
        p('Every month we (in partnership with the KW communications department) 
           release a newsletter with BLS updates for the Louisville MSA area'),
        h6('Schedule of updates can be found here:',
           a('BLS MSA Release Schedule', href = 'http://www.bls.gov/schedule/news_release/metro.htm')), 
        h5('To update the data and visualizations follow these steps:'), 
        p('Find the shinyBlsUpdates file and open shinyBlsUpdates.Rproj to open project'),
        p('In the file ui.r update lines 13 - 19 (use BurningGlass to pull these numbers (saved 
          reports, "monthlyUpdateJobPostings", "associatesPlus", and "bachelorPlus" will give
          you the numbers you need, but make sure to change the time period to the current reference month'),
        p('Run and republish app'), 
        p("Send communication's department new numbers"),
        p('Sweet, you did it :)')
      ),
      
      tabPanel(
        'Quarterly Report', 
        h1("Updating the Quarterly Report"), 
        h4("You will need:"), 
        tags$ul(
          tags$li("Burning Glass & EMSI login credentials"),
          tags$li("access to quarterlyReportMainInput google sheet"), 
          tags$li("quarterlyReport.rProj (Contains necessary rScripts)"),
          tags$li("Quarterly Report InDesign File, and InDesign login credentials")),
        h4("Outline:"), 
        tags$ul(
          tags$li("Pull New Data (Burnning Glass and EMSI)"),
          tags$li("Update google sheet with new data (quarterlyReportMainInput google sheet)"), 
          tags$li("Run R Scripts"),
          tags$li("Drag and drop R script output visualizations into InDesign quarterly report template"))
    ),        
      
      
      tabPanel(
        'Shiny Apps', 
        h1('Shiny Apps'),
        h4('Click on a link below to open Shiny App'),
        h5(a('Race, Education, and Occupations', href = 'https://kwlmi.shinyapps.io/raceByEducationExploration/')),
        br(),
        h5(a('Credentials Analysis', href = 'https://kwlmi.shinyapps.io/shinyCredentialsByEducation/')),
        br(),
        h5(a('Career Pathways',        href = 'https://kwlmi.shinyapps.io/careerpathways2/')), 
        br(),
        h5(a('BLS Newsletter Update',   href = 'https://kwlmi.shinyapps.io/shinyBlsUpdates/')),
        br(),
        h5(a('Business Postings',       href = 'https://kwlmi.shinyapps.io/shinyBusiness/')),
        br(), 
        h5(a('JCTC Data Request',       href = 'https://kwlmi.shinyapps.io/shinyJCTC/')),
        br(),
        h5(a('ResCare Data Request',    href = 'https://kwlmi.shinyapps.io/resCareDataRequest/')),
        br(),
        h5(a('Business Majors',         href = 'https://kwlmi.shinyapps.io/louisvilleBusinessMajors/')),
        br(),
        h5(a('Tech Jobs Data Table',    href = 'https://kwlmi.shinyapps.io/shinyJobs/')),
        br(),
        h5(a('Current Jobs and Training',    href = 'https://kwlmi.shinyapps.io/currentJobsAndTraining/')),
        br(),
        h5(a('Louisville by Industry',  href = ' https://kwlmi.shinyapps.io/louByIndustry/')),
        br(),
        h5(a('Puerto Rico by Industry', href = ' https://kwlmi.shinyapps.io/puertoRicoByIndustry/')),
        br(),
        h5(a('Training Occupations',    href = ' https://kwlmi.shinyapps.io/trainingOccupations/')),
        br()
      ),
      
      tabPanel(
        'Other', 
        h5('Find the', a('MIT Living Wage here', 
                         href = 'http://livingwage.mit.edu/metros/31140'), 
           'we use the measure for a family of two with two children w/o childcare.')
      ),

    tabPanel("Regional Plan Data",
      tabsetPanel(
        tabPanel('QCEW', 
                     h5(a('QCEW Data Link', href = 'https://www.bls.gov/regions/southeast/news-release/countyemploymentandwages_kentucky.htm')), 
                     h5("Helpful Information:"), 
                     tags$ul(
                       tags$li("Multiply wage by 52 to annualize data"),
                       tags$li(""), 
                       tags$li(""),
                       tags$li(""))), 
      tabPanel("ACS", 
      tags$ul(
        tags$li("Pull New Data (Burnning Glass and EMSI)"),
        tags$li("Update google sheet with new data (quarterlyReportMainInput google sheet)"), 
        tags$li("Run R Scripts"),
        tags$li("Drag and drop R script output visualizations into InDesign quarterly report template"))),
      tabPanel("BLS LAUS", 
               h5(a("BLS LAUS Data", href = "https://www.bls.gov/web/metro/laucntycur14.txt")), 
               h5(a("Zip file", href = "https://www.bls.gov/lau/#data")), 
               p("Download file under 'County Data/Table'")),
      tabPanel("Burning Glass", 
               h5(a("Burning Glass Data", href = "http://laborinsight.burning-glass.com/jobs/us#"))), 
      tabPanel("Counties")
    )),   
      
      tabPanel(
        'Datasets', 
        h1('The datasets...'), 
        h5('American Community Survey'),
        p(a('American Fact Finder', 
            href = "https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t"), 
            'makes census data easier to find. We use the S2301 summary sheet for several labor force data. How to use 
          American Fact Finder:',
            tags$ol(
              tags$li("1. Choose a dataset (usually 1 year estimates)"), 
              tags$li("2. Choose a geography (usually Louisville MSA)"), 
              tags$li("3. Choose topic, or search summary sheet key (ex. S2301, DP03)"), 
              tags$li("4. From here you can open and download the data summary of interest")
            )),
        h5('Public Use Microdatasets (PUMS)'),
        p("For the PUMS data we use 1 year estimates"),
        h5('Bureau of Labor Statistics'),
        p('Unemployment rates, median wages'),
        h5('EMSI Analyst'),
        p("EMSI aggregates public datasets and make them easier to access and filter by geo location, they also model
          projections for jobs for up to ten years into the future"),
        h5('Burning Glass'), 
        p('We use BG to look at local online job postings'), 
        h5('KCEWS'), 
        p('We use KCEWS for longitutinal data about college graduates by major'),
        br(),
        downloadLink('datasetInfo', 'Download Helpful Document about datasets')
      )


  ))))

