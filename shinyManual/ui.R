
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
  mainPanel( width = 12,
    tabsetPanel(
      tabPanel(
        'Home',
        h1("Manual"),
        h3("Welcome!"),
        h5(em("This is a guide.")),
        p("A process manual to help anyone replicate the reports, data, and visualizations published the KentuckianaWorks 
          Labor Market Intelligence Department."), 
        p("An attempt will be made to make this process as precise and painless as possible for all involved."),
        p("Each tab contains processes for different projects"), 
        h5("May the odds be ever in your favor :)"), 
        img(src='http://www.laughspark.info/thumbfiles/705X705/cute-cat-with-beanie-and-glasses-635731307117442594-13752.jpg', 
            width = 200, height = 200, align = "center")
      ),
      
      tabPanel(
        'Intro', 
        h5('Hi :) Maybe you are new here?'),
        img(src='http://www.marymarcusfiction.com/wp-content/uploads/2015/09/yay-54383329058.jpeg', 
            width = 200, height = 200, align = "center"), 
        br(),
        br(),
        h5('This guidebook assumes: '), 
        tags$ul(
          tags$li('You have R & RStudio downloaded on your machine and connected to the shinyApps.io account (for publishing)'), 
          tags$li('You know how to open projects in R and run scripts'), 
          tags$li('You have access to all of the R project files and credentials'), 
          tags$li('You are generally familar with pulling data from Burning Glass, ACS, and EMSI')
        )
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
        'Blog',
        h1("Blog"), 
        h5('The LMI blog is housed at', a('KentuckiAnalytics.org', href = 'http://kentuckianalytics.org/')), 
        p('The blog is a wordpress.org based site self-hosted on AWS.'),
        h6('Goal: publish a new blog post once a month, and answer data questions from the public'),
        p('Questions from blog are sent to LMI@kentuckianaworks.org'), 
        p('login credentials:')
      ),
      
      tabPanel(
        'Quarterly Report', 
        h1("Updating the Quarterly Report"), 
        h4("You will need:"), 
        tags$ul(
          tags$li("Burning Glass & EMSI login credentials"),
          tags$li("access to 'QUARTERLY REPORT MAIN INPUT' google sheet"), 
          tags$li("'QUARTERLY REPORT.Rproj' (Contains necessary rScripts)"),
          tags$li("Quarterly Report InDesign File, and InDesign login credentials")),
        h4("Outline:"), 
        tags$ul(
          tags$li("Pull New Data (Burnning Glass and EMSI)"),
          tags$li("Update google sheet with new data ('QUARTERLY REPORT MAIN INPUT' google sheet)"), 
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
      
      tabPanel("Regional Plan Project",
             h3("Project timeframe: April 24th - May 5th", align = "center"), 
             p("All data points needed for both the total 40 county region and the four regions within (KentuckianaWorks, Bluegrass, Northern Kentucky, Lincoln Trail)", 
               align = "center"),
      sidebarPanel(
        h5("Counties:"),
             h6("KentuckianaWorks:"),
               tags$ul(
                 tags$li("Bullitt"),
                 tags$li("Henry"), 
                 tags$li("Jefferson"),
                 tags$li("Oldham"), 
                 tags$li("Shelby"), 
                 tags$li("Spencer"), 
                 tags$li("Trimble")), 
            h6("Bluegrass"),
              tags$ul(
                tags$li("Anderson"),
                tags$li("Bourbon"), 
                tags$li("Boyle"),
                tags$li("Clark"), 
                tags$li("Estill"), 
                tags$li("Fayette"),
                tags$li("Franklin"),
                tags$li("Garrard"), 
                tags$li("Harrison"),
                tags$li("Jessamine"), 
                tags$li("Lincoln"), 
                tags$li("Madison"),
                tags$li("Mercer"),
                tags$li("Nicholas"), 
                tags$li("Powell"),
                tags$li("Scott"), 
                tags$li("Woodford")),
            h6("Northern Kentucky:"),
            tags$ul(
              tags$li("Boone"),
              tags$li("Campbell"), 
              tags$li("Carroll"),
              tags$li("Gallatin"), 
              tags$li("Grant"), 
              tags$li("Kenton"),
              tags$li("Owen"),
              tags$li("Pendleton")), 
            h6("Lincoln Trail:"),
            tags$ul(
              tags$li("Breckinridge"),
              tags$li("Grayson"), 
              tags$li("Hardin"),
              tags$li("Larue"), 
              tags$li("Marion"), 
              tags$li("Meade"),
              tags$li("Nelson"),
              tags$li("Washington")),
          
        br(),
        br(),
        br(),
        br(),
        
        
        fileInput('datafile', h6('Choose CSV file'),
                  accept=c('text/csv', 'text/comma-separated-values,text/plain')),
        
        textInput("text", label = h6("Enter file name:"), value = ""),
        actionButton("do", "Save"),
        tableOutput("filetable")),
    
      mainPanel(
        tabsetPanel(
          tabPanel('QCEW', 
                       h5(a('QCEW County Data', href = 'https://www.bls.gov/regions/southeast/news-release/countyemploymentandwages_kentucky.htm', target = "_blank"), align = "center"), 
                       h5(a('QCEW County Data by Industry', href = 'https://data.bls.gov/cew/apps/data_views/data_views.htm#tab=Tables', target = "_blank"), align = "center"), 
                       h5("Tips:"), 
                       p("multiply wage by 52 to annualize"),
                       h5("Datapoints:"), 
                      tags$ul(
                        tags$li("Total Number of Jobs"),
                        tags$li("Average annual wage"), 
                        tags$li("Numer of jobs and average wage in certain industries (to the extent that these match up with the NAICS industry catagories)",
                                tags$ul(
                                  tags$li("Manufacturing"), 
                                  tags$li("Construction"),
                                  tags$li("Healthcare"),
                                  tags$li("Logistics"),
                                  tags$li("Finance"),
                                  tags$li("Retail"),
                                  tags$li("Food Service"),
                                  tags$li("Hospitality"),
                                  tags$li("Agriculture"), 
                                  tags$li("Military")
                                )))), 
        tabPanel("ACS",
                 h5(a("ACS Data", href = "https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t", target = "_blank"), align = "center"),
                 p("Topics/Dataset/2015 ACS 5-year estimates"),
                 p("Geographies/County/Select Counties "),
                 p("Seach summary sheet code"),
                 h5("Formulas:"), 
                 p("unemployment rate = unemployed/In labor force"), 
                 p("labor force participation rate = In labor force/Total"),
                 h5("Areas:"),
                 tags$ul(
                   tags$li("40 County Region"), 
                   tags$li("Top 4"), 
                   tags$li("Bottom 4"), 
                   tags$li("4 Regions (KentuckianaWorks, Bluegrass, Northern Kentucky, Lincoln Trail")),
                 tags$li(h6("Specific groups (40 county and 4 Regions):"), 
                         tags$ul(
                           tags$li("For 16 - 19 year olds"), 
                           tags$li("For people with disabilities"), 
                           tags$li("For African Americans"), 
                           tags$li("For people with a HS education or less"), 
                           tags$li("For people in poverty")
                         )),
                 h5("Datapoints:"),
        tags$ul(
          tags$li(h6("Unemployment Rates (B23025)")),
          tags$li(h6("Labor Force Participation Rates (B23025)")), 
          tags$li(h6("Educational Attainment (S1501)"), 
                  tags$ul(
                    tags$li("Percent BA +"), 
                    tags$li("Percent Associates +"), 
                    tags$li("Percent High School + ")
                  )))),
        tabPanel("BLS LAUS", 
                 h5(a("BLS LAUS Data", href = "https://www.bls.gov/web/metro/laucntycur14.txt", target = "_blank"), align = "center"), 
                 p(a("Zip file", href = "https://www.bls.gov/lau/#data", target = "_blank"), "Download file under 'County Data/Table'", align = "center"),
                 h5("Datapoints:"),
                 tags$ul(
                   tags$li("Labor Force Size", 
                           tags$ul(
                             tags$li("40 Counties"), 
                             tags$li("Top 4 Counties"), 
                             tags$li("Bottom 4 Counties"), 
                             tags$li("4 Regions (KentuckianaWorks, Bluegrass, Northern Kentucky, Lincoln Trail")
                           )))),
                
        tabPanel("Burning Glass", 
                 h5(a("Burning Glass Data", href = "http://laborinsight.burning-glass.com/jobs/us#", target = "_blank"), align = "center"), 
                 h5("Datapoints:"), 
                 tags$ul(
                   tags$li(tags$s("Top Industries by Job Postings"), "(Completed April 26th - BD, ~/Desktop/regionalPlan/topIndustries/)"),
                   tags$li(tags$s("Top 30 occupations by Job Postings"), "(Completed April 26th - BD, ~/Desktop/regionalPlan/topOccupations/)"),
                   tags$li("Top 30 occupations + advertised education by job postings (NEED CLARIFICATION FROM ERIC)"))))
    )),   
    tabPanel("Data Updates",
             h3("Various data updates", align = "center"), 
             p("", 
               align = "center"),
               # fileInput('datafile', h6('Choose CSV file'),
               #           accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               # 
               # textInput("text", label = h6("Enter file name:"), value = ""),
               # actionButton("do", "Save"),
               # tableOutput("filetable")),
             
             mainPanel(
               tabsetPanel(
                 tabPanel('Median Wage Adjusted for Cost of Living (COL)', 
                          h5(a('Data', href = 'https://www.bls.gov/oes/current/oessrcma.htm', target = "_blank"), align = "center") 
                          ), 
                 tabPanel("Cradle to Career",
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
                                          Current (the year) Jobs, and Typical Entry Level Education, make sure the occupations are at the 5-digit level'))),
                 tabPanel('Career Calculator',
                          h5('Data update schedule can be found here:', 
                             a('Career Calculator Update Schedule',
                               href = 'https://docs.google.com/document/d/1dXribqXG8DJbaM_NsCI20mfLfI73PB-c7805_O0dv_g/pub')), 
                          h3('Updating the App'), 
                          p('You need access to the login credentials for the AWS console found here:', 
                            a('AWS credentials', 
                              href = 'https://docs.google.com/a/kentuckianaworks.org/spreadsheets/d/1cxUQCuoZ6PPvkXAyrGu3Gye1wWG4VRnCD-2Um4Ttecc/edit?usp=sharing')), 
                          p('Once logged in navigate to "Services", then "S3"'), 
                          p('This is where you will find a data "bucket", with downloadable and uploadable data connected to the app'), 
                          p('To update a specific data point, download the datasheet, edit, save, then upload back into the same S3 bucket'), 
                          p('To upload an entire data set, just upload the data set into the bucket, the app is programmed to
                            automatically select and place the new data')),
                 
                tabPanel('Monthly Newsletter', 
                      p('Every month (in partnership with the KW communications department) we
                        release a newsletter with BLS updates for the Louisville MSA area'),
                      h6('Schedule of updates can be found here:',
                         a('BLS MSA Release Schedule', href = 'http://www.bls.gov/schedule/news_release/metro.htm')),
                      br(),
                      h5('Data you will need:'), 
                      tags$ul(
                        tags$li("Total # of job postings in reference month(Burning Glass (BG), saved report 'monthlyUpdateJobPostings')"), 
                        tags$li("# of total postings asking for an associate's degree or higher (BG, saved report 'associatesPlus'"),
                        tags$li("# of total postings asking for a bachelor's degree or higher (BG, saved report 'bachelorPlus'"), 
                        tags$li("BLS data release date (should be today, the day you are updating. Link to release schedule found above :)")
                      ),
                      br(),
                      h5('To update the data and visualizations follow these steps:'), 
                      tags$ol(
                        tags$li('Open shinyBlsUpdates.Rproj'),
                        tags$li('In the project files open dataUpdate.R and RUN the script'),
                        tags$li('Next, open global.R and update lines 7 -10 with the data you pulled from Burning Glass, and the 
                                Data Release date.'),
                        tags$li('Run and republish app'), 
                        tags$li("Send communication's department new numbers")
                        ),
                      br(),
                      h5('Sweet, you did it :)'),
                      br(),
                      img(src='http://s2.quickmeme.com/img/37/374c8bc7e82177cc0d0a36c60ac42e2ae547d80ec7912ecf4c2cc0f895fde54e.jpg', 
                          width = 200, height = 200, align = "center"), 
                      br(),
                      br(),
                      br()
                 ))
             )),   
    
      tabPanel(
        'Datasets', 
        h1('The datasets...'), 
        h5('American Community Survey'),
        p(a('American Fact Finder', 
            href = "https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t", target = "_blank"), 
            'makes census data easier to find. We use the S2301 summary sheet for several labor force data. How to use 
          American Fact Finder:',
            tags$ol(
              tags$li("1. Choose a dataset (usually 1 year estimates)"), 
              tags$li("2. Choose a geography (usually Louisville MSA)"), 
              tags$li("3. Choose topic, or search summary sheet key (ex. S2301, DP03)"), 
              tags$li("4. From here you can open and download the summary sheet of interest")
            ), 
          tags$ul(h5('Common summary sheets:'), 
                  tags$li('S2301: EMPLOYMENT STATUS'), 
                  tags$li('S0501: SELECTED CHARACTERISTICS OF THE NATIVE AND FOREIGN-BORN POPULATIONS'), 
                  tags$li('DP03: SELECTED ECONOMIC CHARACTERISTICS'), 
                  tags$li('S1501: EDUCATIONAL ATTAINMENT')
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
        downloadLink('datasetInfo', 'Download Helpful Document about datasets'), 
        br(),
        br(),
        br(),
        br()
      ), 
    tabPanel(
      'Other', 
      h5('MIT Living Wage', a('here.', 
                              href = 'http://livingwage.mit.edu/metros/31140')), 
      p('(we use the measure for a family of two with two children w/o childcare).'),
      br(),
      h5('Login Credentials', a('here.', 
                                href = 'https://docs.google.com/a/kentuckianaworks.org/spreadsheets/d/1cxUQCuoZ6PPvkXAyrGu3Gye1wWG4VRnCD-2Um4Ttecc/edit?usp=sharing'))
    )


  ))))

