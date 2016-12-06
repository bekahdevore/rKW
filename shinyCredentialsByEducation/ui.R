library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)
library(googleVis)


employersList              <- read.csv('employersList.csv')
majors                     <- read.csv('majorsList.csv')
occupationNames            <- read.csv('occupationList.csv')
certificationList          <- read.csv('certificationList.csv')

t                            <- 23
w                            <- "All"
all                          <- cbind(t, w)
colnames(all)                <- c("X", "occupation")
colnames(occupationNames)[2] <- "occupation"
occupationNames              <- rbind(occupationNames, all)
rm(all)

employersList              <- employersList   %>% select(2)
colnames(employersList)[1] <- "x"
employersList              <- employersList   %>% filter(x != 'na') %>% arrange(x)
majors                     <- majors          %>% arrange(x)
occupationNames            <- occupationNames %>% arrange(occupation)


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
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  theme = shinytheme('cosmo'),
  title = 'Job Postings in the Louisville MSA',
  
  tabPanel('Occupation', 
           #tags$script(src="https://d3js.org/d3.v3.min.js"),
           #tags$script(src="sankey.js"),
           #tags$div(id="chart"),
           h2("Credentials by Occupation Group"),
           p("Here you can select a major occupation group and median wage and you will see the top occupations (left) and credentials (right)
             job postings are asking for."),
           selectInput("occupationGroup", 
                       label = h4("Choose an occupation group:"), 
                       choices = unique(occupationNames$occupation)), 
           sliderInput("wageSlide", 
                       "Occupations with a median wage at or above:", 
                       min=0, 
                       max=216216, 
                       value=47273), 
           
            htmlOutput('view')#,
           #h1('break'),
            #htmlOutput('occupationGroup')
           #br(), 
           #br(), 
           #h2('Including postings not asking for a credential:'), 
           #htmlOutput('sankeyAll')
  ),
  
  tabPanel('Education', 
          fluidPage(
            fluidRow(
          column(2, 
          selectInput("select", 
                      label = h4("Select an education level"), 
                      choices = selectChoices, 
                      0)),
          column(10, align ='center', 
                 h2("Top Credentials by Education Level"), 
                 p("Here you can select an education level and see the top credentials job postings
                   are asking for."))),
          fluidRow(
          #plotOutput('value'), 
          column(12, plotOutput('educationBar'))),
          br(), 
          br(), 
          fluidRow(
          column(2,
                 selectInput('majorsDegree', 
                      label   = h4('Select an education level'), 
                      choices = selectChoicesMajor)),
          column(10, align = 'center',
                 h2('Credentials by Education Level and Major'), 
                 p("Here you can select an education level and major 
                   and you will see the top credentials job postings are asking for."))),
          fluidRow(
          column(2, 
                 selectizeInput('majors', 
                      label   = h4('Select/type a major'), 
                      choices = unique(majors$x))), 
          column(10, align ='center',
                 plotOutput('majors'))),
          br(), 
          br(), 
          br(), 
          br(), 
          br(), 
          br()
  )),
  
  tabPanel('Employer', 
           h2("Credentials by Employer"),
           p("Here you can select/type an employer and see the top credentials job postings are asking for."),
           selectizeInput('employers', "Select/type an employer:", 
                          choices = employersList$x, 
                          selected = 'Humana'), 
           plotOutput('employers'), 
           br(), 
           br(), 
           h2("Employers and Occupations by Credential"), 
           p("Here you can select/type a credential and see the top employers (left) and occupations (right)
             job postings are asking for."),
           selectizeInput('certification', "Select/type a credential", 
                          choices = certificationList$x,
                          selected = 'SIX SIGMA CERTIFICATION'),
           htmlOutput('credentialEmployer')
           ),
  
  tabPanel('About Data', 
           h1('Burning Glass, Labor Insights'),
           p('Online Job postings in the Louisville MSA, January 2015 - June 2016 '), 
           br(),
           br(),
           br(),
           br(),
           h4("For questions or comments please contact the KentuckianaWorks Labor Market Intelligence Department at",
             a("lmi@kentuckianaworks.org", href = "mailto:lmi@kentuckianaworks.org"))
           
  ))
