library(shiny)
library(shinythemes)
library(dplyr)
library(DT)

healthList <- read.csv("occupationList.csv")
manuList   <- read.csv("manufacturingOccupationList.csv")
govList    <- read.csv("govOccupationList.csv")
animalList <- read.csv("animalList.csv")

healthList <- select(healthList, 2)
manuList   <- select(manuList,   2)
govList    <- select(govList,    2)
animalList <- select(animalList, 2)


shinyUI(fluidPage(
  theme = shinytheme("united"),
  # Application title
  mainPanel(
    tabsetPanel(
      tabPanel("Data", 
        h1("Health Science Pathways"), 
        h4("Projected Jobs"),
        dataTableOutput("healthData"),
        p("Based on the Louisville area job growth, potiential turnover and retirements in the following occupations", 
                    tags$ul(
                      tags$li(healthList$x[1]), 
                      tags$li(healthList$x[2]), 
                      tags$li(healthList$x[3]), 
                      tags$li(healthList$x[4]), 
                      tags$li(healthList$x[5]), 
                      tags$li(healthList$x[6]), 
                      tags$li(healthList$x[7]), 
                      tags$li(healthList$x[8]), 
                      tags$li(healthList$x[9]), 
                      tags$li(healthList$x[10])
          )),

        h1("Advanced Manufacturing Pathways"), 
        h4("Projected Jobs"),
        dataTableOutput("manuData"), 
         p("Based on the Louisville area job growth, potiential turnover and retirements in the following occupations", 
                    tags$ul(
                      tags$li(manuList$x[1]), 
                      tags$li(manuList$x[2]), 
                      tags$li(manuList$x[3]), 
                      tags$li(manuList$x[4]), 
                      tags$li(manuList$x[5]), 
                      tags$li(manuList$x[6]), 
                      tags$li(manuList$x[7]), 
                      tags$li(manuList$x[8]), 
                      tags$li(manuList$x[9]), 
                      tags$li(manuList$x[10]), 
                      tags$li(manuList$x[11]), 
                      tags$li(manuList$x[12]), 
                      tags$li(manuList$x[13]), 
                      tags$li(manuList$x[14]), 
                      tags$li(manuList$x[15]), 
                      tags$li(manuList$x[16]), 
                      tags$li(manuList$x[17]), 
                      tags$li(manuList$x[18]), 
                      tags$li(manuList$x[19]), 
                      tags$li(manuList$x[20]),
                      tags$li(manuList$x[21]), 
                      tags$li(manuList$x[22])
                    )),
        h1("Government, Law and Public Administration Pathways"), 
        h4("Projected Jobs"),
        dataTableOutput("govData"),
        p("Based on the Louisville area job growth, potiential turnover and retirements in the following occupations", 
                  tags$ul(
                    tags$li(govList$x[1]), 
                    tags$li(govList$x[2]), 
                    tags$li(govList$x[3]), 
                    tags$li(govList$x[4]), 
                    tags$li(govList$x[5]), 
                    tags$li(govList$x[6]), 
                    tags$li(govList$x[7]), 
                    tags$li(govList$x[8]), 
                    tags$li(govList$x[9]), 
                    tags$li(govList$x[10]), 
                    tags$li(govList$x[11])
                  )),
        h1("Animal and Food Science Pathways"), 
        h4("Projected Jobs"),
        dataTableOutput("animalData"),
        p("Based on the Louisville area job growth, potiential turnover and retirements in the following occupations", 
          tags$ul(
            tags$li(animalList$x[1]), 
            tags$li(animalList$x[2]), 
            tags$li(animalList$x[3]), 
            tags$li(animalList$x[4]), 
            tags$li(animalList$x[5]), 
            tags$li(animalList$x[6]), 
            tags$li(animalList$x[7]), 
            tags$li(animalList$x[8]), 
            tags$li(animalList$x[9])
          ))),
      
    tabPanel("About",
             p("Projected jobs and retirements come from", a("EMSI analyst", href = "http://www.economicmodeling.com/analyst/"), ". 
               EMSI aggregates", a("Census", href = "http://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml"),  
               "and", a("BLS", href = "http://www.bls.gov/"), "data and develops projection models."), 
             p("Turnover rates come from the", a("BLS Job Openings and Labor Turnover Survey (JOLTS)", href = "http://www.bls.gov/jlt/jltfaq.htm"))
      
    )
    
    ))))




