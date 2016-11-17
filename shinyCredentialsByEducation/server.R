library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)
library(googleVis)
library(RCurl)

## LOAD DATA
credentialByEducationLevel   <- read.csv("credentialByEducation.csv")
sankey                       <- read.csv('sankey.csv')
sankey$source <- as.character(sankey$source)

## Add Number to Count Observations
credentialByEducationLevel$n <- 1

## FILTERS
credentialByEducationLevel   <- credentialByEducationLevel %>%  filter(Degree != 'na')

## COUNT BY DEGREE LEVEL AND CERTIFICATION
credentialByEducationLevel   <- credentialByEducationLevel %>% 
                                    group_by(Degree, Certification) %>%
                                    tally  %>%
                                    group_by(Degree) 

## ADD COMMAS FOR LABELS
credentialByEducationLevel$commaNumber <- format(credentialByEducationLevel$nn, big.mark = ',')

## ADD LABELS WITH NAME AND NUMBER OF CERTIFICATIONS WITH COMMAS
credentialByEducationLevel$label       <- paste(credentialByEducationLevel$Certification, 
                                            credentialByEducationLevel$commaNumber,  
                                            sep = '\n') 

## SANKEY COLOR SETTINGS
colors_link       <- c('#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
                       '#cab2d6', '#ffff99', '#1f78b4', '#33a02c')

colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

colors_node_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

opts              <- paste0("{
                         link: { colorMode: 'gradient',
                         colors: ", colors_link_array ," },
                         node: { colors: ", colors_node_array ,"}}" )



## SHINY SERVER
shinyServer(function(input, output) {
  
  credentials <- reactive({ credentialByEducationLevel <- credentialByEducationLevel %>%
                                          filter(Degree == input$select) %>%
                                          top_n(10, wt = nn)})
  
  
  ## OUTPUT PLOTS
  output$value <- renderPlot({
    
              treemap(credentials(),  index = 'label', vSize = 'nn',
                      vColor = 'Certification', 
                      title  = '')})
  
  sankeyData <- reactive({sankey <- sankey %>%
    filter(Occupation == input$occupationGroup) %>%
    select(2:4) %>%
    top_n(10, wt = value)
  })
  
  output$view  <- renderGvis({

                  gvisSankey(sankeyData(), 
                             from    = "source",
                             to      = "target",
                             weight  = "value" ,
                             options = list(height = 500,
                                            width  = "100%", 
                                            sankey = opts))})

})


#sankey ="{link: {color: { fill: '#73AFD4' } },
#                                      node : { width: 4, 
#color:   { fill: 'black' },

#label:   { fontName: 'Helvetica',
#fontSize: 16,
#color: '#333'}}}"))

#  output$it <- renderUI(
#      htmlTemplate('index.html', 
#          sankey = sankey)) 
sankey <- sankey %>%
  filter(Occupation == "Management") %>%
  select(2:4) %>%
  top_n(10, wt = value)

gvisSankey(sankey, 
           from    = "source",
           to      = "target",
           weight  = "value" ,
           options = list(height = 500,
                          width  = "100%", 
                          sankey = opts))

