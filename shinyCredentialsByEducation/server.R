library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)
library(googleVis)
library(RCurl)

## LOAD DATA
credentialByEducationLevel   <- read.csv("credentialByEducation.csv")
sankey                       <- read.csv('sankey.csv')
sankeyAll                    <- read.csv('sankeyAll.csv')


sankeyFilter <- function(dataHere){
  colnames(dataHere)[4] <- 'value'
  dataHere <- dataHere %>% 
            select(2:7)
}

sankey    <- sankeyFilter(sankey)
sankeyAll <- sankeyFilter(sankeyAll)

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
                         node: { colors: ", colors_node_array ,", 
                                label: {fontSize: 18}}}" )



## SHINY SERVER
shinyServer(function(input, output) {
  
  credentials <- reactive({ credentialByEducationLevel <- credentialByEducationLevel %>%
                                          filter(Degree == input$select) %>%
                                          top_n(10, wt = nn)})
  
  sankeyData <- reactive({sankey <- sankey %>%
    filter(Occupation == input$occupationGroup) %>%
    filter(Median.Hourly.Earnings >= input$wageSlide) %>%
    select(1:3) %>%      
    top_n(10, wt = value)
  })
  
  #sankeyAllData <- reactive({sankeyAll <- sankeyAll %>%
   # filter(Occupation == input$occupationGroup) %>%
    #select(1:3) %>%      
    #top_n(50, wt = value)
  #})
  
  
  ## OUTPUT PLOTS
  output$value <- renderPlot({
    
              treemap(credentials(),  index = 'label', vSize = 'nn',
                      vColor = 'Certification', 
                      title  = '')
    })
  

  
  output$view  <- renderGvis({

                  gvisSankey(sankeyData(), 
                             from    = "source",
                             to      = "target",
                             weight  = "value" ,
                             options = list(height = 700,
                                            width  = "100%", 
                                            sankey = opts))
    })
  
  #output$sankeyAll  <- renderGvis({
    
   # gvisSankey(sankeyAllData(), 
    #           from    = "source",
    #           to      = "target",
    #           weight  = "value" ,
    #           options = list(height = 800,
    #                          width  = "100%", 
    #                          sankey = opts))
  #})

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
#sankey$Occupation <- as.character(sankey$Occupation)
#test <- sankey %>% subset(socGroup == 11) %>%
#  select(2:4) %>%
#  top_n(10, wt = value)
#sankey[sankey$Occupation == "Management"]
#gvisSankey(sankey, 
#           from    = "source",
#           to      = "target",
#           weight  = "value" ,
#           options = list(height = 500,
#                          width  = "100%", 
#                          sankey = opts))


# notice how few arguments we need now
# some output but not the nice output I expect