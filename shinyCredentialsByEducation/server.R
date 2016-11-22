library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)
library(googleVis)
library(RCurl)
library(plotly)
library(ggplot2)
library(plyr)


## LOAD DATA
credentialByEducationLevel   <- read.csv("credentialByEducation.csv")
sankey                       <- read.csv('sankey.csv')
sankeyAll                    <- read.csv('sankeyAll.csv')
majors                       <- read.csv('majors.csv')


sankeyFilter <- function(dataHere){
  colnames(dataHere)[4] <- 'value'
  dataHere <- dataHere %>% select(2:7)
}

sankey    <- sankeyFilter(sankey)
sankeyAll <- sankeyFilter(sankeyAll)

## Add Number to Count Observations
credentialByEducationLevel$n <- 1
majors$n                     <- 1

## FILTERS
credentials                <- credentialByEducationLevel %>%  filter(Degree != 'na')

## COUNT BY DEGREE LEVEL AND CERTIFICATION
credentialByEducationLevel   <- credentials %>% 
                                    group_by(Degree, Certification) %>%
                                    tally  %>%
                                    group_by(Degree) 

credentialByEducationLevel$nn            <- as.numeric(as.character(credentialByEducationLevel$nn))
credentialByEducationLevel$Certification <- as.character(credentialByEducationLevel$Certification)

## ADD COMMAS FOR LABELS
#credentialByEducationLevel$commaNumber <- format(credentialByEducationLevel$nn, big.mark = ',')

## ADD LABELS WITH NAME AND NUMBER OF CERTIFICATIONS WITH COMMAS
#credentialByEducationLevel$label       <- paste(credentialByEducationLevel$Certification, 
#                                            credentialByEducationLevel$commaNumber,  
#                                            sep = '\n') 

## SANKEY COLOR SETTINGS
colors_link       <- c('#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
                       '#cab2d6', '#ffff99', '#1f78b4', '#33a02c')

colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

colors_node_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

opts              <- paste0("{
                         link: { colorMode: 'gradient',
                         colors: ", colors_link_array ," },
                         node: { colors: ", colors_node_array ,", 
                                label: {fontSize: 16}}}" )



## SHINY SERVER
shinyServer(function(input, output) {
  
  #credentials <- reactive({ 
   # if(input$select == 'All'){
    #  credentials <- credentials %>%
     #   top_n(10, wt = n)
    #}
    #else {
    #credentialByEducationLevel <- credentialByEducationLevel %>%
     #                                     filter(Degree == input$select) %>%
    #                                      top_n(10, wt = nn)}
    #})
  
  credentialsBar <- reactive({ 
    #if(input$select == 'All') {
     # credentialByEducationLevel <- credentialByEducationLevel %>%
        #filter(nn >= 0) %>%
      #top_n(10, wt = nn)
    #}
    #else {
    credentialByEducationLevel <- credentialByEducationLevel %>%
    filter(Degree == input$select) %>%
    #filter(nn >= 0) %>%
    top_n(10, wt = nn)
    #}
    arrange(credentialByEducationLevel, desc(nn))
    })
  
  
  
  
  sankeyData <- reactive({sankey <- sankey %>%
    filter(Occupation == input$occupationGroup) %>%
    filter(Median.Hourly.Earnings >= input$wageSlide) %>%
    select(1:3) %>%      
    top_n(10, wt = value)
  })
  
  majorsData <- reactive({
    majors <- majors %>%
      filter(Degree   == input$majorsDegree) %>%
      filter(STDMajor == input$majors)
    
    majors    <- dplyr::count(majors, Certification, n)
    majors    <- majors %>% select(1,3)
    majors$nn <- as.numeric(as.character(majors$nn))
    majors$label <- paste(majors$Certification,'\n', '(', majors$nn, 'postings',')')
    majors    <- head(arrange(majors, desc(nn)), n = 15)
    
    
  })
  
  #sankeyAllData <- reactive({sankeyAll <- sankeyAll %>%
   # filter(Occupation == input$occupationGroup) %>%
    #select(1:3) %>%      
    #top_n(50, wt = value)
  #})
  
  
  ## OUTPUT PLOTS
  #output$value <- renderPlot({
    
   #           treemap(credentials(),  index = 'label', vSize = 'nn',
   #                  vColor = 'Certification', 
   #                  title  = '')
  #})
  
  ## NEED TO WORK ON THIS
  output$educationBar <- renderPlot({

    g <- ggplot(credentialsBar(), 
                aes(x = reorder(Certification, nn), 
                    y = nn, 
                    fill = -nn, 
                    label = nn)) 
    g + geom_bar(stat  = 'identity', 
                 color = 'white') + 
      coord_flip() + 
      ylab('Job Postings') +
      theme(
        legend.position = 'none', 
        axis.ticks      = element_blank(), 
        axis.title.y    = element_blank(), 
        axis.text       = element_text(size = 16, 
                                       face = 'bold')) + 
      geom_text(color = 'white', hjust = 1.3)

    
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
  
   output$majors <- renderPlot({
     
     treemap(majorsData(),  index = 'label', vSize = 'nn',
             vColor = 'Certification', 
             title  = '')
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