library(shiny)
library(shinythemes)
library(plyr)
#library(dplyr)
library(treemap)
library(googleVis)
library(RCurl)
library(ggplot2)



## LOAD DATA
credentialByEducationLevel   <- read.csv("credentialByEducation.csv")
sankey                       <- read.csv('sankey.csv')
majors                       <- read.csv('majors.csv')
employers                    <- read.csv('employers.csv')

#credentialByEducationLevel <- credentialByEducationLevel %>% select(2:4)
#credentialByEducationLevel$Certification <- as.character(credentialByEducationLevel$Certification)
#credentialByEducationLevel$nn            <- as.numeric(as.character(credentialByEducationLevel$nn))

sankeyFilter <- function(dataHere){
  colnames(dataHere)[4] <- 'value'
  dataHere <- dataHere %>% select(2:7)
}

sankey    <- sankeyFilter(sankey)
#sankeyAll <- sankeyFilter(sankeyAll)
majors$n <- 1

sankey$label <- as.character(sankey$label)
sankey$Certification <- as.character(sankey$Certification)
sankey$value <- as.numeric(as.character(sankey$value))

colnames(sankey)[1] <- 'source'
colnames(sankey)[2] <- 'target'
colnames(sankey)[3] <- 'value'
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
    select(1:3)# %>%      
    #top_n(10, wt = 'value')
    sankey <- head(arrange(sankey, desc(value)), n = 10)
  })
  
  majorsData <- reactive({
    majors <- majors %>%
      filter(Degree   == input$majorsDegree) %>%
      filter(STDMajor == input$majors)
    
    majors    <- dplyr::count(majors, Certification, n)
    majors    <- majors %>% select(1,3)
    majors$nn <- as.numeric(as.character(majors$nn))
    majors$label <- paste(majors$Certification,'\n', '(', majors$nn, 'postings',')')
    majors <- head(arrange(majors, desc(nn)), n = 15)
    
  })
  
  
  employersData <- reactive({
    employers <- employers %>%
      filter(Employer == input$employers)
    
    employers    <- dplyr::count(employers, Certification, n)
    #employers    <- employers %>% select(1,3)
    employers$nn <- as.numeric(as.character(employers$nn))
    employers$label <- paste(employers$Certification,'\n', '(', employers$nn, 'postings',')')
    employers       <- head(arrange(employers, desc(nn)), n = 15)
  })
  
  credentialEmployerData <- reactive({
    credential <- employers %>%
      filter(Certification == input$certification)
      credential$n <- as.numeric(credential$n)
      
      credential <- credential %>% select(2,4,5)
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
   
   output$employers <- renderPlot({
     
     treemap(employersData(),  index = 'label', vSize = 'nn',
             vColor = 'Certification', 
             title  = '')
   })
   
   output$credentialEmployer  <- renderGvis({
     
     gvisSankey(credentialEmployerData(), 
                from    = "Employer",
                to      = "SOCName",
                weight  = "n" ,
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