library(shiny)
library(shinythemes)
library(dplyr)
library(treemap)
library(googleVis)
library(RCurl)

credentialByEducationLevel   <- read.csv("credentialByEducation.csv")
emsiWageConnection           <- getURL("https://docs.google.com/spreadsheets/d/1CT9R_MIs_s7ULDm-RCexVsW9m6LZa26EqH0Cd3LEB3k/pub?gid=0&single=true&output=csv")
credentialByEducationLevel$n <- 1

wageRanges                   <- read.csv(textConnection(emsiWageConnection))
wageRanges                   <- select(wageRanges, 1:4)

credentialByEducationLevel   <- credentialByEducationLevel %>%
                                    filter(Degree != 'na')

credentialByEducationLevel   <- credentialByEducationLevel %>% 
                                    group_by(Degree, Certification) %>%
                                    tally  %>%
                                    group_by(Degree) 

credentialByEducationLevel$commaNumber <- format(credentialByEducationLevel$nn, big.mark = ',')

credentialByEducationLevel$label       <- paste(credentialByEducationLevel$Certification, 
                                            credentialByEducationLevel$commaNumber,  
                                            sep = '\n') 

sankey <- read.csv('sankey.csv')
sankey <- sankey %>% 
            filter(socGroup != 55)

sankey <- left_join(sankey, wageRanges, by = 'SOC')

sankey$label       <- paste(sankey$source, 
                            "(", sankey$Pct..25.Hourly.Earnings, "-",
                            sankey$Pct..75.Hourly.Earnings,")",
                            sep = '\n') 

sankey <- sankey %>% select(11, 3:7)
colnames(sankey)[1] <- 'source'

shinyServer(function(input, output) {
  
  credentials <- reactive({
                      credentialByEducationLevel <- credentialByEducationLevel %>%
                      filter(Degree == input$select) %>%
                      top_n(10, wt = nn)
                      
  })
  
  output$value <- renderPlot({
    
    treemap(credentials(),  index = 'label', vSize = 'nn',
            vColor = 'Certification', 
            title  = '')
  })
  
#  output$it <- renderUI(
#      htmlTemplate('index.html', 
#          sankey = sankey)) 
  
  output$view  <- renderGvis({
    sankeyData <- reactive({
      test  <- sankey %>%
        filter(Occupation == input$occupationGroup) %>%
        select(1:3) %>%
        top_n(10, wt = value) 
    })
    
    colors_link <- c('#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
                     '#cab2d6', '#ffff99', '#1f78b4', '#33a02c')
    
    colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

    colors_node_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")
    
    opts <- paste0("{
              link: { colorMode: 'gradient',
                      colors: ", colors_link_array ," },
              node: { colors: ", colors_node_array ,"}}" )
    
    gvisSankey(sankeyData(), 
               from    = "source",
               to      = "target",
               weight  = "value" ,
               options = list(height = 500,
                              width  = "100%", 
                              sankey = opts))
  })
})


#sankey ="{link: {color: { fill: '#73AFD4' } },
#                                      node : { width: 4, 
#color:   { fill: 'black' },

#label:   { fontName: 'Helvetica',
#fontSize: 16,
#color: '#333'}}}"))
    

