
library(shiny)
library(dplyr)
library(DT)
library(RCurl)
library(ggplot2)
library(scales)


masters <- read.csv("mastersFinalFourState.csv")
bachelors <- read.csv("bachelorsFinalFourState.csv")
allDataFinal <- read.csv("allDataFinalFourState.csv")

masters   <- masters %>% select(2:4)
bachelors <- bachelors %>% select(2:4)
allDataFinal <- allDataFinal %>% select(-1)

colors_link <- c('#9C0059', '#A4D7F4')
colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

colors_node <- c('grey', 'grey')
colors_node_array <- paste0("[", paste0("'", colors_node,"'", collapse = ','), "]")

opts <- paste0("{
               link: { colorMode: 'source',
               colors: ", colors_link_array ," },
               node: { colors: ", colors_node_array ," }
               }" )

cbPalette <- c(
  '#9C0059',
  '#A4D7F4',
  '#8DC63F',
  '#F8971D',
  '#D31245',
  '#A4D7F4',
  '#00853F',
  '#767662'
)


shinyServer(function(input, output) {
  
  sankeyData <- reactive({
    dataSet <-input$dataSet
    if(dataSet == "Masters") {
      return(masters)
    } else {
      return(bachelors)
    }
  }) 
  
  output$view <- renderGvis({
    
    gvisSankey(
      sankeyData(), 
      from= "occGroup",
      to="DataPoint",
      weight = "percent",
      options=list(height =1000,
                   width = 900, 
                   sankey = opts
                   # sankey = "{link:{
                   # color:{fill:'black'}
                   # }}"
      )
    )    
  })
  
  output$allData <- renderPlot({
    p <- ggplot(allDataFinal, aes(x = Type, y = percent, fill = race, label = label)) + 
      geom_bar(stat = 'identity', position = 'dodge') + facet_grid(~ education) + 
      geom_text(position = position_dodge(width = 1), hjust = -.10)
    
    
    highDemand <- p                                  + 
      coord_flip()                       + 
      facet_grid(education ~ ., switch = 'y') + 
      #theme_minimal()                    +
      theme(strip.text.y = element_text(#angle = 180, 
        # hjust = 1, 
        size = 9, 
        face = 'bold'),
        # strip.background  = element_rect(fill   = 'white'),
        # color  = 'grey'),
        # panel.background  = element_rect(fill   = 'white'),
        # color  = 'grey'),
        # axis.text.y       = element_blank(), 
        axis.ticks.y      = element_blank(), 
        axis.text.x       = element_text(size = 9), 
        legend.title      = element_blank(), 
        legend.text       = element_text(size = 14),
        # face = 'bold'), 
        legend.position   = c(.9, .6), 
        legend.background = element_blank(),
        # legend.key        = element_rect(color = 'white', 
        #                                  size = 3),
        legend.key.size   = unit(1, 'lines'),
        axis.title        = element_blank()) +
      scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) + 
      scale_fill_manual(values = cbPalette)
    
    highDemand
  }, height = 900)
})
