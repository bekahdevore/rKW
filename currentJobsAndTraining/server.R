library(RCurl)
library(dplyr)
library(shiny)
library(stringr)

dataConnectionBg <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=0&single=true&output=csv')
dataConnectionEmsi <- getURL('https://docs.google.com/spreadsheets/d/1DjmOHHFiPAyCKXkze6e8EVHBSGv-N7qt5rX1KezQUx0/pub?gid=1224165436&single=true&output=csv')
dataConnectionSectors <- getURL('https://docs.google.com/spreadsheets/d/1n0DVC8fBwTr6ne9Fw5dFaRtO3ka-Y2i8zweFGIzmVcQ/pub?gid=624724603&single=true&output=csv')

burningGlass <- read.csv(textConnection(dataConnectionBg), check.names = FALSE)
emsi         <- read.csv(textConnection(dataConnectionEmsi), check.names = FALSE)
sectors      <- read.csv(textConnection(dataConnectionSectors), check.names = FALSE)

allData <- left_join(burningGlass, emsi, by = "SOC")

splitSOC <- as.data.frame(t(sapply(allData$SOC, 
            function(x) substring(x, 
                                  first=c(1, 1), 
                                  last=c(2, 7)))))

colnames(splitSOC)[1] <- "socGroup"
colnames(splitSOC)[2] <- "SOC"

splitSOC$socGroup <- as.numeric(as.character(splitSOC$socGroup))
allData <- left_join(allData, splitSOC, by = "SOC")
allData <- left_join(allData, sectors, by = "socGroup")

allData$`Number of Job Postings` <- str_replace_all(allData$`Number of Job Postings`, ",", "")
allData$`Age 55-64` <- str_replace_all(allData$`Age 55-64`, ",", "")
allData$`Age 65+` <- str_replace_all(allData$`Age 65+`, ",", "")
allData$`Median Hourly Earnings` <- str_replace_all(allData$`Median Hourly Earnings`, "\\$", "")

allData$`Number of Job Postings` <- as.numeric(allData$`Number of Job Postings`)
allData$`Median Hourly Earnings` <- as.numeric(allData$`Median Hourly Earnings`)


shinyServer(function(input, output) {
  
  allDataReactive <- reactive({
    if(input$sector == 'All'){ 
      allData <- allData %>% select(1:3, 6:9)
      
    } else {
      allData <- allData %>% filter(sector == input$sector)
      allData <- allData %>% select(1:3,6:9)
    }})

  
  output$table <- DT::renderDataTable(DT::datatable({
          allDataReactive()
  }))
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("jobData.csv", sep = '')
    },
    
    content = function(file) {
      write.csv(allDataReactive(), file)
    }
  )

})
