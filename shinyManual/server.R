
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(googlesheets)

shinyServer(function(input, output) {

  output$datasetInfo <- downloadHandler(
    filename = "aboutDatasets.docx",
    content = function(file) {
      file.copy("aboutDatasets.docx", file)
  })
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  saveFile <- reactive({
    read.csv(infile)
  })
  
  fileName <- reactive({
    name <- input$text
  })
  
  saveGoogle <- reactive({
    withProgress(message = 'Saving ...', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("in progress "))
      }
      iris_ss <- gs_new(fileName(), input = filedata())
    })
    
    
    showNotification("Saved")
    
  })
  
  observeEvent(input$do, {
    saveGoogle()
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata()
  })

})
