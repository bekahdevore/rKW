
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$datasetInfo <- downloadHandler(
    filename = "aboutDatasets.docx",
    content = function(file) {
      file.copy("aboutDatasets.docx", file)
  })

})
