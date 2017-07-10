library("shiny")
library("shinydashboard")
library("datasets")

header <- dashboardHeader()

sidebar <- dashboardSidebar()

body <- dashboardBody(
  dataTableOutput("mtcarsTable")
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    
    output$mtcarsTable <- renderDataTable({
      datasets::mtcars
    }, options = list(rowCallback = I('
            function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                      // Bold and green cells for conditions
                                      if (parseFloat(aData[3]) >= 200)
                                      $("td:eq(3)", nRow).css("font-weight", "bold");
                                      if (parseFloat(aData[3]) >= 100)
                                      $("td:eq(3)", nRow).css("background-color", "#9BF59B");
  }'),
                      pageLength = 10, orderClasses = TRUE, searching = FALSE, paging = FALSE
    ))
    
    }
    )