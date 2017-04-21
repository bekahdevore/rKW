shinyUI(pageWithSidebar(
  headerPanel("CSV File Upload Demo"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    textInput("text", label = p("Enter file name:"), value = ""),
    actionButton("do", "Save")
  ),
  mainPanel(
    tableOutput("filetable")
#    tableOutput("geotable")
  )
))