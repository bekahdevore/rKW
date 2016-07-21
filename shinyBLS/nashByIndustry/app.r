library(shiny)

load("data.Rda")
load("nola.Rda")
industries <- as.list(unique(nola$industry_name))

ui <- fluidPage(
  
  tags$h2("Nashville Historic and Forecasted Employment Numbers, by Industry"),
  tags$br(),
  wellPanel(
    fluidRow(
      column(6, selectInput("industry", "Select industry:", choices = industries)),
      column(6, sliderInput(inputId = "start.year",
                            label = "Select the year with which to start the graph:",
                            value = 1996, min = 1990, max = 2016, sep=""))
    )
  ),
  tags$br(),
  plotOutput("graph"),
  
  tags$p("Black line represents observed, historic data.
         Blue line represents forecasts, with 80% (dark grey) and 95% (light grey) confidence intervals.
         Forecasts are generated using exponential smoothing.
         "),
  tags$hr(),
  tags$p("Industry and employment data from The US Bureau of Labor Statistics",
         tags$a(href = "http://www.bls.gov/ces/", "Current Employment Statistics"), "survey.")
  
  )

server <- function(input, output) {
  
  industries <- as.list(unique(nola$industry_name)) 
  
  output$graph <- renderPlot({
    
    series <- as.character(subset(nola, industry_name == input$industry, select = "series_id")) 
    
    temp.data <- subset(data, series_id == series & year >= input$start.year)
    
    # Create time series
    temp.data.ts <- ts(temp.data$employees, frequency=12, start=c(temp.data$year[1],temp.data$month[1]))
    
    # Generate forecast
    library(fpp)
    ets.model <- ets(temp.data.ts)
    
    library(forecast)
    ets.fcasts <- forecast(ets.model, h=12, level=c(80,95))
    ets.fcasts <- as.data.frame(ets.fcasts)
    ets.fcasts$date <- as.Date(as.yearmon(row.names(ets.fcasts))) + 14
    names(ets.fcasts) <- c('forecast','lo80','hi80','lo95','hi95','date')
    
    temp.data <- merge(temp.data, ets.fcasts, by="date", all = TRUE)
    
    library(ggplot2)
    library(scales) # formatting numbers
    
    title <- paste(input$industry, "Employees", sep = " ")
    title <- paste(title, "\nNashville, Nashville Metropolitan Area", sep = "")
    
    graph <- ggplot(temp.data, aes(x=temp.data$date, y=temp.data$employees))
    graph <- graph + geom_line()# add a line connecting the dots
    graph <- graph + geom_ribbon(aes(x = temp.data$date, ymin = temp.data$lo95, ymax = temp.data$hi95), fill="lightgrey")
    graph <- graph + geom_ribbon(aes(x = temp.data$date, ymin = temp.data$lo80, ymax = temp.data$hi80), fill="darkgrey")
    graph <- graph + geom_line(aes(x = temp.data$date, y = temp.data$forecast), colour="blue")
    graph <- graph + labs(x="Year")
    graph <- graph + scale_y_continuous(name="Number of Employees", labels = comma)
    graph <- graph + ggtitle(title)
    graph
  })
  
}

shinyApp(ui = ui, server = server)