
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(blsAPI)
library(RCurl)
library(RJSONIO)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)

####Load and clean data
#Start and End Years for BLS data and Month to Filter
startYear <- 2006
endYear   <- 2016
blsMonth  <- 'September'
apiKey    <- '2691a2506f514617823b4e653111fdc9'
#,'startyear'= startYear, 'endyear' = endYear
#Load data and parse it out with RJSONIO
#BLS series LNS14000000 returns the Seasonally Adjusted Unemployment Rate for individuals 16 and over.

###########################################################################################################
## FUNCTIONS ##
###########################################################################################################
#Convert Data from BLS API to DataFrame
apiDataConverter <- function(seriesId) {
       payload     <- list('seriesid'        = seriesId,
                           'startyear'       = startYear, 
                           'endyear'         = endYear, 
                           'registrationKey' = apiKey)
       bls.content <- blsAPI(payload)
       bls.json    <- fromJSON(bls.content, simplify=TRUE)
       tmp         <- bls.json$Results[[1]][[1]]
       bls.df      <- data.frame(year=sapply(tmp$data,"[[","year"),
                                 period=sapply(tmp$data,"[[","period"),
                                 periodName=sapply(tmp$data,"[[","periodName"),
                                 value=as.numeric(sapply(tmp$data,"[[","value")),
                                 stringsAsFactors=FALSE)
}

###########################################################################################################
## CREATE DATAFRAMES AND FORMAT ##
###########################################################################################################

#### Labor Force Series ID's ###
JeffersonCountyLaborForce <- apiDataConverter("LAUCN211110000000006")
KentuckyLaborForce        <- apiDataConverter("LAUST210000000000006")
LouisvilleLaborForce      <- apiDataConverter("LAUMT213114000000006")
UnitedStatesLaborForce    <- apiDataConverter("LNU01000000")

### Unemployment Rate Series ID's ###
JeffersonCountyUnemployment <- apiDataConverter("LAUCN211110000000003")
KentuckyUnemployment        <- apiDataConverter("LAUST210000000000003")
LouisvilleUnemployment      <- apiDataConverter("LAUMT213114000000003")
UnitedStatesUnemployment    <- apiDataConverter("LNU04000000")


#Add area variable to data
### Labor Force ###
JeffersonCountyLaborForce$area   <- "Jefferson County"
KentuckyLaborForce$area          <- "Kentucky"
LouisvilleLaborForce$area        <- "Louisville MSA"
UnitedStatesLaborForce$area      <- "United States"
#UnitedStatesLaborForce$value    <- (UnitedStatesLaborForce$value)*1000

### Unemployment ###
JeffersonCountyUnemployment$area <- "Jefferson County"
KentuckyUnemployment$area        <- "Kentucky"
LouisvilleUnemployment$area      <- "Louisville MSA"
UnitedStatesUnemployment$area    <- "United States"


################### Combine dataframes
### Labor Force ###
laborForceData       <- rbind(JeffersonCountyLaborForce, 
                              KentuckyLaborForce, 
                              LouisvilleLaborForce)

### Unemployment ###
unemploymentRateData <- rbind(UnitedStatesUnemployment,
                              KentuckyUnemployment, 
                              LouisvilleUnemployment, 
                              JeffersonCountyUnemployment
                              )

################## Filter for specified month
### Labor Force ###
laborForceData       <- laborForceData%>%
       filter(periodName == blsMonth)

### Unemployment ###
unemploymentRateData <- unemploymentRateData%>%
       filter(periodName == blsMonth)


###################### Convert values to numeric 
### Labor Force ###
laborForceData$value         <- as.numeric(as.character(laborForceData$value))
UnitedStatesLaborForce$value <- as.numeric(as.character(UnitedStatesLaborForce$value))

### Unemployment ###
unemploymentRateData$value   <- as.numeric(as.character(unemploymentRateData$value))

lastYearUnemployment <- unemploymentRateData %>%
                            filter(year == 2015 | year == 2016) %>%
                            select(area, value, year)


thisYearUnemployment <- unemploymentRateData %>%
                            filter(year == 2016)

font     <- list(
       family = "Helvetica, sans-serif",
       size = 20,
       color = "#1f1f2e")

axisFont <- list(
       size = 18)

xAxis    <- list(
       title = "Year",
       titlefont = axisFont)


yAxisLaborForce       <- list(
                            title = "Labor Force",
                            titlefont = axisFont)

yaxisUnemploymentRate <- list(
                            title = "Unemployment Rate",
                            titlefont = axisFont)
       

margin = list(
              l = 80,
              r = 50,
              b = 100,
              t = 50,
              pad = 1)

################################################### SHINY SERVER

shinyServer(function(input, output) {
       
  output$unemploymentLastYear <- renderDataTable({
         lastYearUnemployment

  })
  output$unemploymentThisYear <- renderPrint({
         print(thisYearUnemployment)
  })

           
  output$laborForcePlot <- renderPlotly({
         
         plot_ly(laborForceData, 
                 x = ~year, 
                 y = ~value, 
                 color = ~area,
                 type = 'scatter', 
                 mode = 'lines+markers') %>%
                layout(
                       autosize = F, 
                       width  = 700, 
                       height = 500, 
                       margin = margin, 
                       xaxis  = xAxis, 
                       yaxis  = yAxisLaborForce,
                       font   = font, 
                       legend = list(
                              x = .6, 
                              y = .6))
  })
  
  
  output$unemploymentRatePlot <-  renderPlotly({
         
         plot_ly(unemploymentRateData, 
                 x = ~year, 
                 y = ~value, 
                 color = ~area,
                 type = 'scatter', 
                 mode = 'lines+markers') %>%
                layout(
                       autosize = F, 
                       width  = 700, 
                       height = 500, 
                       margin = margin, 
                       xaxis  = xAxis, 
                       yaxis  = yaxisUnemploymentRate,
                       font   = font, 
                       legend = list(
                              x = .7, 
                              y = .95))

  })

})
