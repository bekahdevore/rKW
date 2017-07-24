# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(dplyr)
library(RCurl)
library(rvest)

allData <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vTdpRstn1btawynSl2J8MiirsN1dnuYErkGeeKVXi_0lRmmI3oIchRnRxwvzxEznlrI7j_RXQ14-win/pub?gid=0&single=true&output=csv")
allData <- read.csv(textConnection(allData), check.names = FALSE)

allData <- allData %>% select(MSA, Population, `Labor Force Participation Rate`, 
                              `Labor Force Size`, `Unemployment Rate`,
                              `Median Home Value`, `Median Household Wage`, 
                              `Median Monthly Rent`, `Annual Median Wage (USD)`)
#allData$Population <- sort(allData$Population, decreasing = TRUE)

## Scrape MIT Living Wage Data
mitLivingWageLouisvilleMSA <- read_html("http://livingwage.mit.edu/metros/31140")
mitLivingWageLouisvilleMSA <- mitLivingWageLouisvilleMSA %>% html_nodes("table") %>% 
  .[[2]] %>% 
  html_table()
mitLivingWageLouisvilleMSA <- mitLivingWageLouisvilleMSA %>% select(-10)

mitLivingWageLouisvilleMSAFamilyOfFour <- mitLivingWageLouisvilleMSA %>%
  filter(`Annual Expenses` == "Required annual income before taxes") %>% 
  select(1, 8)
colnames(mitLivingWageLouisvilleMSAFamilyOfFour)[1] <- ""

louisvilleRankings <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRTRIv6Rnx2ys6irPTE4KS7j9hC-iQORATkLm3ulVZRr_ViCcb91yPyJrZ46NwPjZhuUHIMwqfYw55r/pub?gid=0&single=true&output=csv")
louisvilleRankings <- read.csv(textConnection(louisvilleRankings), check.names = FALSE)
louisvilleRankings$Rank <- sort(louisvilleRankings$Rank) 
louisvilleRankings <- louisvilleRankings %>% select(2, 1, 3)

allRankings <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vTYM8rmhH6Uy_bHbvUtlDAvphu_MLwRH40hRMwRIwwQbtw-KUXNWkEY4zdsAy6fQfmIvF-RgHoMguKl/pub?gid=0&single=true&output=csv")
allRankings <- read.csv(textConnection(allRankings), check.names = FALSE)
allRankings$Population <- sort(allRankings$Population)

allRankings <- allRankings %>% select(MSA, Population, `Labor Force Participation Rate`,
                                      `Labor Force Size`, `Unemployment Rate`,
                                      `Median Home Value`, `Median Household Wage`,
                                      `Median Monthly Rent`, `Annual Median Wage (USD)`)

growthLouisville <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vQuiFsm-Z04pTWSaHlS7iumASPygDLm6qH9xrBRtTtOY3oVY_3XUH55eEFB-3K3JkCxPBRFAkXq42Ka/pub?gid=0&single=true&output=csv")
growthLouisville <- read.csv(textConnection(growthLouisville), check.names = FALSE)

stateUsData <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vTO9CHzSOkZoM8jiVw6tWCjNWdlJW8w4GAaFO9jK_ZKiLlI_6mTSdk6Zeoe5Q9J7doj5vLKCKvqWQ2g/pub?gid=0&single=true&output=csv")
stateUsData <- read.csv(textConnection(stateUsData), check.names = FALSE)  


shinyServer(function(input, output) {

  output$allData <- DT::renderDataTable(
          allData, options = list(lengthChange = FALSE, pageLength = 15, ordering = F, paging = FALSE, 
                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                  rowCallback = DT::JS(
            'function(row, data) {
            // Bold cells for those >= 5 in the first column
            if (data[0] === "Louisville")
            $("td", row).css("background", "#f76b6b");
          }')), 
          rownames = FALSE
  )
  
  output$louisvilleRankings <- DT::renderDataTable(
    louisvilleRankings, options = list(lengthChange = FALSE, pageLength = 15, paging = FALSE, ordering = F, autoWidth = FALSE,
                                       columnDefs = list(list(className = 'dt-center', targets = "_all" 
                                                              ))), 
    rownames = FALSE
  )
  
  output$allRankings <- DT::renderDataTable(
    allRankings, options = list(lengthChange = FALSE, pageLength = 15, paging = FALSE, autoWidth = FALSE,
                                       columnDefs = list(list(className = 'dt-center', targets = "_all" 
                                       )), 
                                rowCallback = DT::JS(
                                  'function(row, data) {
                                  // Bold cells for those >= 5 in the first column
                                  if (data[0] === "Louisville")
                                  $("td", row).css("background", "#f76b6b");
                                  }'
                          )), 
    rownames = FALSE
  )
  
  
  output$stateUsData <- DT::renderDataTable(
    stateUsData, options = list(lengthChange = FALSE, pageLength = 15, paging = FALSE, autoWidth = FALSE,
                                     columnDefs = list(list(className = 'dt-center', targets = "_all" 
                                     )), rowCallback = DT::JS(
                                       'function(row, data) {
                                       if (data[1] === "Louisville")
                                       $("td", row).css("background", "#f76b6b");
}')),  
    rownames = FALSE
  )
 
  output$growthLouisville <- DT::renderDataTable(
    growthLouisville, options = list(lengthChange = FALSE, pageLength = 15, paging = FALSE, ordering = F, autoWidth = FALSE,
                                       columnDefs = list(list(className = 'dt-center', targets = "_all" 
                                       ))), 
    rownames = FALSE
  )
  
  output$mitLivingWageTableFamilyOfFour <-  DT::renderDataTable(
    mitLivingWageLouisvilleMSAFamilyOfFour, options = list(pageLength = 3, searching = FALSE, ordering = F, paging = FALSE),
    rownames = FALSE
  )
  
  output$mitLivingWageTable <- DT::renderDataTable(
     mitLivingWageLouisvilleMSA, options = list(pageLength = 3, searching = FALSE, paging = FALSE,
                        rowCallback = DT::JS(
                          'function(row, data) {
                          // Bold cells for those >= 5 in the first column
                          if (data[1] === "Living Wage")
                          $("td:eq(8)", row).css("color", "#f76b6b");
      }')), rownames = FALSE
  )
})

